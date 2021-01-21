##' Simulate Ordinal Markov Process
##'
##' Simulates longitudinal data for one subject following a first-order Markov process under a proportional odds model.
##' @title simMarkovOrd
##' @param y vector of possible y values in order
##' @param times vector of measurement times
##' @param initial initial value of `y` (baseline state)
##' @param absorb vector of absorbing states, a subset of `y`.  The default is no absorbing states.  Observations are truncated when an absorbing state is simulated.
##' @param intercepts vector of intercepts in the proportional odds model.  There must be one fewer of these than the length of `y`.
##' @param g a user-specified function of three or more arguments which in order are the value of `y` at the previous time, the current time, the gap between the previous time and the current time, and optional arguments such as treatment assignment (scalar).  The function returns the linear predictor for the proportional odds model aside from `intercepts`.
##' @param id if you desire a subject ID value to appear as a column of constants in the resulting matrix, specify an integer scalar as `id`
##' @param carry set to `TRUE` to carry absorbing state forward after it is first hit; the default is to end records for the subject once the absorbing state is hit
##' @param ... additional arguments to pass to `g` such as covariate settings
##' @return matrix with rows = times, columns time and y
##' @author Frank Harrell
##' @examples
##' g <- function(yprev, t, gap) yprev - ((gap - 1)/ 7) * yprev + t / 10
##' simMarkovOrd(1:3, c(1, 2, 3, 7, 14), 2, absorb=3,
##'              intercepts=c(-1, -2), g=g)
##'
##' # Simulate 100 subjects (for 100,000 subjects this takes 2s)
##' s <- sapply(1 : 100, function(id)
##'       simMarkovOrd(1:3, c(1, 2, 3, 7, 14), 2, absorb=3,
##'                    intercepts=c(-1, -2), g=g, id=id) )
##' do.call(rbind, s)   # create one matrix out of 100 matrices having varying # rows
##' @export
##' @md
simMarkovOrd <- function(y, times, initial, absorb=NULL,
                         intercepts, g, id=NULL, carry=FALSE, ...) {
  if(length(initial) != 1) stop('initial must have length 1')
  if(initial %in% absorb) stop('initial state cannot be an absorbing state')
  
  nt    <- length(times)
  Y     <- integer(nt)
  tprev <- 0
  i     <- 0

  for(t in times) {
    i     <- i + 1
    gap   <- t - tprev
    yprev <- if(i == 1) initial else Y[i - 1]
    if(carry && (yprev %in% absorb)) Y[i] <- yprev
    else {
      xb    <- g(yprev, t, gap, ...)
      probs <- plogis(intercepts + xb)
      ## Compute cell probabilities from successive differences in
      ## exceedance probs
      probs <- c(1., probs) - c(probs, 0.)
      Y[i]  <- sample(y, 1, prob=probs)
      if(! carry && (Y[i] %in% absorb)) break
    }
    tprev <- t
  }
  cbind(time=times[1 : i], y=Y[1 : i], id=id)
}


#' State Occupancy Probabilities for First-Order Markov Ordinal Model
#'
#' @title soprobMarkovOrd
#' @param y a vector of possible y values in order
#' @param times vector of measurement times
#' @param initial initial value of `y` (baseline state)
#' @param absorb vector of absorbing states, a subset of `y`.  The default is no absorbing states.
#' @param intercepts vector of intercepts in the proportional odds model, with length one less than the length of `y`
#' @param g a user-specified function of three or more arguments which in order are the value of `y` at the previous time, the current time, the gap between the previous time and the current time, and optional arguments such as treatment assignment (scalar).  The function returns the linear predictor for the proportional odds model aside from `intercepts`.  The first argument of `g` must be allowed to be a vector of all non-absorbing state `y` values.
#' @param ... additional arguments to pass to `g` such as covariate settings
#'
#' @return matrix with rows corresponding to times and columns corresponding to states, with values equal to exact state occupancy probabilities
#' @export
#' @author Frank Harrell
#' @examples
#' # Simulate a sample of 1000 subjects and compare proportions in
#' # states with exact state occupancy probabilities.  Carry absorbing
#' # states forward just to simplify calculation of proportions
#' g <- function(yprev, t, gap) yprev - ((gap - 1)/ 7) * yprev + t / 10
#' s <- sapply(1 : 1000, function(id)
#'        simMarkovOrd(1:3, c(1, 2, 3, 7, 14), 2, absorb=3,
#'                     intercepts=c(-1, -2), g=g, id=id, carry=TRUE), simplify=FALSE)
#' s <- do.call(rbind, s)
#' relfreq <- function(x) table(x) / length(x)
#' do.call(rbind, tapply(s[, 'y'], s[, 'time'], relfreq))
#' soprobMarkovOrd(1:3, c(1, 2, 3, 7, 14), initial=2, absorb=3,
#'                 intercepts=c(-1, -2), g=g)
#' @md
soprobMarkovOrd <- function(y, times, initial, absorb=NULL,
                            intercepts, g, ...) {

  if(initial %in% absorb) stop('initial state cannot be an absorbing state')
  k  <- length(y)
  nt <- length(times)
  P  <- matrix(NA, nrow=nt, ncol=k)
  colnames(P) <- as.character(y)
  rownames(P) <- as.character(times)
  yna         <- setdiff(y, absorb)   # all states except absorbing ones
  yc          <- as.character(y)
  ynac        <- as.character(yna)

  ## Don't uncondition on initial state
  xb <- g(initial, times[1], times[1], ...)  # 3rd arg (gap) assumes time origin 0
  pp <- plogis(intercepts + xb)   # xb is scalar since initial is scalar
  ## Compute cell probabilities
  pp <- c(1., pp) - c(pp, 0.)
  P[1, ] <- pp

  tprev <- times[1]
  for(it in 2 : nt) {
    t <- times[it]
    gap <- t - tprev
    ## Compute linear predictor at all non-absorbing states
    xb <- g(yna, t, gap, ...)   #non-intercept part of x * beta
    names(xb) <- ynac

    ## Matrix of conditional probabilities of Y conditioning on previous Y
    ## Columns = k conditional probabilities conditional on a single previous state
    ## Rows    = all possible previous states
    ## When the row corresponds to an absorbing state, with prob. 1
    ## a subject will remain in that state so give it a prob of 1 and
    ## all other states a prob of 0

    cp <- matrix(NA, nrow=k, ncol=k, dimnames=list(yc, yc))
    for(yval in y) {   # current row
      yvalc <- as.character(yval)
      if(yval %in% absorb) {   # current row is an absorbing state
        cp[yvalc, setdiff(yc, yvalc)]    <- 0.  # P(moving to non-abs state)=0
        cp[yvalc, yvalc]                 <- 1.  # certainty in staying
      }
      else {  # current row is non-absorbing state
        pp <- plogis(intercepts + xb[yvalc])
        ## Compute cell probabilities
        pp <- c(1., pp) - c(pp, 0.)
        cp[yvalc, ] <- pp
      }
    }
    P[it, ] <- t(cp) %*% P[it - 1, ]
    tprev <- t
  }
  P
}
