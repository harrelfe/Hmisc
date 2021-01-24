##' Simulate Ordinal Markov Process
##'
##' Simulates longitudinal data for subjects following a first-order Markov process under a proportional odds model.
##' @title simMarkovOrd
##' @param n number of subjects to simulate
##' @param y vector of possible y values in order
##' @param times vector of measurement times
##' @param initial initial value of `y` (baseline state).  If length 1 this value is used for all subjects, otherwise it is a vector of length `n`.
##' @param X an optional vector of matrix of baseline covariate values passed to `g`.  If a vector, `X` represents a set of single values for all the covariates and those values are used for every subject.  Otherwise `X` is a matrix with rows corresponding to subjects and columns corresponding to covariates which `g` must know how to handle.  `g` only sees one row of `X` at a time.
##' @param absorb vector of absorbing states, a subset of `y`.  The default is no absorbing states.  Observations are truncated when an absorbing state is simulated.
##' @param intercepts vector of intercepts in the proportional odds model.  There must be one fewer of these than the length of `y`.
##' @param g a user-specified function of three or more arguments which in order are the value of `y` at the previous time, the current time, the gap between the previous time and the current time, an optional (usually named) covariate vector `X`, and optional arguments such as a regression coefficient value to simulate from.  The function returns the linear predictor for the proportional odds model aside from `intercepts`.
##' @param carry set to `TRUE` to carry absorbing state forward after it is first hit; the default is to end records for the subject once the absorbing state is hit
##' @param ... additional arguments to pass to `g` such as a regresson coefficient
##' @return data frame with one row per subject per time, and columns id, time, yprev, y, values in ...
##' @author Frank Harrell
##' @examples
##' g <- function(yprev, t, gap) yprev - ((gap - 1)/ 7) * yprev + t / 10
##' # The following takes 1.6s for n=100,000
##' simMarkovOrd(10, 1:3, c(1, 2, 3, 7, 14), 2, absorb=3,
##'              intercepts=c(-1, -2), g=g)
##' @seealso <https://hbiostat.org/R/Hmisc/simMarkovOrd.html>
##' @export
##' @md
simMarkovOrd <- function(n=1, y, times, initial, X=NULL, absorb=NULL,
                         intercepts, g, carry=FALSE, ...) {

  if(length(initial) == 1) initial <- rep(initial, n)
  if(length(initial) != n) stop('initial must have length 1 or n')
  if(any(initial %in% absorb))
    stop('initial state cannot be an absorbing state')

  Xmat <- length(X) && is.matrix(X)
  if(Xmat && ! length(colnames(X)))
    stop('when a matrix, X must have column names')
  if(length(X) && ! Xmat && ! length(names(X)))
    stop('when a vector, elements of X must have names')
  
  nt    <- length(times)
  Y     <- Yp <- integer(nt)
  gaps  <- numeric(nt)
  ID    <- Time <- Gap <- YYprev <- YY <- integer(n * nt)
  is    <- 1
  
  for(id in 1 : n) {
    tprev <- 0
    i     <- 0
    for(t in times) {
      i       <- i + 1
      gap     <- t - tprev
      gaps[i] <- gap
      yprev   <- if(i == 1) initial[id] else Y[i - 1]
      Yp[i]   <- yprev
      if(carry && (yprev %in% absorb)) Y[i] <- yprev
      else {
        xb    <- g(yprev, t, gap, X=if(Xmat) X[id, ] else X, ...)
        probs <- plogis(intercepts + xb)
        ## Compute cell probabilities from successive differences in
        ## exceedance probs
        probs <- c(1., probs) - c(probs, 0.)
        Y[i]  <- sample(y, 1, prob=probs)
        if(! carry && (Y[i] %in% absorb)) break
      }
      tprev <- t
    }
    ie              <- is + i - 1
    ID    [is : ie] <- id
    Time  [is : ie] <- times[1 : i]
    Gap   [is : ie] <- gaps[1 : i]
    YYprev[is : ie] <- Yp[1 : i]
    YY    [is : ie] <- Y[1 : i]
    is              <- ie + 1
  }
  res <- data.frame(id=ID[1 : ie], time=Time[1 : ie], gap=Gap[1 : ie],
                    yprev=YYprev[1 : ie], y=YY[1 : ie], ...)
  ## Handle case where X is a constant vector to distribute to all obs
  if(length(X)) {
    if(Xmat) for(nam in colnames(X)) res[[nam]] <- X[res$id, nam]
    else
      for(nam in names(X)) res[[nam]] <- X[nam]
    }
  res
}    


#' State Occupancy Probabilities for First-Order Markov Ordinal Model
#'
#' @title soprobMarkovOrd
#' @param y a vector of possible y values in order
#' @param times vector of measurement times
#' @param initial initial value of `y` (baseline state)
#' @param absorb vector of absorbing states, a subset of `y`.  The default is no absorbing states.
#' @param intercepts vector of intercepts in the proportional odds model, with length one less than the length of `y`
#' @param g a user-specified function of three or more arguments which in order are the value of `y` at the previous time, the current time, the gap between the previous time and the current time, and optional arguments `X` and others such as true regression coefficients.  The function returns the linear predictor for the proportional odds model aside from `intercepts`.  The first argument of `g` must be allowed to be a vector of all non-absorbing state `y` values.
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
#' s <- simMarkovOrd(1000, 1:3, c(1, 2, 3, 7, 14), 2, absorb=3,
#'                   intercepts=c(-1, -2), g=g, carry=TRUE)
#' relfreq <- function(x) table(x) / length(x)
#' w <- with(s, tapply(y, time, relfreq))
#' do.call(rbind, w)
#' # Compare with exact answer:
#' soprobMarkovOrd(1:3, c(1, 2, 3, 7, 14), initial=2, absorb=3,
#'                 intercepts=c(-1, -2), g=g)
#' @seealso <https://hbiostat.org/R/simMarkovOrd.html>
#' @export 
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


##' Simulate Comparisons For Use in Sequential Markov Longitudinal Clinical Trial Simulations
##'
##' Simulates sequential clinical trials of longitudinal ordinal outcomes using a first-order Markov model.  Looks are done sequentially after subject ID numbers given in the vector `looks` with the earliest possible look being after subject 2.  At each look, a subject's repeated records are either all used or all ignored depending on the sequent ID number.  For each true effect parameter value, simulation, and at each look, runs a function to compute the estimate of the parameter of interest along with its variance.  For each simulation, data are first simulated for the last look, and these data are sequentially revealed for earlier looks.  The user provides a function `g` that has extra arguments specifying the true effect of `parameter` the treatment `group` expecting treatments to be coded 1 and 2.  `parameter` is usually on the scale of a regression coefficient, e.g., a log odds ratio.  Fitting is done using the `rms` `lrm` function.  If `timecriterion` is specified, the function also, for the last data look only, computes the first time at which the criterion is satisfied for the subject.  The Cox/logrank chi-square statistic for comparing groups on the derived time variable is saved.  If `coxzph=TRUE`, the `survival` package correlation coefficient `rho` from the scaled partial residuals is also saved so that the user can later determine to what extent the Markov model resulted in the proportional hazards assumption being violated when analyzing on the time scale.
##' @title estSeqMarkovOrd
##' @param y vector of possible y values in order
##' @param times vector of measurement times
##' @param initial a vector of probabilities summing to 1.0 that specifies the frequency distribution of initial values to be sampled from.  The vector must have names that correspond to values of `y` representing non-absorbing states.
##' @param absorb vector of absorbing states, a subset of `y`.  The default is no absorbing states.  Observations are truncated when an absorbing state is simulated.
##' @param intercepts vector of intercepts in the proportional odds model.  There must be one fewer of these than the length of `y`.
##' @param g a user-specified function whose first 5 arguments in order are the value of `y` at the previous time, the current time, the gap between the previous time and the current time, `group` (1 or 2), and `parameter` (the group effect).  The function returns the linear predictor for the proportional odds model aside from `intercepts`.
##' @param parameter vector of true parameter (effects; group differences) values.  These are group 2:1 log odds ratios in the transition model, conditioning on the previous `y`.
##' @param looks integer vector of ID numbers at which maximum likelihood estimates and their estimated variances are computed.  For a single look specify a scalar value for `loops` equal to the number of subjects in the sample.
##' @param formula a formula object given to the `lrm()` function using variables with these name: `y`, `time`, `yprev`, and `group` (having values 1 and 2)
##' @param groupContrast omit this argument if `group` has only one regression coefficient in `formula`.  Otherwise provide `groupContrast` as a list of two lists that are passed to `rms::contrast.rms()` to compute the contrast of interest and its standard error.  The first list corresponds to group 2, the second to group 1, to get a 2 - 1 contrast.
##' @param timecriterion a function of a time-ordered vector of simulated ordinal responses `y` that returns a vector `FALSE` or `TRUE` values denoting whether the current `y` level met the condition of interest.  For example `estSeqMarkovOrd` will compute the first time at which `y >= 5` if you specify `timecriterion=function(y) y >= 5`.  This function is only called at the last data look for each simulated study.
##' @param coxzph set to `TRUE` if `timecriterion` is specified and you want to compute a statistic for testing proportional hazards at the last look of each simulated data
##' @param nsim number of simulations (default is 1)
##' @param progress set to `TRUE` to send current iteration number to the console
##' @return a data frame with number of rows equal to the product of `nsim`, the length of `looks`, and the length of `parameter`.
##' @author Frank Harrell
##' @seealso `gbayesSeqSim()`, `simMarkovOrd()`, <https://hbiostat.org/R/Hmisc/simMarkovOrd.html>
##' @export
##' @md

estSeqMarkovOrd <- function(y, times, initial, absorb=NULL, intercepts,
                            parameter, looks, g, formula,
                            groupContrast=NULL,
                            timecriterion=NULL, coxzph=FALSE,
                            nsim=1, progress=FALSE) {

  olddd <- getOption('datadist')
  on.exit(options(datadist=olddd))
  
  nas <- setdiff(y, absorb)    # non-absorbing states
  if(length(initial) != length(nas))
    stop('length of initial must be number of non-absorbing values of y')
  if(! all(sort(names(initial)) == sort(as.character(nas))))
    stop('names of elements in initial are incorrect')
  if(coxzph && ! length(timecriterion))
    stop('must specify timecriterion when coxzph=TRUE')
  
  looks  <- sort(looks)
  nlook  <- length(looks)
  N      <- max(looks)
  np     <- length(parameter)
  nc     <- nsim * nlook * np
  parm   <- est <- vest <- numeric(nc)
  look   <- sim <- integer(nc)
  Etimefreq <- NULL
  if(length(timecriterion)) {
    censlab <- paste0(as.character(max(times)), '+')
    Etimefreq <-
      array(0, dim=c(nsim, np, 2, length(times) + 1),
            dimnames=list(paste('sim', 1 : nsim),
                          as.character(parameter),
                          c('1', '2'),
                          c(as.character(times), censlab)))
    lrchisq <- rep(NA, nc)
    if(coxzph) phchisq <- rep(NA, nc)
    }
  
  ## For each simulation and each parameter value, simulate data for the
  ## whole study

  is    <- 0
  pname <- 'group=2'
  h <- function(time, y) {
    u <- timecriterion(y)
    if(any(u)) list(etime=min(time[u]), event=1L)
    else
               list(etime=max(times),   event=0L)
  }
  
  for(isim in 1 : nsim) {
    if(progress) cat('Simulation', isim, '\r')
    for(param in parameter) {
      ## Sample N initial states
      initials <- sample(names(initial), N, replace=TRUE, prob=initial)
      if(is.numeric(y)) initials <- as.numeric(initials)
      ## Sample treatment groups 1 and 2
      X   <- matrix(sample(1 : 2, N, replace=TRUE), ncol=1,
                    dimnames=list(NULL, 'group'))
      sdata <- simMarkovOrd(n=N, y, times, initials, X=X, absorb=absorb,
                            intercepts=intercepts, g=g, parameter=param)
      ## sdata is a data frame containing id, time, yprev, y, ...
      sdata$group <- as.factor(sdata$group)
      if(isim == 1) {
        .dd. <- rms::datadist(sdata)
        assign('.dd.', .dd., envir=.GlobalEnv)
        options(datadist='.dd.')
        }
      
      ## For each look compute the parameter estimate and its variance
      ## If a contrast is specified (say when treatment interacts with time)
      ## use that instead of a simple treatment effect

      for(l in looks) {
        dat <- subset(sdata, id <= l)
        f   <- rms::lrm(formula, data=dat)
        is         <- is + 1
        sim[is]    <- isim
        parm[is]   <- param
        look[is]   <- l
        if(length(groupContrast)) {
          fc <- rms::contrast(f, groupContrast[[2]], groupContrast[[1]])
          est[is]  <- fc$Contrast
          vest[is] <- (fc$SE) ^ 2
        }
        else {
          est[is]    <- coef(f)[pname]
          vest[is]   <- vcov(f)[pname, pname]
          }
      }  # end looks
      if(length(timecriterion)) {
        ## Separately for each subject compute the time until the
        ## criterion is satisfied.  Right censor at last time if it
        ## doesn't occur
        setDT(sdata, key=c('group', 'id', 'time'))
        d <- sdata[, h(time, y), by=.(group, id)]
        fit <- survival::coxph(Surv(etime, event) ~ group, data=d)
        lrchisq[is] <- 2. * diff(fit$loglik)
        phchisq[is] <- survival::cox.zph(fit, transform='identity',
                                         global=FALSE)$table[, 'chisq']
        for(gr in c('1', '2')) {
          utimes <- with(subset(d, group == gr),
                         ifelse(event == 1, as.character(etime), censlab))
          utimes <- factor(utimes, c(times, censlab))
          tab <- table(utimes)
          Etimefreq[isim, as.character(param), gr, ] <-
            Etimefreq[isim, as.character(param), gr, ] + tab
        }
      }
    }  # end param
  } # end sim
  
  if(progress) cat('\n')

  res <- data.frame(sim=sim, parameter=parm, look=look,
                    est=est, vest=vest)
  if(length(timecriterion)) {
    res$lrchisq <- lrchisq
    if(coxzph) res$phchisq <- phchisq
    attr(res, 'etimefreq') <- Etimefreq
    }
  res
}

#' Compute Intercepts for Proportional Odds Markov Model
#'
#' Given a vector `intercepts` of initial guesses at the intercepts in a Markov proportional odds model, solves for the intercept vector that yields a set of occupancy probabilities at time `t` that equal a vector of target values.
#' @param y see [simMarkovOrd]
#' @param times see [simMarkovOrd]
#' @param initial see [simMarkovOrd]
#' @param absorb see [simMarkovOrd]
#' @param intercepts vector of initial guesses for the intercepts
#' @param g see [simMarkovOrd]
#' @param target vector of target state occupancy probabilities at time `t`
#' @param t target time
#' @param ... optional arguments to pass to [stats::nlm()]
#'
#' @return vector of intercept values
#' @author Frank Harrell
#' @export
#' @md
#' @seealso <https://hbiostat.org/R/Hmisc/simMarkovOrd.html>
intMarkovOrd <- function(y, times, initial, absorb=NULL, intercepts, g, target, t, ...) {
  if(abs(sum(target) - 1.) > 1e-5) stop('target must sum to 1')
  if(any(diff(intercepts) > 0)) stop('initial intercepts are out of order')
  
  t <- as.character(t)
  h <- function(a) {
    # Compute state occupancy probabilities at time t for current vector of intercept values
    s <- soprobMarkovOrd(y, times, initial=initial, absorb=absorb,
                         intercepts=a, g=g, X=1)[t, ]
    # Objective function to minimize: sum of absolute differences with targets
    # with restriction that intercepts be in descending order
    sum(abs(s - target)) + 1000 * (any(diff(a) > 0))
  }

  u <- nlm(h, intercepts, ...)
  cat('Iterations:', u$iterations, '\n')
  cat('Sum of absolute errors:', u$minimum, '\n')
  ints <- u$estimate
  # if(any(diff(ints) > 0)) stop('computed intercepts are out of order')
  cat('Intercepts:', round(ints, 3), '\n')
  s1 <- soprobMarkovOrd(y, times, initial=initial, absorb=absorb,
                        intercepts=ints, g=g, X=1)
  cat('\nOccupancy probabilities for group 1:\n\n')
  print(round(s1, 3))
  # Show occupancy probabilities for group 2
  s2 <- soprobMarkovOrd(y, times, initial=initial, absorb=absorb,
                        intercepts=ints, g=g, X=2)
  cat('\nOccupancy probabilities for group 2:\n\n')
  print(round(s2, 3))

  # Compute log odds ratios at day t
  s1t <- rev(cumsum(rev(s1[t, -1])))
  s2t <- rev(cumsum(rev(s2[t, -1])))
  cat('\nLog odds ratios at time', t, 'from occupancy probabilities:' ,
      round(qlogis(s2t) - qlogis(s1t), 3), '\n')
  ints
}

utils::globalVariables(c('id', 'group'))
