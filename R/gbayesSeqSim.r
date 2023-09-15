##' Simulate Bayesian Sequential Treatment Comparisons Using a Gaussian Model
##'
##' Simulate a sequential trial under a Gaussian model for parameter estimates, and Gaussian priors using simulated estimates and variances returned by `estSeqSim`.  For each row of the data frame `est` and for each prior/assertion combination, computes the posterior probability of the assertion.
##' @title gbayesSeqSim
##' @param est data frame created by `estSeqSim()`
##' @param asserts list of lists.  The first element of each list is the user-specified name for each assertion/prior combination, e.g., `"efficacy"`.  The other elements are, in order, a character string equal to "<", ">", or "in", a parameter value `cutoff` (for "<" and ">") or a 2-vector specifying an interval for "in", and either a prior distribution mean and standard deviation named `mu` and `sigma` respectively, or a parameter value (`"cutprior"`) and tail area `"tailprob"`.  If the latter is used, `mu` is assumed to be zero and `sigma` is solved for such that P(parameter > 'cutprior') = P(parameter < - 'cutprior') = `tailprob`.
##' @return a data frame with number of rows equal to that of `est` with a number of new columns equal to the number of assertions added.  The new columns are named `p1`, `p2`, `p3`, ... (posterior probabilities), `mean1`, `mean2`, ... (posterior means), and `sd1`, `sd2`, ... (posterior standard deviations).  The returned data frame also has an attribute `asserts` added which is the original `asserts` augmented with any derived `mu` and `sigma` and converted to a data frame, and another attribute `alabels` which is a named vector used to map `p1`, `p2`, ... to the user-provided labels in `asserts`.
##' @author Frank Harrell
##' @seealso `gbayes()`, `estSeqSim()`, `simMarkovOrd()`, `estSeqMarkovOrd()`
##' @examples
##' \dontrun{
##' # Simulate Bayesian operating characteristics for an unadjusted
##' # proportional odds comparison (Wilcoxon test)
##' # For 100 simulations, 5 looks, 2 true parameter values, and
##' # 2 assertion/prior combinations, compute the posterior probability
##' # Use a low-level logistic regression call to speed up simuluations
##' # Use data.table to compute various summary measures
##' # Total simulation time: 2s
##' lfit <- function(x, y) {
##' f <- rms::lrm.fit(x, y)
##'   k <- length(coef(f))
##'   c(coef(f)[k], vcov(f)[k, k])
##' }
##' gdat <- function(beta, n1, n2) {
##'   # Cell probabilities for a 7-category ordinal outcome for the control group
##'   p <- c(2, 1, 2, 7, 8, 38, 42) / 100
##'
##'   # Compute cell probabilities for the treated group
##'   p2 <- pomodm(p=p, odds.ratio=exp(beta))
##'   y1 <- sample(1 : 7, n1, p,  replace=TRUE)
##'   y2 <- sample(1 : 7, n2, p2, replace=TRUE)
##'   list(y1=y1, y2=y2)
##' }
##' 
##' # Assertion 1: log(OR) < 0 under prior with prior mean 0.1 and sigma 1 on log OR scale
##' # Assertion 2: OR between 0.9 and 1/0.9 with prior mean 0 and sigma computed so that
##' # P(OR > 2) = 0.05
##' asserts <- list(list('Efficacy', '<', 0, mu=0.1, sigma=1),
##'                 list('Similarity', 'in', log(c(0.9, 1/0.9)),
##'                      cutprior=log(2), tailprob=0.05))
##'
##' set.seed(1)
##' est <- estSeqSim(c(0, log(0.7)), looks=c(50, 75, 95, 100, 200),
##'                    gendat=gdat,
##'                    fitter=lfit, nsim=100)
##' z <- gbayesSeqSim(est, asserts)
##' head(z)
##' attr(z, 'asserts')
##' 
##' # Compute the proportion of simulations that hit targets (different target posterior
##' # probabilities for efficacy vs. similarity)
##'
##' # For the efficacy assessment compute the first look at which the target
##' # was hit (set to infinity if never hit)
##' require(data.table)
##' z <- data.table(z)
##' u <- z[, .(first=min(p1 > 0.95)), by=.(parameter, sim)]
##' # Compute the proportion of simulations that ever hit the target and
##' # that hit it by the 100th subject
##' u[, .(ever=mean(first < Inf)),  by=.(parameter)]
##' u[, .(by75=mean(first <= 100)), by=.(parameter)]
##' }
##' @md
gbayesSeqSim <- function(est, asserts) {
  nas     <- length(asserts)
  alabels <- character(nas)
  for(i in 1 : nas) {
    a <- asserts[[i]]
    nam <- names(a)
    if(nam[1] == '') nam[1] <- 'label'
    if(nam[2] == '') nam[2] <- 'dir'
    if(nam[3] == '') nam[3] <- 'cutoff'
    names(a)  <- nam
    if((length(a$cutoff) == 2) != (a$dir == 'in'))
      stop('mismatch of direction and length of cutoff in asserts')
    if(any(c('mu', 'sigma') %nin% names(a))) {
      a$mu    <- 0
      a$sigma <- - a$cutprior / qnorm(a$tailprob)
      if(a$sigma <= 0) stop('error in specification of cutoff or tailprob')
    }
    w <- format(round(a$cutoff, 3))
    a$assertion <- paste(if(a$dir %in% c('<','>')) a$dir,
                         if(length(a$cutoff) == 2)
                           paste0('[', w[1], ',', w[2], ']')
                         else
                           w[1])
    asserts[[i]] <- a
    alabels[i]   <- a$label
  }

  N <- nrow(est)

  ests  <- est$est
  vests <- est$vest
  ## For each simulated parameter estimate and variance compute nas
  ## posterior probabilities

  for(i in 1 : nas) {
    a      <- asserts[[i]]
    dir    <- a$dir
    cutoff <- a$cutoff
    mu     <- a$mu
    sigma2  <- (a$sigma) ^ 2
    var.post  <- 1. / (1. / sigma2 + 1. / vests)
    mean.post <- (mu / sigma2 + ests / vests) * var.post
    sd.post   <- sqrt(var.post)

    pp <-
      switch(dir,
             '<'  = pnorm(cutoff, mean.post, sd.post),
             '>'  = pnorm(cutoff, mean.post, sd.post, lower.tail=FALSE),
             'in' = pnorm(cutoff[2], mean.post, sd.post) -
                    pnorm(cutoff[1], mean.post, sd.post))
    label(pp) <- a$label
    est[[paste0('p', i)]]    <- pp
    est[[paste0('mean', i)]] <- mean.post
    est[[paste0('sd', i)]]   <- sd.post
  }

  a <- asserts
  w <- NULL
  for(j in 1 : nas) {
    a <- asserts[[j]]
    a$dir <- a$cutoff <- NULL
    if(any(c('cutprior', 'tailprob') %nin% names(a)))
      a$cutprior <- a$tailprob <- NA
    w <- rbind(w, as.data.frame(a))
    }
  attr(est, 'asserts') <- w
  names(alabels) <- paste0('p', 1 : nas)
  attr(est, 'alabels') <- alabels
  est
}

##' Simulate Comparisons For Use in Sequential Clinical Trial Simulations
##'
##' Simulates sequential clinical trials.  Looks are done sequentially at observation numbers given in the vector `looks` with the earliest possible look being at observation 2.  For each true effect parameter value, simulation, and at each look, runs a function to compute the estimate of the parameter of interest along with its variance.  For each simulation, data are first simulated for the last look, and these data are sequentially revealed for earlier looks.  The user provides a function `gendat` that given a true effect of `parameter` and the two sample sizes (for treatment groups 1 and 2) returns a list with vectors `y1` and `y2` containing simulated data.  The user also provides a function `fitter` with arguments `x` (group indicator 0/1) and `y` (response variable) that returns a 2-vector containing the effect estimate and its variance.  `parameter` is usually on the scale of a regression coefficient, e.g., a log odds ratio.
##' @title estSeqSim
##' @param parameter vector of true parameter (effects; group differences) values
##' @param looks integer vector of observation numbers at which posterior probabilities are computed
##' @param gendat a function of three arguments: true parameter value (scalar), sample size for first group, sample size for second group
##' @param fitter a function of two arguments: 0/1 group indicator vector and the dependent variable vector
##' @param nsim number of simulations (default is 1)
##' @param progress set to `TRUE` to send current iteration number to the console
##' @return a data frame with number of rows equal to the product of `nsim`, the length of `looks`, and the length of `parameter`.
##' @author Frank Harrell
##' @seealso `gbayesSeqSim()`, `simMarkovOrd()`, `estSeqMarkovOrd()`
##' @examples
##' if (requireNamespace("rms", quietly = TRUE)) {
##'   # Run 100 simulations, 5 looks, 2 true parameter values
##'   # Total simulation time: 2s
##'   lfit <- function(x, y) {
##'   f <- rms::lrm.fit(x, y)
##'     k <- length(coef(f))
##'     c(coef(f)[k], vcov(f)[k, k])
##'   }
##'   gdat <- function(beta, n1, n2) {
##'     # Cell probabilities for a 7-category ordinal outcome for the control group
##'     p <- c(2, 1, 2, 7, 8, 38, 42) / 100
##'
##'     # Compute cell probabilities for the treated group
##'     p2 <- pomodm(p=p, odds.ratio=exp(beta))
##'     y1 <- sample(1 : 7, n1, p,  replace=TRUE)
##'     y2 <- sample(1 : 7, n2, p2, replace=TRUE)
##'     list(y1=y1, y2=y2)
##'   }
##'
##'   set.seed(1)
##'   est <- estSeqSim(c(0, log(0.7)), looks=c(50, 75, 95, 100, 200),
##'                     gendat=gdat,
##'                     fitter=lfit, nsim=100)
##'   head(est)
##' }
##' @md
estSeqSim <- function(parameter, looks, gendat, fitter, nsim=1,
                      progress=FALSE) {

  looks  <- sort(looks)
  nlook  <- length(looks)
  N      <- max(looks)
  np     <- length(parameter)
  nc     <- nsim * nlook * np
  parm   <- est <- vest <- numeric(nc)
  look   <- sim <- integer(nc)
  
  ## For each simulation and each parameter value, simulate data for the
  ## whole study

  is <- 0
  for(isim in 1 : nsim) {
    if(progress) cat('Simulation', isim, '\r')
    for(param in parameter) {
      X   <- sample(0 : 1, N, replace=TRUE)
      dat <- gendat(param, sum(X == 0), sum(X == 1))
      Y   <- rep(NA, N)
      Y[X == 0] <- dat$y1
      Y[X == 1] <- dat$y2

      ## For each look compute the parameter estimate and its variance

      for(l in looks) {
        f  <- fitter(X[1 : l], Y[1 : l])
        is         <- is + 1
        sim[is]    <- isim
        parm[is]   <- param
        look[is]   <- l
        est[is]    <- f[1]
        vest[is]   <- f[2]
      }  # end looks
    }  # end param
  } # end sim
  
  if(progress) cat('\n')

  data.frame(sim=sim, parameter=parm, look=look,
             est=est, vest=vest)
}
