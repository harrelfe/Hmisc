##' Simulate Bayesian Sequential Treatment Comparisons Using a Gaussian Model
##'
##' Simulate a sequential trial under a Gaussian model for parameter estimates, and Gaussian priors.  Looks are done sequentially at observation numbers given in the vector `looks` with the earliest possible look being at observation 2.  For each true effect parameter value, simulation, at each look, and for each prior/assertion combination, computes the posterior probability of the assertion.  For each simulation, data are first simulated for the last look, and these data are sequentialy revealed for earlier looks.  The user provides a function `gendat` that given a true effect of `parameter` and the two sample sizes (for treatment groups 1 and 2) returns a list with vectors `y1` and `y2` containing simulated data.  The user also provides a function `fitter` with arguments `x` (group indicator 0/1) and `y` (response variable) that returns a 2-vector containing the effect estimate and its variance.  `parameter` and `cutoff` are usually on the scale of a regression coefficient, e.g., a log odds ratio.
##' @title gbayesSeqSim
##' @param parameter vector of true parameter (effects; group differences) values
##' @param looks integer vector of observation numbers at which posterior probabilities are computed
##' @param asserts list of lists.  The first element of each list is the user-specified name for each assertion/prior combination, e.g., `"efficacy"`.  The other elements are, in order, a character string equal to "<", ">", or "in", a parameter value `cutoff` (for "<" and ">") or a 2-vector specifying an interval for "in", and either a prior distribution mean and standard deviation named `mu` and `sigma` respectively, or a parameter value (`"cutprior"`) and tail area `"tailprob"`.  If the latter is used, `mu` is assumed to be zero and `sigma` is solved for such that P(parameter > 'cutprior') = P(parameter < - 'cutprior') = `tailprob`.
##' @param gendat a function of three arguments: true parameter value (scalar), sample size for first group, sample size for second group
##' @param fitter a function of two arguments: 0/1 group indicator vector and the dependent variable vector
##' @param nsim number of simulations (default is 1)
##' @return a data frame with number of rows equal to the product of `nsim`, the length of `looks`, the length of `asserts`, and the length of `parameter`.  It also has an attribute `asserts` added which is the original `asserts` augmented with any derived `mu` and `sigma` and converted to a data frame.
##' @author Frank Harrell
##' @seealso `gbayes()`
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
##' z <- gbayesSeqSim(c(0, log(0.7)), looks=c(50, 75, 95, 100, 200),
##'                   asserts=asserts, gendat=gdat,
##'                   fitter=lfit, nsim=100)
##' head(z)
##' attr(z, 'asserts')
##' 
##' # Compute the proportion of simulations that hit targets (different target posterior
##' # probabilities for efficacy vs. similarity)
##' 
##' # For each simulation and assertion/prior/parameter combination compute the
##' # first look at which the target was hit (set to infinity if never hit)
##' require(data.table)
##' # Define posterior probability target for each type of assertion/prior
##' z$target <- c(Efficacy=0.95, Similarity=0.75)[z$assert]
##' z <- data.table(z)
##' u <- z[, .(first=min(look[prob > target])), by=list(parameter, assert, sim)]
##' 
##' # For each assertion/prior and true parameter value combination compute the
##' # proportion of simulations that hit the target (1) ever or (2) by the 100th subject
##' 
##' u[, .(ever=mean(first < Inf)),  by=list(parameter, assert)]
##' u[, .(by75=mean(first <= 100)), by=list(parameter, assert)]
##' 
##' }
##' @md
gbayesSeqSim <- function(parameter,
                         looks, asserts, gendat, fitter, nsim=1) {
  nas    <- length(asserts)
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
  }

  looks  <- sort(looks)
  nlook  <- length(looks)
  N      <- max(looks)
  np     <- length(parameter)
  nc     <- nsim * nlook * nas * np
  assert <- character(nc)
  sim    <- parm <- pp <- numeric(nc)
  look   <- integer(nc)

  
  ## For each simulation and each parameter value, simulate data for the
  ## whole study

  is <- 0
  for(isim in 1 : nsim) {
    for(param in parameter) {
      X  <- sample(0 : 1, N, replace=TRUE)
      
      dat <- gendat(param, sum(X == 0), sum(X == 1))
      Y   <- rep(NA, N)
      Y[X == 0] <- dat$y1
      Y[X == 1] <- dat$y2

      ## For each look and each assertion/prior combination compute
      ## the posterior probability of that assertion

      for(l in looks) {
        f  <- fitter(X[1 : l], Y[1 : l])
        est  <- f[1]
        vest <- f[2]
        for(a in asserts) {
          dir    <- a$dir
          cutoff <- a$cutoff
          mu     <- a$mu
          sigma2  <- (a$sigma) ^ 2
          var.post  <- 1. / (1. / sigma2 + 1. / vest)
          mean.post <- (mu / sigma2 + est / vest) * var.post
          sd.post   <- sqrt(var.post)

          post <- switch(dir,
                         '<'  = pnorm(cutoff, mean.post, sd.post),
                         '>'  = 1. - pnorm(cutoff, mean.post, sd.post),
                         'in' = pnorm(cutoff[2], mean.post, sd.post) -
                                pnorm(cutoff[1], mean.post, sd.post))
          is         <- is + 1
          assert[is] <- a$label
          sim[is]    <- isim
          parm[is]   <- param
          look[is]   <- l
          pp[is]     <- post
        } # end asserts
      }  # end looks
    }  # end param
  } # end sim

  d <- data.frame(assert=assert, sim=sim, parameter=parm, look=look,
                  prob=pp)
  a <- asserts
  w <- NULL
  for(i in 1 : nas) {
    a <- asserts[[i]]
    a$dir <- a$cutoff <- NULL
    if(any(c('cutprior', 'tailprob') %nin% names(a)))
      a$cutprior <- a$tailprob <- NA
    w <- rbind(w, as.data.frame(a))
    }
  attr(d, 'asserts') <- w
  d
}
