##' Simulate Ordinal Markov Process
##'
##' Simulates longitudinal data for subjects following a first-order Markov process under a proportional odds model.  Optionally, response-dependent sampling can be done, e.g., if a subject hits a specified state at time t, measurements are removed for times t+1, t+3, t+5, ...  This is applicable when for example a study of hospitalized patients samples every day, Y=1 denotes patient discharge to home, and sampling is less frequent outside the hospital.  This example assumes that arriving home is not an absorbing state, i.e., a patient could return to the hospital.
##' @title simMarkovOrd
##' @param n number of subjects to simulate
##' @param y vector of possible y values in order (numeric, character, factor)
##' @param times vector of measurement times
##' @param initial initial value of `y` (baseline state; numeric, character, or factor matching `y`).  If length 1 this value is used for all subjects, otherwise it is a vector of length `n`.
##' @param X an optional vector of matrix of baseline covariate values passed to `g`.  If a vector, `X` represents a set of single values for all the covariates and those values are used for every subject.  Otherwise `X` is a matrix with rows corresponding to subjects and columns corresponding to covariates which `g` must know how to handle.  `g` only sees one row of `X` at a time.
##' @param absorb vector of absorbing states, a subset of `y` (numeric, character, or factor matching `y`).  The default is no absorbing states.  Observations are truncated when an absorbing state is simulated.
##' @param intercepts vector of intercepts in the proportional odds model.  There must be one fewer of these than the length of `y`.
##' @param g a user-specified function of three or more arguments which in order are `yprev` - the value of `y` at the previous time, the current time `t`, the `gap` between the previous time and the current time, an optional (usually named) covariate vector `X`, and optional arguments such as a regression coefficient value to simulate from.  The function needs to allow `yprev` to be a vector and `yprev` must not include any absorbing states.  The `g` function returns the linear predictor for the proportional odds model aside from `intercepts`.  The returned value must be a matrix with row names taken from `yprev`.  If the model is a proportional odds model, the returned value must be one column.  If it is a partial proportional odds model, the value must have one column for each distinct value of the response variable Y after the first one, with the levels of Y used as optional column names.  So columns correspond to `intercepts`. The different columns are used for `y`-specific contributions to the linear predictor (aside from `intercepts`) for a partial or constrained partial proportional odds model.  Parameters for partial proportional odds effects may be included in the ... arguments.
##' @param carry set to `TRUE` to carry absorbing state forward after it is first hit; the default is to end records for the subject once the absorbing state is hit
##' @param rdsample an optional function to do response-dependent sampling.  It is a function of these arguments, which are vectors that stop at any absorbing state: `times` (ascending measurement times for one subject), `y` (vector of ordinal outcomes at these times for one subject.  The function returns `NULL` if no observations are to be dropped, returns the vector of new times to sample.
##' @param ... additional arguments to pass to `g` such as a regresson coefficient
##' @return data frame with one row per subject per time, and columns id, time, yprev, y, values in ...
##' @author Frank Harrell
##' @seealso <https://hbiostat.org/R/Hmisc/markov/>
##' @export
##' @md
simMarkovOrd <- function(n=1, y, times, initial, X=NULL, absorb=NULL,
                         intercepts, g, carry=FALSE, rdsample=NULL, ...) {

  if(is.factor(y))       y       <- as.character(y)
  if(is.factor(initial)) initial <- as.character(initial)
  if(is.factor(absorb))  absorb  <- as.character(absorb)
  ychar <- is.character(y)

  if(length(initial) == 1) initial <- rep(initial, n)
  if(length(initial) != n) stop('initial must have length 1 or n')
  if(any(initial %in% absorb))
    stop('initial state cannot be an absorbing state')

  Xmat <- length(X) && is.matrix(X)
  if(Xmat && ! length(colnames(X)))
    stop('when a matrix, X must have column names')
  if(length(X) && ! Xmat && ! length(names(X)))
    stop('when a vector, elements of X must have names')
  
  nt     <- length(times)
  Y      <- Yp <- if(ychar) character(nt) else numeric(nt)
  gaps   <- numeric(nt)
  ID     <- Time <- Gap <- numeric(n * nt)
  YYprev <- YY <- if(ychar) character(n * nt) else numeric(n * nt)
  is     <- 1
  times.saved <- 0
  
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
        ## If partial PO model xb has 1 row (since yprev is scalar) and
        ## columns corresponding to intercepts.  If PO, is 1x1
        probs <- plogis(intercepts + xb)
        ## Compute cell probabilities from successive differences in
        ## exceedance probs
        probs <- c(1., probs) - c(probs, 0.)
        lo <- probs < 0.
        hi <- probs > 1.
        ## The following is needed for partial proportional odds models
        if(any(c(lo , hi))) {
          warning(paste('Probabilities < 0 or > 1 at time t=', t,
                        'id=', id,
                        ':', paste(probs[c(lo, hi)], collapse=' '),
                        'set to 0 or 1'))
          if(any(lo)) probs[lo] <- 0.
          if(any(hi)) probs[hi] <- 1.
          }
        Y[i]  <- sample(y, 1, prob=probs)
        if(! carry && (Y[i] %in% absorb)) break
      }
      tprev <- t
    }
    s     <- 1 : i
    atimes <- times[s]
    agaps  <- gaps[s]
    aYp    <- Yp[s]
    aY     <- Y[s]

    if(length(rdsample)) {
      stimes <- rdsample(atimes, aY)
      lt     <- length(stimes)
      if(lt) {
        times.saved <- times.saved + i - lt
        tsprev <- c(0, stimes[- lt])
        agaps   <- stimes - tsprev
        aY      <- aY[times %in% stimes]
        if(length(aY) != lt) stop('program logic error in simMarkovOrd')
        aYp     <- c(aYp[1], aY[- lt])
        atimes  <- stimes
      }
    }

    ie              <- is + length(aY) - 1
    ID    [is : ie] <- id
    Time  [is : ie] <- atimes
    Gap   [is : ie] <- agaps
    YYprev[is : ie] <- aYp
    YY    [is : ie] <- aY
    is              <- ie + 1
  }

  yy <- YY[1 : ie]
  if(ychar) yy <- factor(yy, levels=y)
  yyp <- YYprev[1 : ie]
  if(ychar) yyp <- factor(yyp, levels=setdiff(y, absorb))

  res <- data.frame(id=ID[1 : ie], time=Time[1 : ie], gap=Gap[1 : ie],
                    yprev=yyp, y=yy, ...)
  attr(res, 'times.saved.per.subject') <- times.saved / n
    
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
#' @inheritParams simMarkovOrd
#' @param y a vector of possible y values in order (numeric, character, factor)
#' @param times vector of measurement times
#' @param initial initial value of `y` (baseline state; numeric, character, factr)
#' @param absorb vector of absorbing states, a subset of `y`.  The default is no absorbing states. (numeric, character, factor)
#' @param intercepts vector of intercepts in the proportional odds model, with length one less than the length of `y`
#' @param ... additional arguments to pass to `g` such as covariate settings
#'
#' @return matrix with rows corresponding to times and columns corresponding to states, with values equal to exact state occupancy probabilities
#' @export
#' @author Frank Harrell
#' @seealso <https://hbiostat.org/R/Hmisc/markov/>
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
  ## Since initial is scalar, xb has one row.  It has multiple columns if
  ## model is partial PO model, with columns exactly corresponding to intercepts
  pp <- plogis(intercepts + xb)
  ## Compute cell probabilities
  pp <- c(1., pp) - c(pp, 0.)
  P[1, ] <- pp

  tprev <- times[1]
  for(it in 2 : nt) {
    t <- times[it]
    gap <- t - tprev
    ## Compute linear predictor at all non-absorbing states
    xb <- g(yna, t, gap, ...)   #non-intercept part of x * beta
    ## g puts non-absorbing states as row names (= ynac)
    ## If partial PO model xb has > 1 column that correspond to intercepts

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
        pp <- plogis(intercepts + xb[yvalc, ])
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
##' Simulates sequential clinical trials of longitudinal ordinal outcomes using a first-order Markov model.  Looks are done sequentially after subject ID numbers given in the vector `looks` with the earliest possible look being after subject 2.  At each look, a subject's repeated records are either all used or all ignored depending on the sequent ID number.  For each true effect parameter value, simulation, and at each look, runs a function to compute the estimate of the parameter of interest along with its variance.  For each simulation, data are first simulated for the last look, and these data are sequentially revealed for earlier looks.  The user provides a function `g` that has extra arguments specifying the true effect of `parameter` the treatment `group` expecting treatments to be coded 1 and 2.  `parameter` is usually on the scale of a regression coefficient, e.g., a log odds ratio.  Fitting is done using the `rms::lrm()` function, unless non-proportional odds is allowed in which case `VGAM::vgam()` is used.  If `timecriterion` is specified, the function also, for the last data look only, computes the first time at which the criterion is satisfied for the subject.  The Cox/logrank chi-square statistic for comparing groups on the derived time variable is saved.  If `coxzph=TRUE`, the `survival` package correlation coefficient `rho` from the scaled partial residuals is also saved so that the user can later determine to what extent the Markov model resulted in the proportional hazards assumption being violated when analyzing on the time scale.
##' @title estSeqMarkovOrd
##' @inheritParams simMarkovOrd
##' @param y vector of possible y values in order (numeric, character, factor)
##' @param times vector of measurement times
##' @param initial a vector of probabilities summing to 1.0 that specifies the frequency distribution of initial values to be sampled from.  The vector must have names that correspond to values of `y` representing non-absorbing states.
##' @param absorb vector of absorbing states, a subset of `y`.  The default is no absorbing states.  Observations are truncated when an absorbing state is simulated.  May be numeric, character, or factor.
##' @param intercepts vector of intercepts in the proportional odds model.  There must be one fewer of these than the length of `y`.
##' @param parameter vector of true parameter (effects; group differences) values.  These are group 2:1 log odds ratios in the transition model, conditioning on the previous `y`.
##' @param looks integer vector of ID numbers at which maximum likelihood estimates and their estimated variances are computed.  For a single look specify a scalar value for `loops` equal to the number of subjects in the sample.
##' @param formula a formula object given to the `lrm()` function using variables with these name: `y`, `time`, `yprev`, and `group` (factor variable having values '1' and '2').  The `yprev` variable is converted to a factor before fitting the model unless `yprevfactor=FALSE`.
##' @param ppo a formula specifying the part of `formula` for which proportional odds is not to be assumed, i.e., that specifies a partial proportional odds model.  Specifying `ppo` triggers the use of `VGAM::vgam()` instead of `rms::lrm` and will make the simulations run slower.
##' @param yprevfactor see `formula`
##' @param groupContrast omit this argument if `group` has only one regression coefficient in `formula`.  Otherwise if `ppo` is omitted, provide `groupContrast` as a list of two lists that are passed to `rms::contrast.rms()` to compute the contrast of interest and its standard error.  The first list corresponds to group 1, the second to group 2, to get a 2:1 contrast.  If `ppo` is given and the group effect is not just a simple regression coefficient, specify as `groupContrast` a function of a `vgam` fit that computes the contrast of interest and its standard error and returns a list with elements named `Contrast` and `SE`.
##' @param cscov applies if `ppo` is not used.  Set to `TRUE` to use the cluster sandwich covariance estimator of the variance of the group comparison.
##' @param timecriterion a function of a time-ordered vector of simulated ordinal responses `y` that returns a vector `FALSE` or `TRUE` values denoting whether the current `y` level met the condition of interest.  For example `estSeqMarkovOrd` will compute the first time at which `y >= 5` if you specify `timecriterion=function(y) y >= 5`.  This function is only called at the last data look for each simulated study.
##' @param sstat set to a function of the time vector and the corresponding vector of ordinal responses for a single group if you want to compute a Wilcoxon test on a derived quantity such as the number of days in a given state.  
##' @param coxzph set to `TRUE` if `timecriterion` is specified and you want to compute a statistic for testing proportional hazards at the last look of each simulated data
##' @param nsim number of simulations (default is 1)
##' @param progress set to `TRUE` to send current iteration number to `pfile` every 10 iterations.  Each iteration will really involve multiple simulations, if `parameter` has length greater than 1.
##' @param pfile file to which to write progress information.  Defaults to `''` which is the console.  Ignored if `progress=FALSE`.
##' @return a data frame with number of rows equal to the product of `nsim`, the length of `looks`, and the length of `parameter`, with variables `sim`, `parameter`, `look`, `est` (log odds ratio for group), and `vest` (the variance of the latter).  If `timecriterion` is specified the data frame also contains `loghr` (Cox log hazard ratio for group), `lrchisq` (chi-square from Cox test for group), and if `coxph=TRUE`, `phchisq`, the chi-square for testing proportional hazards.  The attribute `etimefreq` is also present if `timecriterion=TRUE` and it probvides the frequency distribution of derived event times by group and censoring/event indicator.  If `sstat` is given, the attribute `sstat` is also present, and it contains an array with dimensions corresponding to simulations, parameter values within simulations, `id`, and a two-column subarray with columns `group` and `y`, the latter being the summary measure computed by the `sstat` function.  The returned data frame also has attribute `lrmcoef` which is the average of all the last-look logistic regression coefficient estimates over the `nsim` simulations.
##' @author Frank Harrell
##' @seealso `gbayesSeqSim()`, `simMarkovOrd()`, <https://hbiostat.org/R/Hmisc/markov/>
##' @export
##' @md

estSeqMarkovOrd <- function(y, times, initial, absorb=NULL, intercepts,
                            parameter, looks, g, formula, ppo=NULL,
                            yprevfactor=TRUE,
                            groupContrast=NULL, cscov=FALSE,
                            timecriterion=NULL, coxzph=FALSE,
                            sstat=NULL, rdsample=NULL,
                            nsim=1, progress=FALSE, pfile='') {

  olddd <- getOption('datadist')
  on.exit(options(datadist=olddd))

  isppo <- length(ppo) > 0
  if(isppo) {
    if(! inherits(ppo, 'formula')) stop('ppo must be a formula')
    if(! requireNamespace('VGAM'))
      stop('ppo specified and VGAM package not available')
    # vgam wants you to declare FALSE to indicate non-PO
    ppo <- formula(paste('FALSE ~', as.character(ppo)[-1]))
    }

  if(isppo && cscov) stop('may not specify cscov=TRUE with ppo')
  
  nas <- setdiff(y, absorb)    # non-absorbing states
  if(length(initial) != length(nas))
    stop('length of initial must be number of non-absorbing values of y')
  if(! all(sort(names(initial)) == sort(as.character(nas))))
    stop('names of elements in initial are incorrect')
  if(coxzph && ! length(timecriterion))
    stop('must specify timecriterion when coxzph=TRUE')
  
  looks   <- sort(looks)
  nlook   <- length(looks)
  N       <- max(looks)
  np      <- length(parameter)
  nc      <- nsim * nlook * np
  parm    <- est <- vest <- numeric(nc)
  look    <- sim <- integer(nc)
  
  Etimefreq <- NULL
  if(length(timecriterion)) {
    Etimefreq <-
      array(0, dim=c(nsim, np, 2, 2, length(times)),
            dimnames=list(paste('sim', 1 : nsim),
                          as.character(parameter),
                          c('1', '2'),
                          c('censored', 'event'),
                          as.character(times)))
    loghr <-   lrchisq <- rep(NA, nc) 
    if(coxzph) phchisq <- rep(NA, nc)
  }
  if(length(sstat))
    Sstat <- array(0L, dim=c(nsim, np, N, 2),
                   dimnames=list(paste('sim', 1 : nsim),
                                 as.character(parameter),
                                 paste('id', 1 : N),
                                 c('group', 'y')))

  ## For each simulation and each parameter value, simulate data for the
  ## whole study

  is    <- 0
  pname <- if(isppo) 'group2' else 'group=2'
  h <- function(time, y) {
    u <- timecriterion(y)
    # Note that if there are any absorbing events, the time vector
    # would already have been truncated at the first of such events
    if(any(u)) list(etime=as.numeric(min(time[u])), event=1L)
    else
               list(etime=as.numeric(max(time)),   event=0L)
  }

  lrmcoef <- NULL
  
  for(isim in 1 : nsim) {
    if(progress && (isim %% 10 == 0))
      cat('Simulation', isim, '\n', file=pfile)
    for(param in parameter) {
      ## Sample N initial states
      initials <- sample(names(initial), N, replace=TRUE, prob=initial)
      if(is.numeric(y)) initials <- as.numeric(initials)
      ## Sample treatment groups 1 and 2
      X   <- matrix(sample(1 : 2, N, replace=TRUE), ncol=1,
                    dimnames=list(NULL, 'group'))
      ## For simMarkovOrd X must be a matrix if it varies
      sdata <- simMarkovOrd(n=N, y, times, initials, X=X, absorb=absorb,
                            intercepts=intercepts, g=g, parameter=param,
                            rdsample=rdsample)
      
      tsps <- attr(sdata, 'time.saved.per.subject')
      if(length(tsps))
        cat('Average number of measurement times saved per subject by response-dependent sampling:', round(tsps, 1), '\n')
      ## sdata is a data frame containing id, time, yprev, y, ...
      sdata$group <- as.factor(sdata$group)
      if(yprevfactor) sdata$yprev <- as.factor(sdata$yprev)
      if(isim == 1 && ! isppo) {
        .dd. <- rms::datadist(sdata)
        options(datadist=.dd.)   # requires rms 6.1-1
      }
      
      ## For each look compute the parameter estimate and its variance
      ## If a contrast is specified (say when treatment interacts with time)
      ## use that instead of a simple treatment effect
      
      for(l in looks) {
        dat <- subset(sdata, id <= l)
        
        f   <- if(isppo)
                 VGAM::vgam(formula,
                            VGAM::cumulative(parallel = ppo, reverse=TRUE),
                            data=dat)
               else
                 rms::lrm(formula, data=dat, x=cscov, y=cscov)

        if(cscov) f <- rms::robcov(f, dat$id)
        is         <- is + 1
        sim [is]   <- isim
        parm[is]   <- param
        look[is]   <- l
        if(length(groupContrast)) {
          fc <- if(isppo) groupContrast(f)
                else
                  rms::contrast(f, groupContrast[[2]], groupContrast[[1]])
          est [is] <- fc$Contrast
          vest[is] <- (fc$SE) ^ 2
        }
        else {
          est [is]   <- coef(f)[pname]
          vest[is]   <- vcov(f)[pname, pname]
        }
      }  # end looks
      co <- coef(f)
      if(! length(lrmcoef))
        lrmcoef <- array(0., dim=c(length(parameter), nsim, length(co)),
                         dimnames=list(as.character(parameter),
                                       paste('sim', 1 : nsim),
                                       names(co)))
      lrmcoef[as.character(param), isim, ] <- co
      
      if(length(timecriterion)) {
        # Separately for each subject compute the time until the
        # criterion is satisfied.  Right censor at last observed time if it
        # doesn't occur
        setDT(sdata, key=c('group', 'id', 'time'))
        d <- sdata[, h(time, y), by=.(group, id)]
        
        fit <- survival::coxph(Surv(etime, event) ~ group, data=d)
        loghr  [is] <- fit$coef
        lrchisq[is] <- 2. * diff(fit$loglik)
        if(coxzph)
          phchisq[is] <- survival::cox.zph(fit, transform='identity',
                                           global=FALSE)$table[, 'chisq']

        for(gr in c('1', '2')) {
          for(ev in 0 : 1) {
            utimes <- with(subset(d, group == gr & event == ev),
                           as.character(etime))
            utimes <- factor(utimes, as.character(times))
            tab    <- table(utimes)
            Etimefreq[isim, as.character(param), gr, ev + 1, ] <-
              Etimefreq[isim, as.character(param), gr, ev + 1, ] + tab
          } # end censored vs event
        } # end group
      } # end timecriterion
      if(length(sstat)) {
        ## Separately for each subject compute the summary statistic
        sds <- sdata[, ys := sstat(time, y), by=.(group, id)]
        Sstat[isim, as.character(param), sds$id, ] <-
          cbind(sds$group, sds$ys)
      }  # end sstat
    }  # end param
  } # end sim
  
  res <- data.frame(sim=sim, parameter=parm, look=look,
                    est=est, vest=vest)
  if(length(timecriterion)) {
    res$loghr   <- loghr
    res$lrchisq <- lrchisq
    if(coxzph) res$phchisq <- phchisq
    attr(res, 'etimefreq') <- Etimefreq
  }
  if(length(sstat)) attr(res, 'sstat') <- Sstat
  attr(res, 'lrmcoef') <- lrmcoef
  res
}

#' Compute Parameters for Proportional Odds Markov Model
#'
#' Given a vector `intercepts` of initial guesses at the intercepts in a Markov proportional odds model, and a vector `extra` if there are other parameters, solves for the `intercepts` and `extra` vectors that yields a set of occupancy probabilities at time `t` that equal, as closely as possible, a vector of target values.
#' @inheritParams simMarkovOrd
#' @param intercepts vector of initial guesses for the intercepts
#' @param extra an optional vector of intial guesses for other parameters passed to `g` such as regression coefficients for previous states and for general time trends.  Name the elements of `extra` for more informative output.
#' @param target vector of target state occupancy probabilities at time `t`.  If `extra` is specified, `target` must be a matrix where row names are character versions of `t` and columns represent occupancy probabilities corresponding to values of `y` at the time given in the row.
#' @param t target times.  Can have more than one element only if `extra` is given.
#' @param ftarget an optional function defining constraints that relate to transition probabilities.  The function returns a penalty which is a sum of absolute differences in probabilities from target probabilities over possibly multiple targets.  The `ftarget` function must have two arguments: `intercepts` and `extra`.
#' @param onlycrit set to `TRUE` to only return the achieved objective criterion and not print anything
#' @param constraints a function of two arguments: the vector of current intercept values and the vector of `extra` parameters, returning `TRUE` if that vector meets the constrains and `FALSE` otherwise
#' @param printsop set to `TRUE` to print solved-for state occupancy probabilities for groups 1 and 2
#' @param ... optional arguments to pass to [stats::nlm()].  If this is specified, the arguments that `intMarkovOrd` normally sends to `nlm` are not used.
#'
#' @return list containing two vectors named `intercepts` and `extra` unless `oncrit=TRUE` in which case the best achieved sum of absolute errors is returned
#' @author Frank Harrell
#' @export
#' @md
#' @seealso <https://hbiostat.org/R/Hmisc/markov/>
intMarkovOrd <- function(y, times, initial, absorb=NULL,
                         intercepts, extra=NULL, g, target, t, ftarget=NULL,
                         onlycrit=FALSE, constraints=NULL,
                         printsop=FALSE, ...) {

  if(any(diff(intercepts) > 0)) stop('initial intercepts are out of order')
  
  t <- as.character(t)
  if(length(t) > 1 && (! is.matrix(target) || nrow(target) != length(t)))
    stop('target must be a matrix with # rows = length of t')
  if(length(t) == 1) target <- matrix(target, nrow=1, dimnames=list(t, NULL))
  for(ti in t)
    if(abs(sum(target[ti, ]) - 1.) > 1e-5)
      stop('each row of target must sum to 1')
  
  h <- function(a) {
    ## Compute state occupancy probabilities at time t for current
    ## vector of intercept values and extra
    ints <- a[1 : (length(a) - length(extra))]
    if(any(diff(ints) > 0.)) return(1000.)
    if(length(extra)) extra <- a[-(1 : length(ints))]
    if(length(constraints) && ! constraints(ints, extra)) return(1000.)
    s <- soprobMarkovOrd(y, times, initial=initial, absorb=absorb,
                         intercepts=ints, g=g, X=1, extra=extra)[t,, drop=FALSE ]
    # Objective function to minimize: sum of absolute differences with targets
    # with restriction that intercepts be in descending order
    crit <- 0.  # if(any(diff(ints) > 0.)) 1000. else 0.
    for(tim in rownames(s)) crit <- crit + sum(abs(s[tim, ] - target[tim, ]))
    if(length(ftarget)) crit <- crit + ftarget(intercepts=ints, extra=extra)
    crit
  }

  if(length(list(...)))
    u <- nlm(h, c(intercepts, extra), ...)
  else
    u <- nlm(h, c(intercepts, extra), iterlim=300)

  if(onlycrit) return(u$minimum)
  
  cat('\nIterations:', u$iterations, '\n')
  cat('Sum of absolute errors:', u$minimum, '\n')
  ints <- u$estimate[1 : (length(u$estimate) - length(extra))]
  if(length(extra)) extra <- structure(u$estimate[-(1 : length(ints))],
                                       names=names(extra))
  
  cat('Intercepts:', round(ints, 3), '\n')
  if(length(extra)) {
    cat('\nExtra parameters:\n\n')
    print(round(extra, 4))
    }
  s1 <- soprobMarkovOrd(y, times, initial=initial, absorb=absorb,
                        intercepts=ints, g=g, X=1, extra=extra)
  if(printsop) {
    cat('\nOccupancy probabilities for group 1:\n\n')
    print(round(s1, 3))
    }
  # Show occupancy probabilities for group 2
  s2 <- soprobMarkovOrd(y, times, initial=initial, absorb=absorb,
                        intercepts=ints, g=g, X=2, extra=extra)
  if(printsop) {
    cat('\nOccupancy probabilities for group 2:\n\n')
    print(round(s2, 3))
    }

  ## Compute log odds ratios at day t

  for(ti in t) {
    ## Get cumulative probabilities from right to left except for the first    
    s1t <- rev(cumsum(rev(s1[ti, -1])))
    s2t <- rev(cumsum(rev(s2[ti, -1])))
    lor <- round(qlogis(s2t) - qlogis(s1t), 3)
    cat('\nLog odds ratios at', paste0('t=', ti),
        'from occupancy probabilities:' , lor, '\n')
    }
  list(intercepts=ints, extra=extra)
}

utils::globalVariables(c('id', 'group', 'event', ':=', 'ys'))
