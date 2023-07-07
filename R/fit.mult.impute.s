fit.mult.impute <- function(formula, fitter, xtrans, data,
                            n.impute=xtrans$n.impute, fit.reps=FALSE,
                            dtrans, derived, fun,
                            vcovOpts=NULL,
                            robust=FALSE, cluster,
                            robmethod=c('huber', 'efron'),
                            method=c('ordinary', 'stack', 'only stack'),
                            funstack=TRUE, lrt=FALSE,
                            pr=TRUE, subset, fitargs)
{
  call      <- match.call()
  robmethod <- match.arg(robmethod)
  method    <- match.arg(method)

  if(lrt) {
    if(missing(fitargs)) fitargs <- list(x=TRUE, y=TRUE)
    else {fitargs$x <- fitargs$y <- TRUE}
    method  <- 'stack'
    fun     <- function(fit) list(anova = anova(fit, test='LR'))
    }

  if(deparse(substitute(fitter))[1] == 'lm')
    warning('If you use print, summary, or anova on the result, lm methods use the\nsum of squared residuals rather than the Rubin formula for computing\nresidual variance and standard errors.  It is suggested to use ols\ninstead of lm.')
  
  using.Design <- FALSE
  used.mice <- any(class(xtrans)=='mids')
  if (used.mice && !requireNamespace("mice", quietly = TRUE))
    stop("This data requires the 'mice' package.")
  if(used.mice && missing(n.impute)) n.impute <- xtrans$m
  fits <- if(fit.reps) vector('list', n.impute)
  stats.ok2average <- c('linear.predictors','fitted.values','stats', 'means',
                        'icoef', 'scale', 'center', 'y.imputed')

  if(! missing(fun) && method != 'only stack')
    funresults <- vector('list', n.impute)

  if(! missing(data) && inherits(data, 'data.table'))
    data <- as.data.frame(data)

  if(! missing(cluster)) robust <- TRUE
  if(robust) {
    if(! requireNamespace('rms', quietly=TRUE))
      stop('the rms package must be installed to use robust or cluster')
    if(missing(fitargs)) fitargs <- list(x=TRUE, y=TRUE)
    else { fitargs$x <- fitargs$y <- TRUE }
  }

  if(method != 'ordinary') Data <- vector('list', n.impute)
  
  for(i in 1 : n.impute) {
    if(! missing(fun) && pr) cat('Imputation', i, '\r')
    if(used.mice) {
      completed.data <- mice::complete(xtrans, i)
      for(impvar in names(completed.data))
        if(length(attr(completed.data[[impvar]], 'contrasts')))
          attr(completed.data[[impvar]], 'contrasts') <- NULL
    }
    else {
      completed.data <- data
      imputed.data <-
        impute.transcan(xtrans, imputation=i, data=data,
                        list.out=TRUE, pr=FALSE, check=FALSE)
      ## impute.transcan works for aregImpute
      completed.data[names(imputed.data)] <- imputed.data
    }

    if(!missing(dtrans)) completed.data <- dtrans(completed.data)

    if(! missing(derived)) {
      stop('derived variables in fit.mult.imputed not yet implemented')
      eval(derived, completed.data)
    }

    if(method != 'ordinary') Data[[i]] <- completed.data

    if(method != 'only stack') {    # individual fits needed
      if(using.Design) options(Design.attr=da)
      afit <- list(formula=formula,
                   data=if(missing(subset)) completed.data
                        else                completed.data[subset,])
      if(! missing(fitargs)) afit <- c(afit, fitargs)
      f <- do.call(fitter, afit)

      stats <- f$stats
      
      if(robust) f <- if(missing(cluster))
                        rms::robcov(f,          method=robmethod)
                  else  rms::robcov(f, cluster, method=robmethod)
    
      ## For some reason passing subset= causes model.frame bomb in R
      if(fit.reps) fits[[i]] <- f
      
      if(! missing(fun) && method != 'only stack')
        funresults[[i]] <- fun(f)
      
      cof <- f$coef
      v <- do.call('vcov', c(list(object=f, intercepts='all'), vcovOpts))
      
      if(i == 1) {
        if(inherits(f, 'orm') && length(f$na.action) &&
           length(f$na.action$nmiss) && f$na.action$nmiss[1] > 0)
          warning('When using fit.mult.impute with orm, there should not be any missing\nY values because different imputations will result in differing numbers\nof intercepts')
        assign <- f$assign
        ns     <- num.intercepts(f)
        ik <- coef.intercepts <- NULL
        if(ns > 0) {
          ik <- attr(v, 'intercepts')  # intercepts kept in f$var
          if(length(ik)) {
            if(ik == 'all') ik <- 1 : ns else if(ik == 'none') ik <- 0
            lenik <- length(ik); if(length(ik) == 1 && ik == 0) lenik <- 0
            ## Shift parameter indexes to left b/c of omitted intercepts for orm
            if(lenik != ns) {
              for(j in 1 : length(assign))
                assign[[j]] <- assign[[j]] - (ns - lenik)
              coef.intercepts <- ik
            }
          }
        }
      }
      if(length(ik)) cof <- c(cof[ik], cof[-(1 : ns)])
    
      ## From Rainer Dyckerhoff to work correctly with models that have
      ## a scale parameter (e.g. psm).  Check whether length of the
      ## coefficient vector is different from the the number of rows of
      ## the covariance matrix. If so, the model contains scale
      ## parameters that are not fixed at some value and we have to 
      ## append the scale parameters to the coefficient vector.
      nvar0 <- length(cof)
      nvar <- nrow(v)
      if(nvar > nvar0) {
        cof <- c(cof, log(f$scale))
        names(cof) <- c(names(f$coef),
                        if((nvar - nvar0) == 1) "Log(scale)"
                        else names(f$scale))
      }
      
      if(i==1) {
        vavg <- 0*v
        p <- length(cof)
        bar <- rep(0, p)
        vname <- names(cof)
        cov <- matrix(0, nrow=p, ncol=p, dimnames=list(vname,vname))
        
        astats <- NULL
        fitcomp <- names(f)[names(f) %in% stats.ok2average]
        if(length(fitcomp)) for(ncomp in fitcomp)
                              astats[[ncomp]] <- f[[ncomp]]
        
        if(inherits(f,'Design') | inherits(f, 'rms')) {
          using.Design <- TRUE
          da <- f$Design
        }
      }

      vavg <- vavg + v
      bar  <- bar + cof
      cof  <- as.matrix(cof)
      cov  <- cov + cof %*% t(cof)
      
      if(i > 1 && length(fitcomp))
        for(ncomp in fitcomp)
          astats[[ncomp]] <- astats[[ncomp]] + f[[ncomp]]
      } # end ordinary or stacked including individual fits
    }   # end main loop i over n.impute

  if(method == 'only stack')
    warning("standard errors for frequently missing variables will ",
            "be underestimated with method='only stack'")
  else {
    vavg <- vavg / n.impute    ## matrix \bar{U} in Rubin's notation
    bar  <- bar  / n.impute
    bar  <- as.matrix(bar)
    ## Matrix B in Rubin's notation:
    cov  <- (cov - n.impute * bar %*% t(bar)) / (n.impute - 1)
    U    <- diag(vavg)
    B    <- diag(cov)  ## save the diagonals of U and B

    cov <- vavg + (n.impute + 1) / n.impute * cov  ## final covariance matrix

    r <- diag(cov) / diag(vavg)
    names(r) <- vname
    tau  <- (1 + 1/n.impute) * B / U
    missingInfo <- tau / (1 + tau)
    dfmi <- (n.impute - 1) * ((1 + 1/tau)^2)
    ## Same as dfmi <- (n.impute - 1) * (1 + U / (B * (1 + 1 / n.impute))) ^ 2

    if(length(fitcomp))
      for(ncomp in fitcomp)
        f[[ncomp]] <- astats[[ncomp]] / n.impute
    
    if(pr) {
      cat('\nWald Statistic Information\n\nVariance Inflation Factors Due to Imputation:\n\n')
      print(round(r,2))
      cat('\nFraction of Missing Information:\n\n')
      print(round(missingInfo,2))
      cat('\nd.f. for t-distribution for Tests of Single Coefficients:\n\n')
      print(round(dfmi,2))
      if(length(fitcomp)) {
        cat('\nThe following fit components were averaged over the',
            n.impute, 'model fits:\n\n')
        cat(' ', fitcomp, '\n\n')
      }
    }
  
    f$coefficients <- drop(bar)
    if(length(coef.intercepts))
      attr(f$coefficients, 'intercepts') <- coef.intercepts
    attr(cov, 'intercepts') <- ik
    f$var <- cov
    f$variance.inflation.impute <- r
    f$missingInfo <- missingInfo
    f$dfmi    <- dfmi
    f$fits    <- fits
    f$formula <- formula
    f$assign  <- assign
    f$call    <- call
    if(! missing(fun)) f$funresults <- funresults
    if(using.Design) options(Design.attr=NULL)
    class(f) <- c('fit.mult.impute', class(f))
    f$fmimethod <- 'ordinary'
    f$n.impute  <- n.impute
    if(method == 'ordinary') return(f)
  } # end ordinary

  ## Final result based on single fit of stacked data
  ## Stack individual data frames into one tall one

  f <- do.call(fitter, c(list(formula=formula, data=do.call(rbind, Data)),
                         if(! missing(fitargs)) fitargs))
  f$call    <- call
  ## Fix variances/covariances to not reward > 1 row per observation
  M <- n.impute
  if(method != 'only stack') f$var <- cov
  else if(length(f$var))     f$var <- f$var * M
  ## For statistics that are sample-size related such as frequency counts
  ## and chi-square divide them by the number of imputations
  st <- f$stats
  ns <- length(st)
  sn <- intersect(names(st),
                  c('Model L.R.', 'Obs', 'Events', 'error d.f.',
                    'Sum of Weights', 'n'))
  if(length(sn)) for(s in sn) st[s] <- st[s] / M
  lr <- st['Model L.R.']
  if('Model L.R.' %in% sn)
    st['P'] <- 1. - pchisq(lr, st['d.f.'])
  if('freq' %in% names(f)) f$freq <- f$freq / M
  if('clusterInfo' %in% names(f)) f$clusterInfo$n <- f$clusterInfo$n / M

  ## For ols fits make the residual d.f. "real"
  if(inherits(f, 'ols')) f$df.residual <- unname(st['n'] - st['d.f.'] - 1)

  f$fmimethod <- method
  f$n.impute  <- n.impute
  if(! missing(fun)) {
    if(funstack) {
      if(method == 'only stack') funresults <- fun(f)
      else funresults[[n.impute + 1]] <- fun(f)
      }
    f$funresults <- funresults
    }
  f$stats <- st
  f$x <- f$y <- NULL
  class(f) <- c('fit.mult.impute', class(f))
  f
}


## orm fit$var has only middle intercept
## fit.mult.impute from orm has all intercepts
vcov.fit.mult.impute <-
  function(object, regcoef.only=TRUE, intercepts='mid', ...) {
    if(inherits(object, 'orm'))
      return(NextMethod('vcov', object, regcoef.only=regcoef.only,
                           intercepts=intercepts, ...))
    ns    <- num.intercepts(object)
    v     <- object$var
    if(ns == 0) return(v)
    vari  <- attr(v, 'intercepts')
    lvari <- length(vari)
    
    if(is.character(intercepts)) {
      switch(intercepts,
             mid = {
               if(lvari > 0L && lvari != 1L)
                 stop('requested middle intercept but more than one intercept stored in object$var')
               return(v) },
             all = {
               if(lvari > 0 && lvari < ns)
                 stop('requested all intercepts but not all stored in object$var')
               return(v) },
             none = if(inherits(object, 'orm') && lvari == 1)
              return(v[-1, -1, drop=FALSE]) else
              return(v[-(1 : ns),-(1 : ns), drop=FALSE]))
    }
    ## intercepts is integer scalar or vector
    if(lvari && isTRUE(all.equal(sort(vari), sort(intercepts)))) return(v)
    if(length(intercepts) == ns) return(v)
    if(length(intercepts) >  ns) stop('more intercepts requested than in model')
    i  <- c(intercepts, (ns + 1) : ncol(v))
    v[i, i, drop=FALSE]
}
