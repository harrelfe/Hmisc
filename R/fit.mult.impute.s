fit.mult.impute <- function(formula, fitter, xtrans, data,
                            n.impute=xtrans$n.impute, fit.reps=FALSE,
                            dtrans, derived,
                            vcovOpts=NULL,
                            pr=TRUE, subset, ...)
{
  call <- match.call()

  if(deparse(substitute(fitter)) == 'lm')
    warning('If you use print, summary, or anova on the result, lm methods use the\nsum of squared residuals rather than the Rubin formula for computing\nresidual variance and standard errors.  It is suggested to use ols\ninstead of lm.')
  
  using.Design <- FALSE
  fits <- if(fit.reps) vector('list', n.impute)
  used.mice <- any(class(xtrans)=='mids')
  if(used.mice && missing(n.impute)) n.impute <- xtrans$m
  stats.ok2average <- c('linear.predictors','fitted.values','stats', 'means',
                        'icoef', 'scale', 'center', 'y.imputed')
  
  for(i in 1 : n.impute) {
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

    if(!missing(derived)) {
      stop('derived variables in fit.mult.imputed not yet implemented')
      eval(derived, completed.data)
    }

    if(using.Design) options(Design.attr=da)
    f <- if(missing(subset)) fitter(formula, data=completed.data, ...)
    else fitter(formula, data=completed.data[subset,], ...)
    
    ## For some reason passing subset= causes model.frame bomb in R
    if(fit.reps) fits[[i]] <- f

    cof <- f$coef
    v <- do.call('vcov', c(list(object=f, intercepts='all'), vcovOpts))

    if(i == 1) {
      if(inherits(f, 'orm'))
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
  }
  
  vavg <- vavg / n.impute    ## matrix \bar{U} in Rubin's notation
  bar  <- bar / n.impute
  bar  <- as.matrix(bar)
  ## Matrix B in Rubin's notation:
  cov  <- (cov - n.impute * bar %*% t(bar)) / (n.impute - 1)
  U    <- diag(vavg)
  B    <- diag(cov)  ## save the diagonals of U and B

  cov <- vavg + (n.impute + 1) / n.impute * cov  ## final covariance matrix

  r <- diag(cov) / diag(vavg)
  names(r) <- vname
  tau  <- (1 + 1/n.impute)*B/U
  missingInfo <- tau/(1+tau)
  dfmi <- (n.impute - 1)*((1 + 1/tau)^2)
  ## Same as dfmi <- (n.impute - 1) * (1 + U / (B * (1 + 1 / n.impute))) ^ 2

  if(length(fitcomp))
    for(ncomp in fitcomp)
      f[[ncomp]] <- astats[[ncomp]] / n.impute

  if(pr) {
    cat('\nVariance Inflation Factors Due to Imputation:\n\n')
    print(round(r,2))
    cat('\nRate of Missing Information:\n\n')
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
  if(using.Design) options(Design.attr=NULL)
  class(f) <- c('fit.mult.impute', class(f))
  f
}


vcov.fit.mult.impute <-
  function(object, regcoef.only=TRUE, intercepts='mid', ...) {
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
