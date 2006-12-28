# $Id$
aregImpute <- function(formula, data, subset, n.impute=5,
                       group=NULL, nk=0,
                       type=c('pmm','regression'),
                       match=c('weighted','closest'), fweighted=0.2,
                       burnin=3, x=FALSE,
                       pr=TRUE, plotTrans=FALSE,
                       tolerance=NULL, B=75)
{
  
  acall   <- match.call()
  type    <- match.arg(type)
  match   <- match.arg(match)

  if(!inherits(formula,'formula'))
    stop('formula must be a formula')
  
  nam <- var.inner(formula)

  m <- match.call(expand = FALSE)
  Terms <- terms(formula, specials='I')
  m$formula <- formula
  m$match <- m$fweighted <- m$x <- m$n.impute <- m$nk <- m$burnin <-
    m$type <- m$group <- m$pr <- m$plotTrans <- m$tolerance <- m$B <- NULL
  m$na.action <- na.retain

  m[[1]] <- as.name("model.frame")
  z <- eval(m, sys.parent())
  p <- length(z)
  n <- nrow(z)
  rnam <- row.names(z)
  if(length(rnam)==0) rnam <- as.character(1:n)

  lgroup <- length(group)
  if(lgroup) {
    if(lgroup != n)
      stop('group should have length equal to number of observations')
    
    ngroup <- length(unique(group[!is.na(group)]))
  }

  linear <- nam[attr(Terms,'specials')$I]

  cat.levels <- vector('list',p)
  names(cat.levels) <- nam
  vtype <- rep('s', p); names(vtype) <- nam
  dof <- rep(NA, p); names(dof) <- nam
  na <- vector('list',p)
  names(na) <- nam
  nna <- integer(p); names(nna) <- nam

  xf <- matrix(as.double(1), nrow=n, ncol=p, dimnames=list(rnam,nam))
  imp <- vector('list',p)
  names(imp) <- nam
  if(lgroup) group.inds <- imp

  for(i in 1:p) {
    xi <- z[[i]]
    ni <- nam[i]
    nai <- is.na(xi)
    na[[i]] <- (1:n)[nai] 
    nna[i] <- nnai <- sum(nai)
    if(nnai > 0) imp[[ni]] <-  matrix(NA, nrow=nnai, ncol=n.impute,
                                      dimnames=list(rnam[nai],NULL))
    if(lgroup) {
      if(any(is.na(group[!nai])))
        stop('NAs not allowed in group')
      
      if(length(unique(group[!nai])) != ngroup)
        stop(paste('not all',ngroup,
                   'values of group are represented in\n',
                   'observations with non-missing values of',
                   ni))
      group.inds[[i]] <- split((1:n)[!nai], group[!nai])
    }
  
    iscat <- FALSE
    if(is.character(xi)) {
      xi <- as.factor(xi)
      lev <- levels(xi)
      iscat <- TRUE
    }
    else if(is.category(xi)) {
      lev <- levels(xi)
      iscat <- TRUE
    }
    if(iscat) {
      cat.levels[[ni]] <- lev
      xi <- as.integer(xi)
      vtype[ni] <- 'c'
    }
    else {
      u <- unique(xi[!nai])
      if(length(u) == 1)
        stop(paste(ni,'is constant'))
      else
        if((length(nk)==1 && nk==0) || length(u) == 2 || ni %in% linear)
          vtype[ni] <- 'l'
    }
    xf[,i] <- xi
    
    ## Initialize imputed values to random sample of non-missings
    if(nnai > 0) xf[nai,i] <-
      sample(xi[!nai], nnai, replace=nnai > (n-nnai))
  }
  z <- NULL
  wna <- (1:p)[nna > 0]

  
  ## xf = original data matrix (categorical var -> integer codes)
  ## with current imputations
  rsq <- double(length(wna));
  names(rsq) <- nam[wna]

  resampacc <- list()
  
  for(iter in 1:(burnin + n.impute)) {
    if(pr) cat('Iteration',iter,'\r')
    for(i in wna) {
      nai <- na[[i]]      ## subscripts of NAs on xf[i,]
      j <- (1:n)[-nai]    ## subscripts of non-NAs on xf[i,]
      npr <- length(j)
      
      if(iter==(burnin + n.impute) && length(nk) > 1) {
        rn <- c('Bootstrap bias-corrected R^2',
                '10-fold cross-validated  R^2',
                'Bootstrap bias-corrected mean   |error|',
                '10-fold cross-validated  mean   |error|',
                'Bootstrap bias-corrected median |error|',
                '10-fold cross-validated  median |error|')
        racc <- matrix(NA, nrow=6, ncol=length(nk),
                       dimnames=list(rn, paste('nk=',nk,sep='')))
        jj <- 0
        for(k in nk) {
          jj <- jj + 1
          f <- areg(xf[,-i,drop=FALSE], xf[,i],
                    xtype=vtype[-i], ytype=vtype[i],
                    nk=k, na.rm=FALSE,
                    tolerance=tolerance, B=B, crossval=10)
          w <- c(f$r2boot, f$rsquaredcv, f$madboot, f$madcv,
                 f$medboot, f$medcv)
          racc[,jj] <- w
        }
        resampacc[[nam[i]]] <- racc
      }

      if(lgroup) {        ## insure orig. no. obs from each level of group
        s <- rep(NA, npr)
        for(ji in 1:ngroup) {
          gi <- (group.inds[[i]])[[ji]]
          s[gi] <- sample(gi, length(gi), replace=TRUE)
        }
      }
      else
        s <- sample(j, npr, replace=TRUE)  ## sample of non-NAs
      
      nami <- nam[i]
      nm <- c(nami, nam[-i])

      X <- xf[,-i,drop=FALSE]

      f <- areg(X[s,], xf[s,i], xtype=vtype[-i], ytype=vtype[i],
                nk=min(nk), na.rm=FALSE, tolerance=tolerance)
      dof[names(f$xdf)] <- f$xdf
      dof[nami] <- f$ydf
      
      if(plotTrans) plot(f)
      
      rsq[nami] <- f$rsquared
      pti <- predict(f, X)  # predicted transformed xf[,i]
      
      if(type=='pmm') {
      if(vtype[i]=='l') pti <- (pti - mean(pti))/sqrt(var(pti))
        whichclose <- if(match=='closest') {
          
          ## Jitter predicted transformed values for non-NAs to randomly
          ## break ties in matching with predictions for NAs in xf[,i]
          ## Becuase of normalization used by fitter, pti usually ranges
          ## from about -4 to 4
          pti[j] <- pti[j] + runif(npr,-.0001,.0001)
          
          ## For each orig. missing xf[,i] impute with non-missing xf[,i]
          ## that has closest predicted transformed value
          j[whichClosest(pti[j], pti[nai])]  ## see Misc.s
        }
        else
          j[whichClosePW(pti[j], pti[nai], f=fweighted)]
        impi <- xf[whichclose,i]
      } else {
        ## residuals off of transformed predicted values
        res <- f$residuals
        
        ## predicted transformed target var + random sample of res,
        ## for NAs
        ptir <- pti[nai] +
          sample(res, length(nai),
                 replace=length(nai) > length(res))
        
        ## predicted random draws on untransformed scale
        impi <- approxExtrap(f$ty, f$y, xout=ptir)$y
      }
      xf[nai,i] <- impi
      if(iter > burnin) imp[[nam[i]]][,iter-burnin] <- impi
    }
  }
  if(pr)
    cat('\n')

  if(!x)
    xf <- NULL
  
  structure(list(call=acall, formula=formula,
                 match=match, fweighted=fweighted,
                 n=n, p=p, na=na, nna=nna,
                 type=vtype, nk=min(nk),
                 cat.levels=cat.levels, df=dof,
                 n.impute=n.impute, imputed=imp, x=xf, rsq=rsq,
                 resampacc=resampacc),
            class='aregImpute')
}

print.aregImpute <- function(x, digits=3, ...)
{
  cat("\nMultiple Imputation using Bootstrap and PMM\n\n")
  dput(x$call)
  cat("\n")
  cat('tn:',x$n,'\tp:',x$p,
      '\tImputations:',x$n.impute,' \tnk:',x$nk,'\n')
  cat('\nNumber of NAs:\n'); print(x$nna); cat('\n')
  info <- data.frame(type=x$type, d.f.=x$df,
                     row.names=names(x$type))
  print(info)
  
  cat('\nR-squares for Predicting Non-Missing Values for Each Variable\nUsing Last Imputations of Predictors\n')
  print(round(x$rsq, digits))

  racc <- x$resampacc
  if(length(racc)) {
    cat('\nResampling results for determining the complexity of imputation models\n\n')
    for(i in 1:length(racc)) {
      cat('Variable being imputed:', names(racc)[i], '\n')
      print(racc[[i]], digits=digits)
      cat('\n')
    }
    cat('\n')
  }
  invisible()
}

plot.aregImpute <- function(x, nclass=NULL, type=c('ecdf','hist'),
                            diagnostics=FALSE, maxn=10, ...)
{
  type <- match.arg(type)
  i <- x$imputed
  catg <- x$categorical
  lev  <- x$cat.levels
  n.impute <- x$n.impute
  for(n in names(i)) {
    xi <- i[[n]]
    if(!length(xi))
      next
    
    if(diagnostics) {
      r <- range(xi)
      ## cat(min(maxn,nrow(xi)))
      for(j in 1:min(maxn,nrow(xi))) {
        plot(1:n.impute, xi[j,], ylim=r, xlab='Imputation',
             ylab=paste("Imputations for Obs.",j,"of",n))
      }
    }
    
    ix <- as.vector(i[[n]])
    lab <- paste('Imputed',n)
    if(n %in% catg) {
      tab <- table(ix)
      mar <- par('mar')
      dotchart2(tab, lev[[n]], auxdata=tab, xlab='Frequency',
                ylab=lab)
      par(mar=mar)
    }
    else {
      if(type=='ecdf')
        Ecdf(ix, xlab=lab, datadensity='hist', subtitles=FALSE)
      else {
        if(length(nclass))
          hist(ix, xlab=n, nclass=nclass, main='')
        else
          hist(ix, xlab=lab, main='')
        scat1d(ix)
      }
    }
  }
  invisible()
}
