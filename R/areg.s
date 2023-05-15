areg <- function(x, y, xtype=NULL, ytype=NULL, nk=4,
                 B=0, na.rm=TRUE,
                 tolerance=NULL, crossval=NULL) {
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  ism <- is.matrix(x)
  if(!ism) {
    x <- as.matrix(x)
    if(!length(colnames(x))) colnames(x) <- xname
  }
  if(na.rm) {
    omit <- is.na(x %*% rep(1,ncol(x))) | is.na(y)
    nmiss <- sum(omit)
    if(nmiss) {
      x <- x[!omit,,drop=FALSE]
      y <- y[!omit]
    }
  } else nmiss <- 0
    
  d <- dim(x)
  n <- d[1]; p <- d[2]
  xnam <- colnames(x)
  if(!length(xnam)) xnam <- paste('x', 1:p, sep='')

  nuy <- length(unique(y))
  
  if(!length(ytype)) ytype <- 
    if(is.factor(y) || is.character(y)) 'c' else
      if(nk==0 || (nuy < 3)) 'l' else 's'
  
  if(nk==0 && ytype=='s') ytype <- 'l'
  if(ytype=='s' && nk > 0 && nuy < 3) {
    warning('requested smooth transformation of y but less than 3 unique values; forced linear')
    ytype <- 'l'
  }

  if(!length(xtype)) xtype <- rep(if(nk==0)'l' else 's', p)
  xtype[nk==0 & xtype=='s'] <- 'l'
  names(xtype) <- xnam

  nux <- apply(x, 2, function(z) length(unique(z)))
  tooTied <- xtype=='s' & nux < nk
  if(any(tooTied)) {
    warning(paste('the following x variables have too few unique values for the value of nk;\nlinearity forced:', paste(xnam[tooTied], collapse=',')))
    xtype[nux] <- 'l'
  }

  fcancor <- function(X, Y) {
    ## colnames must exist for correct insertion of zeros for singular
    ## elements
    colnames(X) <- paste('x', 1:ncol(X), sep='')
    colnames(Y) <- paste('y', 1:ncol(Y), sep='')
    ## If canonical variate transformation of Y is descending in Y,
    ## negate all parameters
    f <- cancor(X, Y)
    f$r2 <- f$cor[1]^2
    n <- nrow(Y); if(!length(n)) n <- length(y)
    varconst <- sqrt(n-1)
    ## cancor returns rows only for non-singular variables
    ## For first canonical variate insert zeros for singular variables
    xcoef <- 0. * X[1, ]
    b     <- f$xcoef[, 1]
    xdf   <- length(b)
    xcoef[names(b)] <- b
    xcoef <- c(intercept = -sum(xcoef * f$xcenter), xcoef) * varconst
    ycoef <- 0. * Y[1, ]
    b     <- f$ycoef[, 1]
    ydf   <- length(b)
    ycoef[names(b)] <- b
    ycoef <- c(intercept = -sum(ycoef * f$ycenter), ycoef) * varconst
    ty <- matxv(Y, ycoef)
    g <- lm.fit.qr.bare(Y, ty, singzero=TRUE)
    
    if(g$coefficients[2] < 0) {
      xcoef <- -xcoef
      ycoef <- -ycoef
      ty    <- -ty
    }
    f$xcoef <- xcoef
    f$ycoef <- ycoef
    f$ty    <- ty
    f$xdf   <- xdf
    f$ydf   <- ydf
    f
  }

  need2getinv <- FALSE
  xpxi <- NULL
  
  Y <- aregTran(y, ytype, nk, functions=TRUE)
  at <- attributes(Y)
  ytrans <- at$fun
  yinv   <- at$inversefun  ## NULL if type='s'; need coef
  yparms <- at$parms
  
  xdf <- ifelse(xtype=='l', 1, nk - 1)
  j <- xtype == 'c'
  if(any(j)) xdf[j] <- nux[j] - 1
  names(xdf) <- xnam

  X <- matrix(NA, nrow=n, ncol=sum(xdf))
  xparms <- list()
  j <- 0
  xn <- character(0)
  for(i in 1 : p) {
    w <- aregTran(x[,i], xtype[i], nk)
    xparms[[xnam[i]]] <- attr(w, 'parms')
    m <- ncol(w)
    xdf[i] <- m
    X[, (j + 1) : (j + m)] <- w
    j <- j + m
    xn <- c(xn, paste(xnam[i], 1:m, sep=''))
  }
  ## See if rcpsline.eval could not get desired no. of knots due to ties
  if(ncol(X) > sum(xdf)) X <- X[, 1 : sum(xdf), drop=FALSE]
  
  covx <- covy <- r2opt <- r2boot <-
    madopt <- madboot <- medopt <- medboot <- NULL
  if(B > 0L) {
    r <- 1L + sum(xdf)
    barx <- rep(0., r)
    vname <- c('Intercept', xn)
    covx <- matrix(0, nrow=r, ncol=r, dimnames=list(vname,vname))
    if(ytype != 'l') {
      r <- ncol(Y) + 1L
      bary  <- rep(0., r)
      vname <- c('Intercept', paste(yname, 1 : (r - 1), sep=''))
      covy  <- matrix(0, nrow=r, ncol=r, dimnames=list(vname, vname))
    }
  }
  if(ytype == 'l') {
    f <- lm.fit.qr.bare(X, Y, tolerance=tolerance, xpxi=TRUE, singzero=TRUE)
    xcof <- f$coefficients
    r2   <- f$rsquared
    xpxi <- f$xpxi
    cof  <- 1
    ty   <- y
    ydf  <- 1
    lp   <- f$fitted.values
    res  <- f$residuals
    mad  <- mean  (abs(y - lp))
    med  <- median(abs(y - lp))
    if(B > 0) {
      r2opt <- madopt <- medopt <- 0
      for(j in 1:B) {
        s <- sample(1:n, replace=TRUE)
        if(ytype=='c' && length(unique(y[s])) < nuy)
          stop('a bootstrap resample had too few unique values of y')
        xch <- which(xtype == 'c')
        if(length(xch)) {
          xtf <- apply(x[s, xch, drop=FALSE], 2,
                       function(z)length(unique(z))) < nux[xch]
          if(any(xtf))
            stop(paste('a bootstrap resample had too few unique values of the following x variables:',
                       paste(xnam[xch[xtf]], collapse=','), sep='\n'))
                }
        g <- lm.fit.qr.bare(X[s,, drop=FALSE], Y[s], singzero=TRUE)
        b <- g$coefficients
        r2boot <- g$rsquared
        yhat   <- matxv(X, b)
        r2orig <- cor(yhat, y)^2
        r2opt  <- r2opt + r2boot - r2orig
        er <- abs(Y[s] - g$fitted.values)
        madboot <- mean(er)
        medboot <- median(er)
        er      <- abs(y - yhat)
        madorig <- mean(er)
        medorig <- median(er)
        madopt  <- madopt + madboot - madorig
        barx    <- barx + b
        b       <- as.matrix(b)
        covx    <- covx + b %*% t(b)
      }
      r2opt   <- r2opt / B
      r2boot  <- r2 - r2opt
      madopt  <- madopt / B
      madboot <- mad - madopt
      medopt  <- medopt / B
      medboot <- med - medopt
      barx <- as.matrix(barx / B)
      covx <- (covx - B * barx %*% t(barx)) / (B - 1)
    }
  } else {
    f <- fcancor(X, Y)
    r2   <- f$r2
    xcof <- f$xcoef
    cof  <- f$ycoef
    ty   <- f$ty
    ydf  <- f$ydf
    lp   <- as.vector(matxv(X, xcof))
    res  <- as.vector(ty - lp)
    
    if(!length(yinv)) {
      ## spline transformation, need coef to get inverse y transform
      yy   <- seq(min(y), max(y), length=1000)
      tyy  <- ytrans(yy, coef=cof)
      yinv <- inverseFunction(yy, tyy)
      need2getinv <- TRUE
    }
    
    puy  <- yinv(lp, what='sample')
    if(length(y) != length(puy)) stop('program logic error')
    mad  <- mean  (abs(y - puy))
    med  <- median(abs(y - puy))
    
    if(B > 0) {
      r2opt <- madopt <- medopt <- 0
      for(j in 1L : B) {
        s <- sample(1L : n, replace=TRUE)
        if(ytype=='c' && length(unique(y[s])) < nuy)
          stop('a bootstrap resample had too few unique values of y')
        xch <- which(xtype == 'c')
        if(length(xch)) {
          xtf <- apply(x[s, xch, drop=FALSE], 2,
                       function(z)length(unique(z))) < nux[xch]
          if(any(xtf))
            stop(paste('a bootstrap resample had too few unique values of the following x variables:',
                       paste(xnam[xch[xtf]], collapse=','), sep='\n'))
        }
        
        f <- fcancor(X[s,, drop=FALSE], Y[s,, drop=FALSE])
        bx     <- f$xcoef
        by     <- f$ycoef
        r2boot <- f$r2
        xbeta  <- matxv(X, bx)
        ybeta  <- matxv(Y, by)
        r2orig <- cor(xbeta, ybeta)^2
        r2opt  <- r2opt + r2boot - r2orig
        puyall <- if(need2getinv) {
          tyyb  <- ytrans(yy, coef=by)  ## keeping constant knots
          yinvb <- inverseFunction(yy, tyyb)
          yinvb(xbeta, coef=by, what='sample')
        }
        else
          yinv(xbeta, coef=by)
        er      <- abs(y[s] - puyall[s])
        madboot <- mean(er)
        medboot <- median(er)
        er      <- abs(y - puyall)
        madorig <- mean(er)
        medorig <- median(er)
        madopt  <- madopt + madboot - madorig
        medopt  <- medopt + medboot - medorig
        barx    <- barx + bx
        bx      <- as.matrix(bx)
        covx    <- covx + bx %*% t(bx)
        bary    <- bary + by
        by      <- as.matrix(by)
        covy    <- covy + by %*% t(by)
      }
      r2opt   <- r2opt / B
      r2boot  <- r2 - r2opt
      madopt  <- madopt / B
      madboot <- mad - madopt
      medopt  <- medopt / B
      medboot <- med - medopt
      
      barx <- as.matrix(barx / B)
      bary <- as.matrix(bary / B)
      covx <- (covx - B * barx %*% t(barx)) / (B - 1)
      covy <- (covy - B * bary %*% t(bary)) / (B - 1)
    }
  }
  j <- 0
  beta <- xcof[-1]
  tx <- x
  xmeans <- list()
  for(i in 1:p) {
    m <- xdf[i]
    z <- matxv(X[, (j + 1) : (j + m), drop=FALSE], beta[(j + 1) : (j + m)])
    mz <- mean(z)
    xmeans[[xnam[i]]] <- mz
    tx[,i] <- z - mz
    j <- j + m
  }
  r2cv <- madcv <- medcv <- NULL
  if(length(crossval)) {
    s <- sample(1:crossval, n, replace=TRUE)
    r2cv <- madcv <- medcv <- 0
    for(j in 1:crossval) {
      g    <- fcancor(X[s!=j,, drop=FALSE], Y[s!=j,, drop=FALSE])
      bx   <- g$xcoef
      by   <- g$ycoef
      xbo  <- matxv(X[s==j,, drop=FALSE], bx)
      ybo  <- matxv(Y[s==j,, drop=FALSE], by)
      r2cv <- r2cv + cor(xbo, ybo)^2
      puy <- if(need2getinv) {
        tyyb  <- ytrans(yy, coef=by)  ## keeping constant knots
        yinvb <- inverseFunction(yy, tyyb)
        yinvb(xbo, coef=by, what='sample')
      }
      else yinv(xbo, coef=by)
      er   <- abs(y[s==j] - puy)
      madcv<- madcv + mean(er)
      medcv<- medcv + median(er)
    }
    r2cv  <- r2cv / crossval
    madcv <- madcv / crossval
    medcv <- medcv / crossval
  }
  structure(list(y=y, x=x, ty=ty, tx=tx,
                 rsquared=r2, rsquaredcv=r2cv, nk=nk, xdf=xdf, ydf=ydf,
                 xcoefficients=xcof, ycoefficients=cof,
                 xparms=xparms, yparms=yparms, xmeans=xmeans,
                 ytrans=ytrans, yinv=yinv,
                 linear.predictors=lp, residuals=res,
                 xtype=xtype, ytype=ytype, yname=yname,
                 r2boot=r2boot, r2opt=r2opt,
                 mad=mad, madboot=madboot, madopt=madopt,
                 med=med, medboot=medboot, medopt=medopt,
                 madcv=madcv, medcv=medcv,
                 xcov=covx, ycov=covy, xpxi=xpxi,
                 n=n, m=nmiss, B=B, crossval=crossval),
            class='areg')
}

aregTran <- function(z, type, nk = length(parms), parms = NULL,
                     functions = FALSE) {
  if(type=='l' || (type=='s' && nk==0)) 
    return(if(functions)
           structure(as.matrix(z),
                     fun       =function(x,...) x,
                     inversefun=function(x,...) x) else as.matrix(z))

  if(type=='c') {
    n <- length(z)
    lp <- length(parms)
    ## Assume z is integer code if parms is given
    w <- if(lp) z else factor(z)
    x <- as.integer(w)
    if(!lp) parms <- 1 : max(x, na.rm=TRUE)
    z <- matrix(0, nrow=n, ncol=length(parms) - 1)
    z[cbind(1 : n, x - 1)] <- 1
    attr(z, 'parms') <- if(lp)parms else levels(w)
    if(functions) {
      attr(z, 'fun') <- function(x, parms, coef) {
        if(length(parms) > length(coef)) coef <- c(0,coef)
        coef[-1] <- coef[-1] + coef[1]
        names(coef) <- parms
        coef[x]
      }
      formals(attr(z, 'fun')) <-
        list(x=integer(0), parms=parms, coef=numeric(0))
      
      ## what is ignored; for compatibility with inverseFunction in Misc.s
      attr(z, 'inversefun') <- function(y, parms, coef, what=character(0)) {
        if(length(parms) > length(coef)) coef <- c(0, coef)
        isna <- is.na(y)
        y[isna] <- 0
        x <- parms[whichClosest(c(coef[1], coef[1] + coef[-1]), y)]
        x[isna] <- NA
        x
      }
      formals(attr(z, 'inversefun')) <-
        list(y=numeric(0), parms=parms,
             coef=numeric(0), what=character(0))
      
    }
    z
  }
  else {
    z <- rcspline.eval(z, knots=parms, nk=nk, inclx=TRUE)
    knots <- attr(z, 'knots')
    attr(z,'parms') <- knots
    if(functions) attr(z, 'fun') <- rcsplineFunction(knots)
    ## inverse function created later when coefficients available
    z
  }
}

predict.areg <- function(object, x, type=c('lp','fitted','x'),
                         what=c('all','sample'), ...) {
  type <- match.arg(type)
  what <- match.arg(what)
  beta   <- object$xcoefficients
  xparms <- object$xparms
  xtype  <- object$xtype
  xdf    <- object$xdf
  ybeta  <- object$ycoefficients
  yinv   <- object$yinv
  x <- as.matrix(x)
  p <- length(xdf)
  X <- matrix(NA, nrow=nrow(x), ncol=sum(xdf))
  j <- 0
  xnam <- names(xtype)
  for(i in 1:p) {
    w <- aregTran(x[,i], xtype[i], parms=xparms[[xnam[i]]])
    m <- ncol(w)
    X[, (j + 1) : (j + m)] <- w
    j <- j + m
  }
  if(type == 'x') return(cbind(1, X))
  xb <- matxv(X, beta)
  if(type=='fitted') yinv(xb, what=what, coef=ybeta) else xb
}

print.areg <- function(x, digits=4, ...) {
  xdata <- x[c('n', 'm', 'nk', 'rsquared', 'xtype', 'xdf', 'ytype', 'ydf')]
  xinfo <- data.frame(type=xdata$xtype, d.f.=xdata$xdf,
                      row.names=names(xdata$xtype))
  cat('\nN:', xdata$n, '\t', xdata$m,
      ' observations with NAs deleted.\n')
  cat('R^2: ', round(xdata$rsquared, 3), '\tnk: ', xdata$nk,
      '\tMean and Median |error|: ', format(x$mad, digits=digits), ', ',
      format(x$med, digits=digits), '\n\n', sep='')
  if(length(x$r2boot)) {
    x1 <- format(c(x$r2opt,  x$madopt,  x$medopt),  digits=digits)
    x2 <- format(c(x$r2boot, x$madboot, x$medboot), digits=digits)
    n  <- c('R^2', 'Mean |error|', 'Median |error|')
    d  <- cbind('Bootstrap Estimates'=n, Optimism=x1, 'Optimism-corrected'=x2)
    row.names(d) <- rep('', 3)
    print(d, quote=FALSE, right=TRUE)
  }
  if(length(x$crossval)) {
    x1 <- format(c(x$rsquaredcv, x$madcv, x$medcv), digits=digits)
    n  <- c('R^2', 'Mean |error|', 'Median |error|')
    d  <- cbind(n, x1)
    dimnames(d) <- list(rep('',3), 
                        c(paste(x$crossval,'-fold Cross-validation',sep=''),
                          'Estimate'))
    cat('\n')
    print(d, quote=FALSE, right=TRUE)
  }
  cat('\n')
  print(xinfo)
  cat('\ny type:', xdata$ytype, '\td.f.:', xdata$ydf, '\n\n')
  invisible()
}

plot.areg <- function(x, whichx=1 : ncol(x$x), ...) {
  plot(x$y, x$ty, xlab=x$yname,
       ylab=paste('Transformed',x$yname))
  r2 <- round(x$rsquared, 3)
  title(sub=bquote(R^2==.(r2)), adj=0)
  xdata <- x$x
  cn <- colnames(xdata)
  for(i in whichx)
    plot(xdata[,i], x$tx[,i],
         xlab=cn[i], ylab=paste('Transformed', cn[i]), ...) 
  invisible()
}
