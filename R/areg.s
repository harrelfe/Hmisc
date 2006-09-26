# $Id$
areg <- function(x, y, xtype=NULL, ytype=NULL, nk=4,
                 linear.predictors=FALSE,
                 B=0, na.rm=TRUE,
                 tolerance=NULL) {

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
  if(!length(xnam)) xnam <- paste('x',1:p,sep='')
  if(!length(ytype)) ytype <- 
    if(is.factor(y) || is.category(y) || is.character(y)) 'c' else
      if(nk==0 || (length(unique(y)) < 3)) 'l' else 's'
  if(nk==0 && ytype=='s') ytype <- 'l'

  if(!length(xtype)) xtype <- rep(if(nk==0)'l' else 's', p)
  xtype[nk==0 & xtype=='s'] <- 'l'
  names(xtype) <- xnam
  
  Y <- aregTran(y, ytype, nk)
  yparms <- attr(Y, 'parms')

  xdf <- ifelse(xtype=='l', 1, nk-1)
  j <- xtype=='c'
  if(any(j))
    xdf[j] <- apply(x[,j,drop=FALSE], 2,
                    function(z) length(unique(z)) - 1)
  names(xdf) <- xnam

  X <- matrix(NA, nrow=n, ncol=sum(xdf))
  xparms <- list()
  j <- 0
  xn <- character(0)
  for(i in 1:p) {
    w <- aregTran(x[,i], xtype[i], nk)
    xparms[[xnam[i]]] <- attr(w, 'parms')
    m <- ncol(w)
    xdf[i] <- m
    X[,(j+1):(j+m)] <- w
    j <- j + m
    xn <- c(xn, paste(xnam[i],1:m,sep=''))
  }
  ## See if rcpsline.eval could not get desired no. of knots due to ties
  if(ncol(X) > sum(xdf)) X <- X[,1:sum(xdf),drop=FALSE]

  covx <- covy <- lp <- NULL
  if(B > 0) {
    r <- 1 + sum(xdf)
    barx <- rep(0, r)
    vname <- c('Intercept',xn)
    covx <- matrix(0, nrow=r, ncol=r, dimnames=list(vname,vname))
    if(ytype != 'l') {
      r <- ncol(Y)+1
      bary <- rep(0, r)
      vname <- c('Intercept',paste(yname, 1:(r-1), sep=''))
      covy <- matrix(0, nrow=r, ncol=r, dimnames=list(vname,vname))
    }
  }
  if(ytype=='l') {
    f <- lm.fit.qr.bare(X, Y, tolerance=tolerance)
	xcof <- f$coefficients
	r2 <- f$rsquared
    cof <- 1
    ty <- y
    ydf <- 1
    if(B > 0) {
      for(j in 1:B) {
        s <- sample(1:n, replace=TRUE)
        b <- lm.fit.qr.bare(X[s,,drop=FALSE], Y[s])$coefficients
        barx <- barx + b
        b <- as.matrix(b)
        covx <- covx + b %*% t(b)
      }
      barx <- as.matrix(barx/B)
      covx <- (covx - B * barx %*% t(barx))/(B-1)
    }
  } else {
    fcancor <- function(X, Y) {
      ## If canonical variate transformation of Y is descending in Y,
      ## negate all parameters
      f <- cancor(X, Y)
      f$r2 <- f$cor[1]^2
      n <- nrow(Y); if(!length(n)) n <- length(y)
      varconst <- sqrt(n-1)
      xcoef <- c(intercept = -sum(f$xcoef[, 1] * f$xcenter),
                 f$xcoef[, 1]) * varconst
      ycoef <- c(intercept = -sum(f$ycoef[, 1] * f$ycenter),
                 f$ycoef[, 1]) * varconst
      ty <- matxv(Y, ycoef)
      g <- lm.fit.qr.bare(Y,ty)
      if(g$coefficients[2] < 0) {
        xcoef <- -xcoef
        ycoef <- -ycoef
        ty    <- -ty
      }
      f$xcoef <- xcoef
      f$ycoef <- ycoef
      f$ty    <- ty
      f
    }
    
    f <- fcancor(X, Y)
    r2 <- f$r2
    xcof <- f$xcoef
    cof  <- f$ycoef
    ty   <- f$ty
    ydf <- length(cof) - 1
  
    if(B > 0) {
      for(j in 1:B) {
        s <- sample(1:n, replace=TRUE)
        f <- fcancor(X[s,,drop=FALSE],Y[s,,drop=FALSE])
        bx <- f$xcoef
        by <- f$ycoef
        barx <- barx + bx
        bx <- as.matrix(bx)
        covx <- covx + bx %*% t(bx)
        bary <- bary + by
        by <- as.matrix(by)
        covy <- covy + by %*% t(by)
      }
      barx <- as.matrix(barx/B)
      bary <- as.matrix(bary/B)
      covx <- (covx - B * barx %*% t(barx))/(B-1)
      covy <- (covy - B * bary %*% t(bary))/(B-1)
    }
  }
  j <- 0
  beta <- xcof[-1]
  tx <- x
  xmeans <- list()
  if(linear.predictors) lp <- xcof[1]
  for(i in 1:p) {
    m <- xdf[i]
    z <- matxv(X[,(j+1):(j+m),drop=FALSE], beta[(j+1):(j+m)])
    if(linear.predictors) lp <- lp + z
    mz <- mean(z)
    xmeans[[xnam[i]]] <- mz
    tx[,i] <- z - mz
    j <- j + m
  }
  structure(list(y=y, x=x, ty=ty, tx=tx,
                 rsquared=r2, nk=nk, xdf=xdf, ydf=ydf,
                 xcoefficients=xcof, ycoefficients=cof,
                 xparms=xparms, yparms=yparms, xmeans=xmeans,
                 linear.predictors=lp,
                 xtype=xtype, ytype=ytype, yname=yname,
                 xcov=covx, ycov=covy,
                 n=n, m=nmiss, B=B),
            class='areg')
}

aregTran <- function(z, type, nk=length(parms), parms=NULL) {
  if(type=='l' || (type=='s' && nk==0)) return(as.matrix(z))
  if(type=='c') {
    n <- length(z)
    lp <- length(parms)
    ## Assume z is integer code if parms is given
    w <- if(lp) z else factor(z)
    x <- as.integer(w)
    if(!lp) parms <- 1:max(x)
    z <- matrix(0, nrow=n, ncol=length(parms)-1)
    z[cbind(1:n, x-1)] <- 1
    attr(z, 'parms') <- if(lp)parms else levels(w)
    z
  } else {
    z <- rcspline.eval(z, knots=parms, nk=nk, inclx=TRUE)
    attr(z,'parms') <- attr(z,'knots')
    z
  }
}

predict.areg <- function(object, x) {
  beta   <- object$xcoefficients
  xparms <- object$xparms
  xtype  <- object$xtype
  xdf    <- object$xdf
  x <- as.matrix(x)
  p <- length(xdf)
  X <- matrix(NA, nrow=nrow(x), ncol=sum(xdf))
  j <- 0
  xnam <- names(xtype)
  for(i in 1:p) {
    w <- aregTran(x[,i], xtype[i], parms=xparms[[xnam[i]]])
    m <- ncol(w)
    X[,(j+1):(j+m)] <- w
    j <- j + m
  }
  matxv(X, beta)
}

print.areg <- function(object) {
  x <- object[c('n','m','nk','rsquared','xtype','xdf','ytype','ydf')]
  xinfo <- data.frame(type=x$xtype, d.f.=x$xdf,
                      row.names=names(x$xtype))
  cat('\nN:',x$n,'\t',x$m,
      ' observations with NAs deleted.\n')
  cat('R^2:', round(x$rsquared,3),'\tnk:',x$nk,'\n\n')
  print(xinfo)
  cat('\ny type:', x$ytype,'\td.f.:', x$ydf,'\n\n')
  invisible()
}

plot.areg <- function(object, whichx=1:ncol(x), ...) {
	plot(object$y, object$ty, xlab=object$yname,
         ylab=paste('Transformed',object$yname))
    r2 <- round(object$rsquared,3)
    if(.R.) title(sub=bquote(R^2==.(r2)), adj=0) else
     title(sub=paste('R^2=',r2),adj=0)
    x <- object$x
    cn <- colnames(x)
    for(i in whichx)
      plot(x[,i], object$tx[,i],
           xlab=cn[i], ylab=paste('Transformed', cn[i]), ...) 
    invisible()
}
