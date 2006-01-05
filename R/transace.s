transace <- function(x, monotonic=NULL, categorical=NULL, binary=NULL,
                     pl=TRUE)
{
  if(.R.)
    require('acepack')  # provides ace, avas

  nam <- dimnames(x)[[2]]
  omit <- is.na(x %*% rep(1,ncol(x)))
  omitted <- (1:nrow(x))[omit]
  if(length(omitted)) x <- x[!omit,]
  p <- ncol(x)
  xt <- x  # binary variables retain original coding
  if(!length(nam))
    stop("x must have column names")
  
  rsq <- rep(NA, p)
  names(rsq) <- nam


  for(i in (1:p)[!(nam %in% binary)]) {
    lab <- nam[-i]
    w <- 1:(p-1)
    im <- w[lab %in% monotonic]
    ic <- w[lab %in% categorical]
    if(nam[i] %in% monotonic)
      im <- c(0, im)

    if(nam[i] %in% categorical)
      ic <- c(0, ic)
    m <- 10*(length(im)>0)+(length(ic)>0)
    if(m==11)
      a <- ace(x[,-i], x[,i], mon=im, cat=ic)
    else if (m==10)
      a <- ace(x[,-i], x[,i], mon=im)
    else if(m==1)
      a <- ace(x[,-i], x[,i], cat=ic)
    else
      a <- ace(x[,-i], x[,i])

    xt[,i] <- a$ty
    rsq[i] <- a$rsq
    if(pl)
      plot(x[,i], xt[,i], xlab=nam[i], ylab=paste("Transformed",nam[i]))
  }

  cat("R-squared achieved in predicting each variable:\n\n")
  print(rsq)

  attr(xt, "rsq") <- rsq
  attr(xt, "omitted") <- omitted
  invisible(xt)
}


areg.boot <- function(x, y, data, weights, subset, na.action=na.delete,
                      B = 100, method=c('avas','ace'), evaluation=100, 
                      valrsq=TRUE, probs=c(.25,.5,.75),...)
{
  acall   <- match.call()
  method  <- match.arg(method)
  if(.R.)
    require('acepack')  # provides ace, avas

  ## Temporarily fix bug in ace
  if(.R.) {
    ace <- function (x, y, wt = rep(1, nrow(x)), cat = NULL, mon = NULL, 
                     lin = NULL, circ = NULL, delrsq = 0.01) 
    {
      x <- as.matrix(x)
      if (delrsq <= 0) {
        cat("delrsq must be positive")
        return()
      }

      iy <- ncol(x) + 1
      l <- matrix(1, ncol = iy)
      if (length(circ)) {
        for (i in 1:length(circ)) {
          if (circ[i] < 0 || circ[i] > ncol(x)) {
            cat("bad circ= specification")
            return()
          }

          if (circ[i] == 0) {
            cat("response spec can only be lin or ordered (default)")
            return()
          }
          else {
            nncol <- circ[i]
            if (l[nncol] != 2 & l[nncol] != 1) {
              cat("conflicting transformation specifications")
              return()
            }

            l[nncol] <- 2
          }
        }
      }

      if (length(mon)) {
        for (i in 1:length(mon)) {
          if (mon[i] < 0 || mon[i] > ncol(x)) {
            cat("bad mon= specification")
            return()
          }

          if (mon[i] == 0) {
            cat("response spec can only be lin or ordered (default)")
            return()
          } else {
            nncol <- mon[i]
            if (l[nncol] != 3 && l[nncol] != 1) {
              cat("conflicting transformation specifications")
              return()
            }

            l[nncol] <- 3
          }
        }
      }

      if (length(lin)) {
        for (i in 1:length(lin)) {
          if (lin[i] < 0 || lin[i] > ncol(x)) {
            cat("bad lin= specification")
            return()
          }

          if (lin[i] == 0) {
            nncol <- iy
          } else {
            nncol <- lin[i]
          }
          if (l[nncol] != 4 && l[nncol] != 1) {
            cat("conflicting transformation specifications")
            return()
          }

          l[nncol] <- 4
        }
      }

      if (length(cat)) {
        for (i in 1:length(cat)) {
          if (cat[i] < 0 || cat[i] > ncol(x)) {
            cat("bad cat= specification")
            return()
          }
          if (cat[i] == 0) {
###                cat("response spec can only be lin or ordered (default)")
###                return()
          } else {
            nncol <- cat[i]
            if (l[nncol] != 4 && l[nncol] != 1) {
              cat("conflicting transformation specifications")
              return()
            }
            l[nncol] <- 4
          }
        }
      }

      tx <- x
      ty <- y
      m <- matrix(0, nrow = nrow(x), ncol = iy)
      z <- matrix(0, nrow = nrow(x), ncol = 12)
      z <- as.matrix(z)
      ns <- 1
      mode(x) <- "double"
      mode(y) <- "double"
      mode(tx) <- "double"
      mode(ty) <- "double"
      mode(wt) <- "double"
      mode(delrsq) <- "double"
      mode(z) <- "double"
      junk <- .Fortran("mace", p = as.integer(ncol(x)), n = as.integer(nrow(x)), 
                       x = t(x), y = y, w = as.double(wt), l = as.integer(l), 
                       delrsq = delrsq, ns = as.integer(ns), tx = tx, ty = ty, 
                       rsq = double(1), ierr = integer(1), m = as.integer(m), 
                       z = z, PACKAGE = "acepack")
      return(junk)
    }
  }

  
  categorical <- NULL
  linear  <- NULL
  mono    <- NULL

  if(inherits(x,'formula')) {
    ##nam <- attr(terms.inner(x),'term.labels')  2Apr01
    ## terms.inner will cause I(), monotone() wrappers to be ignored
    nam <- var.inner(x)

    m <- match.call(expand = FALSE)
    Terms <- terms(x, specials=c('I','monotone'))  # 2Apr01
    m$formula <- x
    m$x <- m$y <- m$B <- m$method <- m$evaluation <- m$valrsq <- m$probs <- 
      m$... <- NULL
    m$na.action <- na.action
    
    m[[1]] <- as.name("model.frame")
    x <- eval(m, sys.parent())
    k <- length(x)
    p <- k - 1
    nact <- attr(x,"na.action")
    ## Terms <- terms(x, specials=c('I','monotone'))  # 2Apr01
    
    linear <- attr(Terms,'specials')$I
    if(length(linear))
      linear <- linear - 1  ## y is pos. 0 for avas,ace
    
    mono <- attr(Terms,'specials')$monotone
    if(length(mono)) {
      mono <- mono - 1
      if(method=='avas' && any(mono==0)) 
        stop('y is always monotone with method="avas"')
    }
    ##attr(Terms, "formula") <- formula

    ylab <- as.character(attr(Terms,'variables')[if(.R.)2 else 1]) #2Apr01
    xbase <- 'x'
    weights <- model.extract(x, weights)
    cat.levels <- values <- vector('list',k)
    names(cat.levels) <- names(values) <- c(ylab,nam)

    for(j in 1:k) {
      xj <- x[[j]]
      if(is.character(xj)) {
        xj <- as.factor(xj)
        cat.levels[[j]] <- lev <- levels(xj)
        x[[j]] <- as.integer(xj)
        categorical <- c(categorical, j-1)
        values[[j]] <- 1:length(lev)
        if(method=='avas' && j==1)
          stop('categorical y not allowed for method="avas"')
      } else if(is.category(xj)) {
        cat.levels[[j]] <- lev <- levels(xj)
        x[[j]] <- as.integer(xj)
        categorical <- c(categorical, j-1)
        values[[j]] <- 1:length(lev)
        if(method=='avas' && j==1)
          stop('categorical y not allowed for method="avas"')
      } else {
        xj <- oldUnclass(xj) # 5Mar01
        xu <- sort(unique(xj))
        nu <- length(xu)
        if(nu < 3)
          linear <- unique(c(linear, j-1))

        values[[j]] <- if(nu <= length(probs)) xu else quantile(xj,probs)
      }
    }
    y <- x[,1]
    x <- as.matrix(x[,-1])
  } else {
    nact <- values <- NULL
    if(missing(weights))
      weights <- NULL
    
    ylab <- deparse(substitute(y))
    xbase <- deparse(substitute(x))
    x <- as.matrix(x)
    nam <- dimnames(x)[[2]]
    p <- ncol(x)
    if(!length(nam))
      nam <- if(p==1)xbase
             else paste(xbase,1:p,sep='')

    omit <- is.na(y + (x %*% rep(1, ncol(x))))
    if(any(omit)) {
      warning(paste(sum(omit),'observations with NAs deleted'))
      x <- x[!omit,,drop=FALSE]
      y <- y[!omit]
    }
  }

  n <- length(y)

  if(length(weights)==0)
    weights <- rep(1,n)

  fcall <- call(method, as.name('x'), as.name('y'), as.name('weights'))
  if(.R.) {  # 2Apr01
    if(length(mono))
      fcall$mon <- mono
    
    if(length(linear))
      fcall$lin <- linear
    
    if(length(categorical))
      fcall$cat <- categorical
  } else {
    if(length(mono))
      fcall$monotone <- mono
    
    if(length(linear))
      fcall$linear <- linear
    
    if(length(categorical))
      fcall$categorical <- categorical
  }

  f <- eval(fcall)
  rsquared.app <- f$rsq

  k <- p + 1
  f.orig <- lm.fit.qr.bare(f$tx, f$ty)
  coef.orig <- f.orig$coefficients
  names(coef.orig) <- cnam <- c('Intercept',nam)
  lp <- f$ty - f.orig$residuals
  ##  bar <- rep(0, k)
  ##  cov <- matrix(0, nrow=k, ncol=k, dimnames=list(cnam, cnam))

  trans <- cbind(f$ty,f$tx)
  Xo <- cbind(y, x)
  xlim <- apply(Xo, 2, range)
  xlim[,1] <- range(trans[,1])   # 26Feb00
  nam <- c(ylab, nam)
  fit <- vector('list',k)
  names(fit) <- nam
  neval <- rep(evaluation, k)
  for(i in 1:k) {
    if(length(categorical) && ((i-1) %in% categorical)) neval[i] <- xlim[2,i]
    ## Note: approx will return NAs even when rule=3 if x coordinate
    ## contains duplicates, so sort by x and remove dups (fctn in Misc.s)
    fit[[i]] <-
      if(i==1)
        approxExtrap(trans[,1],y,
                     xout=seq(xlim[1,i],xlim[2,i],length=neval[i]))
      else
        approxExtrap(Xo[,i], trans[,i],
                     xout=seq(xlim[1,i],xlim[2,i],length=neval[i]))
  }

  ## 14may02  was:
  ##      fit[[i]] <- approx(if(i==1) xySortNoDupNoNA(trans[,1], y) else
  ##                         xySortNoDupNoNA(Xo[,i], trans[,i]), 
  ##                        xout=seq(xlim[1,i],xlim[2,i],length=neval[i]), rule=3)

  if(max(neval) > evaluation) 
    stop('evaluation must be >= # levels of categorical predictors')

  boot <- array(NA, c(evaluation,B,k), list(NULL,NULL,nam))
  coefs <- matrix(NA, nrow=B, ncol=k, dimnames=list(NULL,cnam))

  optimism <- 0

  fcall <-
    if(.R.)
      parse(text=paste(method,'(x[s,],y[s],weights[s]',
                       if(length(mono))',mon=mono',
                       if(length(linear))',lin=linear',
                       if(length(categorical))',cat=categorical',
                       ')',sep=''))
    else
      parse(text=paste(method,'(x[s,],y[s],weights[s]',
                       if(length(mono))',monotone=mono',
                       if(length(linear))',linear=linear',
                       if(length(categorical))',categorical=categorical',
                       ')',sep=''))

  ## could not figure out how to get eval like first one to 
  ## evaluate subscripted expressions

  nfail <- 0 # 2Apr01
  for(b in 1:B) {
    cat(b,'')
    s <- sample(n, n, rep = TRUE)
    g <- eval(fcall)
    if(!all(is.finite(g$tx))) {  # 2Apr01
      nfail <- nfail + 1
      next
    }

    f.ols <- lm.fit.qr.bare(g$tx, g$ty)
    cof <- f.ols$coefficients
    coefs[b,] <- cof
    ##	bar <- bar + cof
    ##	cov <- cov + cof %*% t(cof)

    X <- cbind(y,x)[s,]
    trans <- cbind(g$ty, g$tx)
    for(i in 1:k)
      boot[1:neval[i],b,i] <-
        if(i==1) approxExtrap(trans[,1],X[,1],
                              xout=seq(xlim[1,i],xlim[2,i],length=neval[i]))$y
        else
          approxExtrap(X[,i], trans[,i],
                       xout=seq(xlim[1,i],xlim[2,i],
                                length=neval[i]))$y
    ##      boot[1:neval[i],b,i] <-    14may02
    ##        approx(if(i==1)xySortNoDupNoNA(trans[,1], X[,1]) else   # 26Feb00
    ##               xySortNoDupNoNA(X[,i], trans[,i]),
    ##               xout=seq(xlim[1,i],xlim[2,i],
    ##                 length=neval[i]), rule=3)$y

    if(valrsq) {
      rsq.boot <- f.ols$rsquared
      yxt.orig <- matrix(NA,nrow=n,ncol=k)
      for(i in 1:k)
        yxt.orig[,i] <- approxExtrap(X[,i],trans[,i],xout=Xo[,i])$y

      ##approx(xySortNoDupNoNA(X[,i], trans[,i]),  14may02
      ##								  xout=Xo[,i], rule=3)$y

      yt.hat <- cbind(1,yxt.orig[,-1]) %*% cof
      yt <- yxt.orig[,1]
      resid <- yt - yt.hat
      yt <- yt[!is.na(resid)]
      resid <- resid[!is.na(resid)]
      m <- length(resid)
      sst <- sum((yt - mean(yt))^2)
      sse <- sum(resid^2)
      rsquare <- 1 - sse/sst
      optimism <- optimism + rsq.boot - rsquare
    }
  }
  if(nfail > 0)
    warning(paste(method,'failed to converge in',
                  nfail,'resamples'))  ## 2Apr01

  ## bar <- bar/B
  ## cov <- (cov - B * bar %*% t(bar)) / (B-1)
  rsq.val <- if(valrsq) rsquared.app - optimism/(B-nfail)  ##nfail 2Apr01

  structure(list(call=acall, method=method, 
                 coefficients=coef.orig, ##var=cov,
                 linear.predictors=if(.R.)lp
                                   else as.single(lp),
                 fitted.values=approxExtrap(fit[[1]],xout=lp)$y,  ##14may02
                 residuals=if(.R.)f.orig$residuals
                           else as.single(f.orig$residuals),
                 na.action=nact, fit=fit, n=n,
                 linear=linear, categorical=categorical, monotone=mono,
                 cat.levels=cat.levels, values=values,
                 rsquared.app=rsquared.app,rsquared.val=rsq.val,
                 boot=boot,coef.boot=coefs,nfail=nfail), class='areg.boot')
}


print.areg.boot <- function(x, ...)
{
  cat("\n")
  cat(x$method,"Additive Regression Model\n\n")
  dput(x$call)
  cat("\n")

  if(length(x$categorical)) cat('Categorical variables:',
                                paste(names(x$fit)[x$categorical+1],
                                      collapse=' '),'\n\n')

  if(length(x$nfail) && x$nfail > 0)   ## 2Apr01
    cat('\n',x$method,' failed to converge in ',
        x$nfail,' resamples\n\n',sep='')

  if(length(z <- x$na.action)) naprint(z)

  cat('n=',x$n,'  p=',length(x$fit)-1,
      '\n\nApparent R2 on transformed Y scale:',round(x$rsquared.app,3))
  if(length(x$rsquared.val))
    cat('\nBootstrap validated R2            :',round(x$rsquared.val,3))

  cat('\n\nCoefficients of standardized transformations:\n\n')
  print(x$coefficients)
  res <- x$residuals
  rq <- c(quantile(res), mean(res), sqrt(var(res)))
  names(rq) <- c("Min", "1Q", "Median", "3Q", "Max", "Mean", "S.D.")
  cat("\n\nResiduals on transformed scale:\n\n")
  print(rq)
  cat('\n')
  invisible()
}


summary.areg.boot <- function(object, conf.int=.95, values, adj.to,
                              statistic='median',q=NULL, ...)
{
  scall   <- match.call()
  fit <- object$fit
  Boot <- object$boot
  Values <- object$values
  if(!missing(values)) Values[names(values)] <- values
  if(length(Values)==0) stop('summary does not work when first argument to areg.boot was not a formula')

  nfail <- object$nfail  # 2Apr01
  if(!length(nfail)) nfail <- 0
  
  res <- object$residuals
  
  Adj.to <- sapply(Values, function(y)median(1*y)) # 12May00 - handles logicals
  names(Adj.to) <- names(Values)   # median adds .50% in R
  if(!missing(adj.to))
    Adj.to[names(adj.to)] <- adj.to

  zcrit <- qnorm((1+conf.int)/2)
  k <- length(fit)
  p <- k - 1
  B <- dim(Boot)[2]
  nam <- names(fit)
  coef.orig <- object$coefficients
  coefs <- object$coef.boot
  trans.orig.y <- fit[[1]]
  ytransseq <- trans.orig.y[[1]]

  ## The next 2 loops are required because it takes an extra step to compute 
  ## the linear predictor at all predictor adjust-to settings, not just jth
  ## Get predicted transformed y with all variables set to adj. values
  pred.ty.adj <- double(p)
  for(j in 2:k) {
    namj <- nam[j]
    trans.orig <- fit[[namj]]
    pred.ty.adj[j-1] <- coef.orig[j] *
      approxExtrap(trans.orig, xout=Adj.to[namj])$y
    ## approx(trans.orig, xout=Adj.to[namj], rule=3)$y 14may02
  }

  ## For each bootstrap rep compute term summarizing the contribution
  ## of the jth predictor, evaluated at the adj. value, to predicting
  ## the transformed y, using only transformations from that boot. rep.
  boot.adj <- matrix(NA, nrow=B, ncol=p)
  for(j in 2:k) {
    namj <- nam[j]
    adjj <- Adj.to[namj]
    bootj <- Boot[,,j]
    xt <- fit[[namj]]$x
    for(i in 1:B) {
      bootji <- bootj[,i]
      s <- !is.na(bootji)
      ## is.na added 3Apr01
      if(!is.na(coefs[i,j])) 
        boot.adj[i, j-1] <- coefs[i,j]*approxExtrap(xt[s], bootji[s],
                                                    xout=adjj)$y  ## 14may02
    }
  }
  
  ## Now for each predictor compute differences in the chosen
  ## statistical parameter for the original scale of predicted y

  boot.y <- Boot[,,1]
  R <- vector('list',p)
  names(R) <- nam[-1]

  for(j in 2:k) {
    namj <- nam[j]
    xv <- Values[[namj]]
    trans.orig <- fit[[namj]]
    pred.term <- coef.orig[j]*approxExtrap(trans.orig, xout=xv)$y # 14may02
    pred.ty <- coef.orig[1] + sum(pred.ty.adj[-(j-1)]) + pred.term
    ##	pred.y <- approx(trans.orig.y$y, trans.orig.y$x, xout=pred.ty,rule=3)$y
    pred.y <- smearingEst(pred.ty, trans.orig.y, res,
                          statistic=statistic, q=q)
    lab <- attr(pred.y,'label')
    diff.pred <- pred.y[-1] - pred.y[1]

    ## For the same variable (j) repeat this over bootstrap reps

    sumd <- sumd2 <- rep(0, length(xv)-1)
    bootj <- Boot[,,j]
    xt <- trans.orig$x
    b <- 0
    bmiss <- 0
    for(i in 1:B) {
      if(is.na(coefs[i,j]))
        next   ## From avas/ace failure

      bootji <- bootj[,i]
      s <- !is.na(bootji)
      pred.term <- coefs[i,j]*approxExtrap(xt[s],bootji[s], xout=xv)$y #14may02
      if(any(is.na(pred.term))) {
        bmiss <- bmiss+1
        next
      }
      pred.ty <- coefs[i,1] + sum(boot.adj[i,-(j-1)]) + pred.term
      s <- !is.na(boot.y[,i])
      ##	  pred.y <- approx(boot.y[s,i], trans.orig.y$x[s], xout=pred.ty,rule=3)$y
      pred.y <- smearingEst(pred.ty, list(x=ytransseq,y=boot.y[,i]), res,
                            statistic=statistic, q=q)
      if(any(is.na(pred.y))) {
        bmiss <- bmiss+1
        next
      }

      b <- b + 1
      dp <- pred.y[-1] - pred.y[1]
      sumd <- sumd + dp
      sumd2 <- sumd2 + dp*dp
    }

    if(b < B)
      warning(paste('For',bmiss,'bootstrap samples a predicted value for one of the settings for',namj,'\ncould not be computed.  These bootstrap samples ignored.\nConsider using less extreme predictor settings.\n'))

    sediff <- sqrt((sumd2 - sumd*sumd/b)/(b-1))
    r <- cbind(c(0,  diff.pred), c(NA, sediff),
               c(NA, diff.pred-zcrit*sediff),
               c(NA, diff.pred+zcrit*sediff),
               c(NA, diff.pred/sediff),
               c(NA, 2*(1-pnorm(abs(diff.pred/sediff)))))
    cl <- object$cat.levels[[namj]]
    dimnames(r) <- list(x=if(length(cl))cl
                          else format(xv),
                        c('Differences','S.E',paste('Lower',conf.int),
                          paste('Upper',conf.int),"Z","Pr(|Z|)"))

    R[[j-1]] <- r
  }

  if(nchar(lab) > 10)
    lab <- substring(lab, 1, 10)

  structure(list(call=scall, results=R, adj.to=Adj.to, label=lab,
                 B=B, nfail=nfail, bmiss=bmiss),
            class='summary.areg.boot')
}


print.summary.areg.boot <- function(x, ...)
{
  R <- x$results
  adj.to <- x$adj.to
  nam <- names(R)
  dput(x$call)

  cat('\nEstimates based on', x$B-x$nfail-x$bmiss, 'resamples\n\n')

  cat('\n\nValues to which predictors are set when estimating\neffects of other predictors:\n\n')
  print(adj.to)

  cat('\nEstimates of differences of effects on',x$label,'Y (from first X value),\nand bootstrap standard errors of these differences.\nSettings for X are shown as row headings.\n')
  for(j in 1:length(nam)) {
    cat('\n\nPredictor:',nam[j],'\n')
    print(R[[j]])
  }

  invisible()
}


plot.areg.boot <- function(x, ylim, boot=TRUE,
                           col.boot=2, lwd.boot=.15, conf.int=.95,
                           ...)
{
  fit <- x$fit
  Boot <- x$boot
  k <- length(fit)
  B <- dim(Boot)[2]
  nam <- names(fit)
  boot <- if(is.logical(boot)) (if(boot) B
                                else 0)
          else min(boot, B)
  
  mfr <- par('mfrow')
  if(!length(mfr) || max(mfr) == 1) {
    mf <-
      if(k<=2)c(1,2)
      else if(k<=4)c(2,2)
      else if(k<=6)c(2,3)
      else if(k<=9)c(3,3)
      else if(k<=12)c(3,4)
      else if(k<=16) c(4,4)
      else c(4,5)

    oldmfrow <- par(mfrow=mf,err=-1)
    on.exit(par(oldmfrow))
  }

  Levels <- x$cat.levels
  for(i in 1:k) {
    fiti <- fit[[i]]
    if(i==1)
      fiti <- list(x=fiti[[2]], y=fiti[[1]])

    xx <- fiti[[1]]
    y <- fiti[[2]]
    lx <- length(xx)
    booti <- Boot[,,i]
    yl <- if(!missing(ylim))
      ylim
    else {
      rbi <- quantile(booti,c(.01,.99),na.rm=TRUE)
      if(i==1)
        range(approxExtrap(fiti, xout=rbi)$y)
      else range(rbi) #14may02
    }

    levi <- Levels[[i]]
    plot(xx, y, ylim=yl,
         xlab=nam[i], ylab=paste('Transformed',nam[i]), type='n', lwd=3,
         axes=length(levi)==0)
    if(ll <- length(levi)) {
      mgp.axis(2, pretty(yl))
      mgp.axis(1, at=1:ll, labels=levi)
    }

    if(boot>0)
      for(j in 1:boot) {
        if(i==1) {
          if(any(is.na(booti[1:lx,j]))) next
          lines(xx, approxExtrap(fiti, xout=booti[1:lx,j])$y,   # 14may02
                col=col.boot, lwd=lwd.boot)
        }
        else
          lines(xx, booti[1:lx,j], col=col.boot, lwd=lwd.boot)  # 5Mar01
      }

    if(!(is.logical(conf.int) && !conf.int)) {
      quant <- apply(booti[1:lx,],1,quantile,
                     na.rm=TRUE,probs=c((1-conf.int)/2, (1+conf.int)/2))
      if(i==1) {
        lines(xx, approxExtrap(fiti, xout=quant[1,])$y, lwd=2)  # 14may02
        lines(xx, approxExtrap(fiti, xout=quant[2,])$y, lwd=2)  # 14may02
      } else {
        lines(xx, quant[1,], lwd=2)
        lines(xx, quant[2,], lwd=2)
      }
    }

    lines(xx, fiti[[2]], lwd=3)
  }

  invisible()
}


Function.areg.boot <- function(object, type=c('list','individual'),
                               ytype=c('transformed','inverse'),
                               prefix='.', suffix='', frame=0,
                               where=1, ...)
{
  type <- match.arg(type)
  ytype <- match.arg(ytype)
  if(missing(type) && !(missing(prefix) & missing(suffix) &
                        missing(frame) & missing(where)))
    type <- 'individual'

  fit <- object$fit
  k <- length(fit)
  nam <- names(fit)
  g <- vector('list',k)
  catg <- object$categorical
  catl <- object$cat.levels
  names(g) <- nam
  for(i in 1:k) {
    if(length(catg) && ((i-1) %in% catg)) {
      if(i==1 && ytype=='inverse')
        stop('currently does not handle ytype="inverse" when y is categorical')

      h <- function(x, trantab)
      {
        if(is.category(x)) x <- as.character(x)
        trantab[x]
      }

      w <- fit[[i]]$y
      names(w) <- catl[[nam[i]]]
      formals(h) <- list(x=numeric(0), trantab=w)
    } else {
      h <- function(x, trantab)
      {
        s <- !is.na(x)
        res <- rep(NA, length(x))
        res[s] <- approxExtrap(trantab, xout=x[s])$y  # 14may02
        res
      }

      fiti <- fit[[i]]
      formals(h) <- list(x=numeric(0),
                         trantab=if(i==1 && ytype=='transformed')
                                   list(x=fiti[[2]],y=fiti[[1]])
                                 else fiti)
    }

    g[[i]] <- h
  }

  if(type=='list')
    return(g)

  fun.name <- paste(prefix, nam, suffix, sep='')
  for(i in 1:k)
    if(missing(where))
      assign(fun.name[i], g[[i]], frame=frame)
    else if(.R.)
      assign(fun.name[i], g[[i]], pos=where)
    else
      assign(fun.name[i], g[[i]], where=where)

  invisible(fun.name)
}


predict.areg.boot <- function(object, newdata, 
                              statistic=c('lp','median','quantile','mean',
                                          'fitted','terms'), q=NULL,
                              ...)
{
  if(!is.function(statistic))
    statistic <- match.arg(statistic)

  fit  <- object$fit
  fity <- fit[[1]]
  res  <- object$residuals
  if(missing(newdata)) {
    if(statistic=='terms')
      stop('statistic cannot be "terms" when newdata is omitted')

    lp <- object$linear.predictors
    y <- smearingEst(lp, fity, res, statistic=statistic, q=q)
    nac <- object$na.action
    return(if(length(nac)) nafitted(nac, y)
           else y)
  }
  
  cof <- object$coefficients
  Fun <- Function(object)
  nam <- names(fit)
  p <- length(nam)-1
  X <- matrix(NA, nrow=length(newdata[[1]]), ncol=p)
  for(i in 1:p) {
    nami <- nam[i+1]
    X[,i] <- Fun[[nami]](newdata[[nami]])
  }

  if(!is.function(statistic) && statistic=='terms')
    return(X)

  lp <- matxv(X, cof)
  smearingEst(lp, fity, res, statistic=statistic, q=q)
}


monotone <- if(!.SV4.)
  function(x) structure(x, class = unique(c("monotone",
                                            attr(x,'class')))) else
function(x) structure(x, class='monotone')
## SV4 can't handle multiple inheritance.  The above gets rid
## of e.g. "imputed" class

Mean <- function(object, ...) UseMethod("Mean")
Quantile <- function(object, ...) UseMethod("Quantile")


Mean.areg.boot <- function(object, evaluation=200, ...)
{
  r <- range(object$linear.predictors)
  lp <- seq(r[1], r[2], length=evaluation)
  res <- object$residuals
  ytrans <- object$fit[[1]]
  asing <- if(.R.) function(x)x else as.single

  if(length(lp)*length(res) < 100000)
    means <- asing(smearingEst(lp, ytrans, res, statistic='mean'))
  else {
    means <- if(.R.)double(evaluation)
             else single(evaluation)
    for(i in 1:evaluation)
      means[i] <- mean(approxExtrap(ytrans, xout=lp[i]+res)$y) # 14may02
  }

  g <- function(lp, trantab) approxExtrap(trantab, xout=lp)$y  # 14may02

  formals(g) <- list(lp=numeric(0),
                     trantab=list(x=if(.R.)lp
                                    else asing(lp),
                                  y=means))
  g
}


Quantile.areg.boot <- function(object, q=.5, ...)
{
  if(length(q) != 1 || is.na(q))
    stop('q must be length 1 and not NA')
  
  g <- function(lp, trantab, residualQuantile)
    approxExtrap(trantab, xout=lp+residualQuantile)$y  # 14may02

  formals(g) <- list(lp=numeric(0), trantab=object$fit[[1]],
                     residualQuantile <- quantile(object$residuals, q))
  g
}


smearingEst <- function(transEst, inverseTrans, res,
                        statistic=c('median','quantile','mean','fitted','lp'),
                        q=NULL)
{
  if(is.function(statistic))
    label <- deparse(substitute(statistic))
  else {
    statistic <- match.arg(statistic)
    switch(statistic,
           median = {statistic <- 'quantile'; q <- .5; label <- 'Median'},
           quantile = {
             if(!length(q))
               stop('q must be given for statistic="quantile"');
             
             label <- paste(format(q),'quantile')
           },
           mean = {
             statistic <- mean;
             label <- 'Mean'
           },
           fitted = {
             label <- 'Inverse Transformation'
           },
           lp = {
             label <- 'Transformed'
           })
  }
  y <- if(is.function(statistic)) {
    if(is.list(inverseTrans)) apply(outer(transEst, res,
                                          function(a, b, ytab) approxExtrap(ytab, xout=a+b)$y,  # 14may02
                                          inverseTrans), 1, statistic)
    else
      apply(outer(transEst, res, function(a, b, invfun)invfun(a+b),
                  inverseTrans), 1, statistic)
  } else switch(statistic,
                lp = transEst,
                fitted = if(is.list(inverseTrans)) approxExtrap(   # 14may02
                                                                inverseTrans,
                                                                xout=transEst)$y
                         else
                           inverseTrans(transEst),
                quantile = if(is.list(inverseTrans)) approxExtrap( # 14may02
                                                                  inverseTrans,
                                                                  xout=transEst+quantile(res,q))$y
                           else
                             inverseTrans(transEst+quantile(res,q)))
  structure(y, class='labelled', label=label)
}
