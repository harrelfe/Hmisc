transace <- function(formula, trim=0.01, data=environment(formula)) {
  if (! requireNamespace("acepack", quietly = TRUE))
    stop("This function requires the 'acepack' package.")

  monotonic <- categorical <- linear <- NULL
  trms <- terms(formula, data=data,
                specials=c('monotone', 'categorical', 'linear'))
  nam  <- all.vars(formula)
  s <- attr(trms, 'specials')
  mono <- s$monotone
  if(length(mono)) monotonic   <- nam[mono]
  catg <- s$categorical
  if(length(catg)) categorical <- nam[catg]
  lin  <- s$linear
  if(length(lin))  linear      <- nam[lin]
  if(is.environment(data)) data <- as.list(data)
  x <- data[nam]
  if(! is.data.frame(x)) x <- as.data.frame(x)
  
  Levels <- list()
  nam <- names(x)
  for(n in nam) {
    z                     <- x[[n]]
    if(is.character(z)) z <- as.factor(z)
    if(is.factor(z)) {
      if(n %nin% c(monotonic, linear))
        categorical         <- unique(c(categorical, n))
      Levels[[n]]         <- levels(z)
      z                   <- as.integer(z)
    }
    else {
      u <- unique(z[! is.na(z)])
      if(length(u) < 3) {
        u           <- sort(u)
        linear      <- unique(c(linear, n))
        z           <- 1 * (z == u[length(u)])
        Levels[[n]] <- u
      }
    }
    x[[n]] <- z
  }

  x <- as.matrix(x)
  if(is.character(x)) stop('transace requires a numeric matrix x')
  
  omit <- is.na(x %*% rep(1, ncol(x)))
  omitted <- (1 : nrow(x))[omit]
  if(length(omitted)) x <- x[! omit, ]
  p  <- ncol(x)
  xt <- x  # linear variables retain original coding
  if(!length(nam)) stop("x must have column names")
  
  rsq        <- rep(NA, p)
  names(rsq) <- nam

  Tr        <- vector('list', p)
  names(Tr) <- nam
  Trim      <- list()
  for(i in (1 : p)[! (nam %in% linear)]) {
    n   <- nam[i]
    lab <- nam[-i]
    w <- 1 : (p - 1)
    im <- if(any(lab %in% monotonic))   w[lab %in% monotonic]
    ic <- if(any(lab %in% categorical)) w[lab %in% categorical]
    il <- if(any(lab %in% linear))      w[lab %in% linear]
    if(n %in% monotonic)   im <- c(0, im)
    if(n %in% categorical) ic <- c(0, ic)
    a  <- acepack::ace(x[, - i], x[, i], cat=ic, mon=im, lin=il)
    j  <- ! duplicated(a$y)
    y  <- a$y [j]
    ty <- a$ty[j]
    j  <- order(y)
    y  <- y[j]
    ty <- ty[j]
    if(trim != 0.0 && n %nin% c(linear, categorical)) {
      ylim      <- quantile(a$y,  c(trim, 1. - trim))
      tylim     <- quantile(a$ty, c(trim, 1. - trim))
      Trim[[n]] <- list(ylim=ylim, tylim=tylim)
      }
    Tr[[n]] <- list(y=y, ty=ty)
    xt[,i]  <- a$ty
    rsq[i]  <- a$rsq
  }

  type <- structure(rep('general', length(nam)), names=nam)
  type[monotonic]   <- 'monotone'
  type[categorical] <- 'categorical'
  type[linear]      <- 'linear'
  res <- list(formula=formula, n=nrow(x),
              rsq=rsq, omitted=omitted, trantab=Tr, levels=Levels,
              trim=trim, limits=Trim, transformed=xt, type=type)
  class(res) <- 'transace'
  res
}

print.transace <- function(x, ...) {
  cat('\nTransformations Using Alternating Conditional Expectation\n\n')
  print(x$formula)
  cat('\nn=', x$n)
  if(length(x$omitted))
    cat(length(x$omitted), 'observations deleted due to NAs\n')
  cat('\n\nTransformations:\n\n')
  print(x$type, quote=FALSE)
  cat("\n\nR-squared achieved in predicting each variable:\n\n")
  print(x$rsq, digits=3)
  invisible()
  }
 
ggplot.transace <- function(data, mapping, ..., environment, nrow=NULL) {
  w      <- data$trantab
  trim   <- data$trim
  limits <- data$limits
  if(length(trim) && trim != 0.0)
    for(n in names(w)) {
      z   <- w[[n]]
      lim <- limits[[n]]
      if(length(lim)) {
        ylim  <- lim$ylim
        tylim <- lim$tylim
        j     <- z$y  >= ylim [1] & z$y  <= ylim [2] &
                 z$ty >= tylim[1] & z$ty <= tylim[2]
        z$y [! j] <- NA
        z$ty[! j] <- NA
        w[[n]] <- z
      }
    }
                   
  w  <- data.table::rbindlist(w, idcol='v')
  ggplot(w, aes(x=.data$y, y=.data$ty))  + geom_line() +
       facet_wrap(~ .data$v, scales='free', nrow=nrow) +
       xlab(NULL) + ylab('Transformed')
  }

areg.boot <- function(x, data, weights, subset, na.action=na.delete,
                      B = 100, method=c('areg','avas'), nk=4, evaluation=100, 
                      valrsq=TRUE, probs=c(.25,.5,.75),
                      tolerance=NULL)
{
  acall   <- match.call()
  method  <- match.arg(method)
  ## if(method=='avas') require(acepack)

  if(!inherits(x,'formula')) stop('first argument must be a formula')

  m <- match.call(expand.dots = FALSE)
  Terms <- terms(x, specials=c('I','monotone'))
  m$formula <- x
  m$x <- m$B <- m$method <- m$evaluation <- m$valrsq <- m$probs <- 
    m$nk <- m$tolerance <- NULL
  m$na.action <- na.action
  
  m[[1]] <- as.name("model.frame")
  x <- eval(m, sys.parent())

  nam <- unique(var.inner(Terms))
  ylab <- names(x)[1]

  k <- length(x)
  p <- k - 1
  nact <- attr(x,"na.action")
  
  default <- if(nk==0)'l' else 's'
  xtype <- rep(default, p); ytype <- default
  names(xtype) <- nam
  linear <- attr(Terms,'specials')$I
  if(length(linear)) {
    if(any(linear==1)) ytype <- 'l'
    if(any(linear>1 )) xtype[linear-1] <- 'l'
  }
  mono <- attr(Terms,'specials')$monotone
  if(length(mono)) {
    if(method=='avas' && any(mono==1))
      stop('y is always monotone with method="avas"')
    if(method=='areg') stop('monotone not implemented by areg')
    xtype[mono-1] <- 'm'
  }

  xbase <- 'x'
  weights <- model.extract(x, weights)
  cat.levels <- values <- vector('list',k)
  names(cat.levels) <- names(values) <- c(ylab,nam)

  for(j in 1:k) {
    typ <- ' '
    xj <- x[[j]]
    if(is.character(xj)) {
      xj <- as.factor(xj)
      cat.levels[[j]] <- lev <- levels(xj)
      x[[j]] <- as.integer(xj)
      typ <- 'c'
      values[[j]] <- 1:length(lev)
    } else if(is.factor(xj)) {
      cat.levels[[j]] <- lev <- levels(xj)
      x[[j]] <- as.integer(xj)
      typ <- 'c'
      values[[j]] <- 1:length(lev)
      if(method=='avas' && j==1)
        stop('categorical y not allowed for method="avas"')
    } else {
      xj <- unclass(xj) # 5Mar01
      xu <- sort(unique(xj))
      nu <- length(xu)
      if(nu < 3) typ <- 'l'
      values[[j]] <- if(nu <= length(probs)) xu else quantile(xj,probs)
    }
    if(typ != ' ') {
      if(j==1) ytype <- typ else xtype[j-1] <- typ
    }
  }

  y <- x[,1]
  x <- x[,-1,drop=FALSE]
  n <- length(y)

  if(length(weights)) stop('weights not implemented for areg') else
   weights <- rep(1,n)

 if(method=='areg')
   {
    f <- areg(x, y, xtype=xtype, ytype=ytype,
              nk=nk, na.rm=FALSE, tolerance=tolerance)
    rsquared.app <- f$rsquared
  }
 else
   {
     if (!requireNamespace("acepack", quietly = TRUE))
       stop("The 'avas' method requires the 'acepack' package.")
     
     Avas <- function(x, y, xtype, ytype, weights)
       {
         p <- ncol(x)
         types <- c(ytype, xtype)
         mono  <- (0:p)[types == 'm']
         lin   <- (0:p)[types == 'l']
         categ <- (0:p)[types == 'c'] 
         acepack::avas(x, y, weights, cat=categ, mon=mono, lin=lin)
       }
     f <- Avas(x, y, xtype, ytype, weights)
     rsquared.app <- f$rsq
   }

  f.orig <- lm.fit.qr.bare(f$tx, f$ty)
  coef.orig <- f.orig$coefficients
  names(coef.orig) <- cnam <- c('Intercept',nam)
  lp <- f$ty - f.orig$residuals

  trans <- cbind(f$ty,f$tx)
  Xo <- cbind(y, x)
  xlim <- apply(Xo, 2, range)
  xlim[,1] <- range(trans[,1])
  nam <- c(ylab, nam)
  fit <- vector('list',k)
  names(fit) <- nam
  neval <- rep(evaluation, k)
  for(i in 1:k) {
    iscat <- if(i==1) ytype=='c' else xtype[i-1]=='c'
    if(i > 1 && iscat) neval[i] <- xlim[2,i]   # i > 1 2024-03-14
    if(neval[i] != round(neval[i])) stop('program logic error')
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

  if(max(neval) > evaluation) 
    stop('evaluation must be >= # levels of categorical predictors')

  boot <- array(NA, c(evaluation,B,k), list(NULL,NULL,nam))
  coefs <- matrix(NA, nrow=B, ncol=k, dimnames=list(NULL,cnam))

  optimism <- 0

  nfail <- 0
  for(b in 1:B) {
    cat(b,'\r')
    s <- sample(n, n, replace = TRUE)
    g <- if(method=='areg')
      areg(x[s,,drop=FALSE], y[s], xtype=xtype, ytype=ytype, nk=nk,
           na.rm=FALSE, tolerance=tolerance) else
      Avas(x[s,,drop=FALSE], y[s], xtype=xtype, ytype=ytype,
           weights=weights[s])
    if(!all(is.finite(g$tx))) {
      nfail <- nfail + 1
      next
    }

    f.ols <- lm.fit.qr.bare(g$tx, g$ty)
    cof <- f.ols$coefficients
    coefs[b,] <- cof

    X <- Xo[s,]
    trans <- cbind(g$ty, g$tx)
    # May have to improve code for y=categorical to use only observed levels in xout
    for(i in 1:k)
      boot[1:neval[i],b,i] <-
        if(i==1) approxExtrap(trans[,1],X[,1],
                              xout=seq(xlim[1,i],xlim[2,i],length=neval[i]))$y
        else
          approxExtrap(X[,i], trans[,i],
                       xout=seq(xlim[1,i],xlim[2,i],
                                length=neval[i]))$y
    
    if(valrsq) {
      rsq.boot <- f.ols$rsquared
      yxt.orig <- matrix(NA,nrow=n,ncol=k)
      for(i in 1:k)
        yxt.orig[,i] <- approxExtrap(X[,i],trans[,i],xout=Xo[,i])$y


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
  cat('\n')
  if(nfail > 0)
    warning(paste(method,'failed to converge in',
                  nfail,'resamples'))
  
  rsq.val <- if(valrsq) rsquared.app - optimism/(B-nfail)

  structure(list(call=acall, method=method, 
                 coefficients=coef.orig,
                 linear.predictors=lp,
                 fitted.values=approxExtrap(fit[[1]],xout=lp)$y,
                 residuals=f.orig$residuals,
                 na.action=nact, fit=fit, n=n, nk=nk,
                 xtype=xtype, ytype=ytype,
                 xdf=f$xdf, ydf=f$ydf,
                 cat.levels=cat.levels, values=values,
                 rsquared.app=rsquared.app,rsquared.val=rsq.val,
                 boot=boot, coef.boot=coefs, nfail=nfail), class='areg.boot')
}


print.areg.boot <- function(x, ...)
{
  cat("\n")
  cat(x$method,"Additive Regression Model\n\n")
  dput(x$call)
  cat("\n")

  xinfo <- data.frame(type=x$xtype, row.names=names(x$xtype))
  if(length(x$xdf)) xinfo$d.f. <- x$xdf
  cat('\nPredictor Types\n\n')
  print(xinfo)
  cat('\ny type:', x$ytype)
  if(length(x$ydf)) cat('\td.f.:', x$ydf)
  cat('\n\n')

  if(length(x$nfail) && x$nfail > 0)
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

  nfail <- object$nfail
  if(!length(nfail)) nfail <- 0
  
  res <- object$residuals
  
  Adj.to <- sapply(Values, function(y)median(1*y))
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
                                                    xout=adjj)$y
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
    pred.term <- coef.orig[j]*approxExtrap(trans.orig, xout=xv)$y
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
      pred.term <- coefs[i,j]*approxExtrap(xt[s],bootji[s], xout=xv)$y
      if(any(is.na(pred.term))) {
        bmiss <- bmiss+1
        next
      }
      pred.ty <- coefs[i,1] + sum(boot.adj[i,-(j-1)]) + pred.term
      s <- !is.na(boot.y[,i])
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
               c(NA, 2 * pnorm(- abs(diff.pred/sediff))))
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

  cat('\nEstimates of differences of effects on',x$label,'Y (from first X\nvalue), and bootstrap standard errors of these differences.\nSettings for X are shown as row headings.\n')
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

    oldmfrow <- par('mfrow', 'err')
    par(mfrow=mf, err=-1)
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
      rbi <- quantile(booti,c(.025,.975),na.rm=TRUE)
      if(i==1)
        range(approxExtrap(fiti, xout=rbi)$y)
      else range(rbi)
    }

    levi <- Levels[[i]]
    plot(xx, y, ylim=yl,
         xlab=nam[i], ylab=paste('Transformed',nam[i]), type='n', lwd=2,
         axes=length(levi)==0)
    if(ll <- length(levi)) {
      mgp.axis(2, pretty(yl))
      mgp.axis(1, at=1:ll, labels=levi)
    }

    if(boot>0)
      for(j in 1:boot) {
        if(i==1) {
          if(any(is.na(booti[1:lx,j]))) next
          lines(xx, approxExtrap(fiti, xout=booti[1:lx,j])$y,
                col=col.boot, lwd=lwd.boot)
        }
        else
          lines(xx, booti[1:lx,j], col=col.boot, lwd=lwd.boot)
      }

    if(!(is.logical(conf.int) && !conf.int)) {
      quant <- apply(booti[1:lx,],1,quantile,
                     na.rm=TRUE,probs=c((1-conf.int)/2, (1+conf.int)/2))
      if(i==1) {
        lines(xx, approxExtrap(fiti, xout=quant[1,])$y, lwd=1.5)
        lines(xx, approxExtrap(fiti, xout=quant[2,])$y, lwd=1.5)
      } else {
        lines(xx, quant[1,], lwd=1.5)
        lines(xx, quant[2,], lwd=1.5)
      }
    }

    lines(xx, fiti[[2]], lwd=2)
  }

  invisible()
}


Function.areg.boot <-
  function(object, type=c('list','individual'),
           ytype=c('transformed','inverse'),
           prefix='.', suffix='', pos=-1, ...)
{
  type <- match.arg(type)
  ytype <- match.arg(ytype)
  if(missing(type) && !(missing(prefix) & missing(suffix) &
                        missing(pos)))
    type <- 'individual'

  fit <- object$fit
  k <- length(fit)
  nam <- names(fit)
  g <- vector('list',k)
  xtype <- object$xtype
  typey <- object$ytype

  catl <- object$cat.levels
  names(g) <- nam
  for(i in 1:k) {
    typ <- if(i==1) typey else xtype[i-1]
    if(typ=='c') {
      if(i==1 && ytype=='inverse')
        stop('currently does not handle ytype="inverse" when y is categorical')

      h <- function(x, trantab)
      {
        if(is.factor(x)) x <- as.character(x)
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
        res[s] <- approxExtrap(trantab, xout=x[s])$y
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
        assign(fun.name[i], g[[i]], pos=pos)
  invisible(fun.name)
}


predict.areg.boot <-
  function(object, newdata, 
           statistic=c('lp','median','quantile','mean',
             'fitted','terms'), q=NULL, ...)
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
    return(if(length(nac)) naresid(nac, y)   ## FEH30Aug09 was nafitted
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


monotone <- function(x) structure(x, class = unique(c("monotone",
                                       attr(x,'class'))))

Mean <- function(object, ...) UseMethod("Mean")
Quantile <- function(object, ...) UseMethod("Quantile")


Mean.areg.boot <- function(object, evaluation=200, ...)
{
  r <- range(object$linear.predictors)
  lp <- seq(r[1], r[2], length=evaluation)
  res <- object$residuals
  ytrans <- object$fit[[1]]
  asing <- function(x)x

  if(length(lp)*length(res) < 100000)
    means <- asing(smearingEst(lp, ytrans, res, statistic='mean'))
  else {
    means <- double(evaluation)
    for(i in 1:evaluation)
      means[i] <- mean(approxExtrap(ytrans, xout=lp[i]+res)$y)
  }

  g <- function(lp, trantab) approxExtrap(trantab, xout=lp)$y

  formals(g) <- list(lp=numeric(0),
                     trantab=list(x=lp,
                                  y=means))
  g
}


Quantile.areg.boot <- function(object, q=.5, ...)
{
  if(length(q) != 1 || is.na(q))
    stop('q must be length 1 and not NA')
  
  g <- function(lp, trantab, residualQuantile)
    approxExtrap(trantab, xout=lp+residualQuantile)$y

  formals(g) <- list(lp=numeric(0), trantab=object$fit[[1]],
                     residualQuantile = quantile(object$residuals, q))
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
    if(is.list(inverseTrans))
      apply(outer(transEst, res,
                  function(a, b, ytab) approxExtrap(ytab, xout=a+b)$y,
                   inverseTrans), 1, statistic) else
    apply(outer(transEst, res, function(a, b, invfun)invfun(a+b),
                inverseTrans), 1, statistic)
  } else switch(statistic,
                lp = transEst,
                fitted = if(is.list(inverseTrans))
                approxExtrap(
                             inverseTrans,
                             xout=transEst)$y else
                           inverseTrans(transEst),
                quantile = if(is.list(inverseTrans))
                approxExtrap(
                             inverseTrans,
                             xout=transEst+quantile(res,q))$y else
                inverseTrans(transEst+quantile(res,q)))
  structure(y, class='labelled', label=label)
}

utils::globalVariables(c('ty'))
