utils::globalVariables("pch.to.use")

drawPlot <- function(..., xlim=c(0,1), ylim=c(0,1), xlab='', ylab='',
                     ticks=c('none','x','y','xy'),
                     key=FALSE, opts=NULL)
{

  Points <- function(label=' ', type=c('p','r'), n, pch=pch.to.use[1],
                     cex=par('cex'), col=par('col'),
                     rug=c('none','x','y','xy'),
                     ymean=NULL) {
    type <- match.arg(type)
    rug <- match.arg(rug)
    cat('\nClick mouse for each point',
        if(label!='')
          paste(' for group ',label), '.',
        if(missing(n))
          ' Right click when finished (Esc with RStudio).',
        '\n',sep='')
    
    pts <-
      if(missing(n))
        locator(type='p', pch=pch, cex=cex, col=col)
      else
        locator(n, type='p', pch=pch, cex=cex, col=col)
    
    if(length(ymean))
      pts$y <- pts$y - mean(pts$y) + ymean
    
    if(type=='p')
      assign("pch.to.use", pch.to.use[pch.to.use != pch],
             envir=environment(Points))
    else {
      scat1d(pts$x, side=1)
      pch <- NA
    }
    
    switch(rug,
           x = scat1d(pts$x, side=1),
           y = scat1d(pts$y, side=2),
           xy = {scat1d(pts$x, side=1); scat1d(pts$y, side=2)},
           none = )
    
    structure(list(points=pts, label=label, type=type,
                   pch=pch, cex=cex, col=col, rug=rug), class='Points')
  }
  
  Curve <- function(label=' ',
                    type=c('bezier','polygon','linear','pol','loess','step',
                           'gauss'),
                    n=NULL, lty=1, lwd=par('lwd'), col=par('col'),
                    degree=2, evaluation=100, ask=FALSE) {
    isfun <- is.function(type)
    if(! isfun) type <- match.arg(type)
    
    if(! isfun && ! length(n) && type == 'linear') n <- 2
    
    if(! isfun && type=='gauss') n <- 3
    
    xlim <- par('usr')[1 : 2]
    redraw <- TRUE
    
    if(isfun) {
      x <- seq(xlim[1], xlim[2], length=evaluation)
      pts <- list(x=as.numeric(x), y=as.numeric(type(x)))
      lines(pts, lty=lty, lwd=lwd, col=col)
    }
    else repeat {
      cat('\nClick mouse for each point',
          if(label!='')
            paste(' for group ',label),
          '.',
          if(!length(n))
            ' Right click when finished (Esc with RStudio).',
          '\n', sep='')
      
      pts <-
        if(!length(n))
          locator(type='l', lty=lty, lwd=lwd, col=col)
        else
          locator(n, type='l', lty=lty, lwd=lwd, col=col)
      
      n <- length(pts$x)
      if(n < 2) stop('must click at least 2 points')
      
      if(n==2) type <- 'linear'
      
      if(type=='pol') {
        x <- matrix(NA, nrow=n, ncol=degree)
        for(i in 1:degree) x[,i] <- pts$x^i
        f <- lm.fit.qr.bare(x, pts$y)
        x <- matrix(NA, nrow=evaluation, ncol=degree)
        x[,1] <- seq(min(pts$x),max(pts$x), length=evaluation)
        if(degree > 1)
          for(i in 1:degree) x[,i] <- x[,1]^i
        
        cof <- f$coefficients
        y <- cof[1] + x %*% cof[-1]
        pts <- list(x=as.numeric(x[,1]), y=as.numeric(y))
        if(redraw) lines(pts, lty=lty, lwd=lwd, col=col)
      }

      if(type == 'loess') {
        w <- lowess(pts$x, pts$y, f=.25)
        pts <- approx(w, xout=seq(min(pts$x), max(pts$x), length=evaluation))
        if(redraw) lines(pts, lty=lty, lwd=lwd, col=col)
      }
      
      if(type=='bezier') {
        pts <- bezier(pts, xlim=range(pts$x), evaluation=evaluation)
        if(redraw)
          lines(pts, lty=lty, lwd=lwd, col=col)
      }
      
      if(type=='gauss') {
        mu <- pts$x[2]
        delta <- diff(pts$x[-2])/2
        htavg <- sum(pts$y[-2])/2
        htmax <- pts$y[2]
        x <- seq(xlim[1], xlim[2], length=evaluation)
        b2 <- delta^2 / log(htmax/htavg)
        y <- htmax * exp(-(x-mu)^2/b2)
        i <- y > 1e-4
        pts <- list(x=as.single(x[i]), y=as.single(y[i]))
        lines(pts, lty=lty, lwd=lwd, col=col)
        }
      
      if(type=='step' && redraw)
        lines(pts, type='s', lty=lty, lwd=lwd, col=col)
      
      if(!ask) break
      
      if(readline('\nType y to accept, n to re-draw:') == 'y')
        break
    }
    
    structure(list(points=pts, label=label, type=type, lty=lty,
                   lwd=lwd, col=col),  class='Curve')
  }
  
  Abline <- function(...) {
    abline(...)
    structure(list(...), class='Abline')
  }
  
  environment(Points)$pch.to.use <- c(1,2,3,4,16,17,5,6,15,18,19)

  ticks <- match.arg(ticks)
  if(missing(ticks)) {
    if(!missing(xlim)) ticks <- 'x'
    
    if(!missing(ylim)) ticks <- 'y'
    
    if(!missing(xlim) && !missing(ylim)) ticks <- 'xy'
  }
  
  plot(xlim, ylim, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
       type='n', axes=ticks=='xy')
  
  switch(ticks,
         none={
           axis(1, at=xlim, labels=FALSE)
           axis(2, at=ylim, labels=FALSE)
         },
         x={
           axis(1)
           axis(2, at=ylim, labels=FALSE)
         },
         y={
           axis(1, at=xlim, labels=FALSE)
           axis(2)
         },
         xy = )

  cc <- as.list(substitute(list(...))[-1])
  W <- list()
  for(i in seq_along(cc)) W[[i]] <- eval(cc[[i]])
  ## See http://stackoverflow.com/questions/33737102/elegant-way-to-define-a-function-inside-another-function

  m <- length(W)
  type <- label <- rep('', m)
  lty <- lwd <- pch <- cex <- col <- rep(NA, m)
  curves <- vector('list', m)
  i <- 0
  for(j in 1:m) {
    w <- W[[j]]
    if(attr(w,'class')=='Abline')
      next
    
    i <- i + 1
    isfun <- is.function(w$type)
    curves[[i]] <- if(!key || isfun) w$points
    else switch(w$type,
                step = approx(w$points,
                  xout=seq(min(w$points$x), max(w$points$x), length=50),
                  method='constant', f=0),
                linear = approx(w$points,
                  xout=seq(min(w$points$x), max(w$points$x), length=50)),
                w$points)
    
    label[i] <- w$label
    col[i]   <- w$col
    type[i]  <- if(isfun) 'l'
    else switch(w$type,
                p   = 'p',
                r   = 'r',
                step= 's',
                'l')
    
    if(type[i]=='p') {
      pch[i] <- w$pch
      cex[i] <- w$cex
    }
    else if(type[i] != 'r') {
      lty[i] <- w$lty
      lwd[i] <- w$lwd
    }
  }
  
  if(i < m) {
    curves <- curves[1:i]
    label  <- label [1:i]
    type   <- type  [1:i]
    lty    <- lty   [1:i]
    lwd    <- lwd   [1:i]
    pch    <- pch   [1:i]
    cex    <- cex   [1:i]
    col    <- col   [1:i]
  }
  
  keyloc <- NULL
  j <- type != 'r'
  if(any(j)) {
    if(!key)
      labcurve(curves[j], labels=label[j], type=type[j],
               lty=lty[j], lwd=lwd[j], col.=col[j], opts=opts)
    else {
      x <- unlist(lapply(curves, function(z)z$x))
      y <- unlist(lapply(curves, function(z)z$y))
      keyloc <- putKeyEmpty(x, y, labels=label[j], type=type[j],
                            pch=pch[j], lty=lty[j],
                            lwd=lwd[j], cex=cex[j], col=col[j])
    }
  }
  
  invisible(structure(list(W, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim,
                           ticks=ticks, key=key, keyloc=keyloc, opts=opts),
                      class='drawPlot'))
}


bezier <- function(x, y, xlim, evaluation=100) {
  if(missing(y)) {
    y <- x[[2]]
    x <- x[[1]]
  }
  
  n <- length(x)
  X <- Y <- single(evaluation)
  Z <- seq(0, 1, length=evaluation)
  X[1] <- x[1];
  X[evaluation] <- x[n]
  Y[1] <- y[1];
  Y[evaluation] <- y[n]
  for(i in 2:(evaluation - 1)) {
    z <- Z[i]
    xz <- yz <- 0
    const <- (1 - z) ^ (n - 1)
    for(j in 0 : (n - 1)) {
      xz <- xz + const * x[j + 1]
      yz <- yz + const * y[j + 1]
      const <- const* (n - 1 - j) / (j + 1) * z / (1 - z)
      if(is.na(const)) prn(c(i, j, z))
    }
    
    X[i] <- xz; Y[i] <- yz
  }
  
  list(x=as.numeric(X), y=as.numeric(Y))
}


plot.drawPlot <- function(x, xlab, ylab, ticks,
                          key=x$key, keyloc=x$keyloc, ...)
{
  if(missing(xlab)) xlab <- x$xlab
  if(missing(ylab)) ylab <- x$ylab
  
  xlim <- x$xlim
  ylim <- x$ylim
  if(missing(ticks)) ticks <- x$ticks
  
  plot(xlim, ylim, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab,
       type='n', axes=ticks=='xy')
  switch(ticks,
         none={
           axis(1, at=xlim, labels=FALSE)
           axis(2, at=ylim, labels=FALSE)
         },
         x={
           axis(1)
           axis(2, at=ylim, labels=FALSE)
         },
         y={
           axis(1, at=xlim, labels=FALSE)
                 axis(2)
         },
         xy= )

  data <- x[[1]]
  m <- length(data)
  type <- label <- rep('', m)
  lty <- lwd <- pch <- cex <- col <- rep(NA, m)
  curves <- vector('list', m)
  i <- 0
  for(j in 1 : m) {
    w <- data[[j]]
    if(attr(w, 'class') == 'Abline') {
      do.call("abline", unclass(w))
      next
    }
    
    i <- i + 1
    if(is.function(w$type)) w$type <- 'l'
    
    curves[[i]] <-
      if(!key)
        w$points
      else switch(w$type,
                  step = approx(w$points,
                                xout=seq(min(w$points$x),max(w$points$x),
                                  length=50),
                                method='constant', f=0),
                  linear = approx(w$points,
                                  xout=seq(min(w$points$x),max(w$points$x),
                                    length=50)),
                  w$points)
    
    label[i] <- w$label
    col[i]   <- w$col
    switch(attr(w, 'class'),
           Points = {
             type[i] <- w$type
             pch[i] <- w$pch
             cex[i] <- w$cex
             switch(w$type,
                    p = points(w$points, cex=w$cex, pch=w$pch, col=w$col),
                    r = scat1d(w$points$x, side=1, col=w$col))
             switch(w$rug,
                    x = scat1d(w$points$x, side=1, col=w$col),
                    y = scat1d(w$points$y, side=2, col=w$col),
                    xy = {
                      scat1d(w$points$x, side=1, col=w$col)
                      scat1d(w$points$y, side=2, col=w$col)
                    },
                    none = )
           },
           Curve = {
             type[i] <- if(w$type=='step') 's' else 'l'
             lty[i] <- w$lty
             lwd[i] <- w$lwd
             lines(w$points, lty=w$lty, lwd=w$lwd, col=col[i], type=type[i])
           })
  }

  if(i < m) {
    curves <- curves[1:i]
    label  <- label[1:i]
    type   <- type[1:i]
    pch    <- pch[1:i]
    lty    <- lty[1:i]
    lwd    <- lwd[1:i]
    cex    <- cex[1:i]
    col    <- col[1:i]
  }
  
  if(key && !length(keyloc))
    stop('you may not specify key=T unless key=T was specified to drawPlot or keyloc is specified to plot')

  if(any(label!='')) {
    j <- type!='r'
    if(any(j)) {
      if(key) putKey(keyloc, labels=label[j],
                     type=type[j], pch=pch[j],
                     lty=lty[j], lwd=lwd[j], cex=cex[j], col=col[j])
      else
        labcurve(curves[j], type=type[j],
                 lty=lty[j], lwd=lwd[j], col.=col[j],
                 labels=label[j], opts=x$opts)
    }
  }
  
  invisible()
}
