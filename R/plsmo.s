plsmo <-
  function(x, y, method=c("lowess", "supsmu", "raw", "intervals"),
           xlab, ylab, add=FALSE, lty=1 : lc, col=par('col'),
           lwd=par('lwd'), iter=if(length(unique(y)) > 2) 3 else 0,
           bass=0, f=2 / 3, mobs=30, trim, fun, ifun=mean,
           group=rep(1, length(x)), prefix, xlim, ylim, 
           label.curves=TRUE, datadensity=FALSE, scat1d.opts=NULL,
           lines.=TRUE, subset=TRUE, grid=FALSE, evaluate=NULL, ...)
{
  gfun <- ordGridFun(grid)
  nam <- as.character(sys.call())[2 : 3]
  method <- match.arg(method)
  if(method == 'intervals')
    doint <- function(x, y, m, ifun, fun) {
      g <- cutGn(x, m=m, what='factor')
      if(length(levels(g)) < 2)
        stop(paste('number of observations not large enough for',
                   m, 'observations per interval'))
      w <- cutGn(x, m=m, what='cuts')
      p <- fun(tapply(y, g, ifun, na.rm=TRUE))
      seg1 <- list(x1=w[- length(w)], y1=p, x2=w[-1], y2=p)
      ne <- 2 : (length(w) - 1)
      seg2 <- list(x1=w[ne], y1=p[-1], x2=w[ne], y2=p[- length(p)])
      list(x = (w[-length(w)] + w[-1]) / 2, y = p,
           xbar = tapply(x, g, mean, na.rm=TRUE),
           seg1 = seg1, seg2=seg2)
    }
  
  if(missing(ylab))
    ylab <- label(y, units=TRUE, plot=TRUE, default=nam[2])
  Y <- as.matrix(y)
  p <- ncol(Y)
  if(!missing(subset)) {
    x     <- x[subset]
    Y     <- Y[subset,, drop=FALSE]
    group <- group[subset]
  }
    
  group <- as.factor(group)
  if(!missing(prefix)) levels(group) <- paste(prefix, levels(group))
  
  group <- as.factor(group)
  nna <- !(is.na(x) | (rowSums(is.na(Y)) == p) | is.na(group))
  x     <- x[nna]
  Y     <- Y[nna,, drop=FALSE]
  group <- group[nna]

  lev  <- levels(group)
  nlev <- length(lev)
  lc   <- p * nlev
  curves <- list()
  clev   <- rep('', lc)  # for each curve what is the level of group

  xmin <- ymin <- 1e30; xmax <- ymax <- -1e30
  ic <- 0
  for(k in 1:p) {
    y <- Y[, k]
    for(g in lev) {
      ic <- ic + 1
      s <- group == g
      z <- switch(method, 
                  lowess = lowess(x[s], y[s], iter=iter, f=f),
                  supsmu = supsmu(x[s], y[s], bass=bass),
                  raw    = approx(x[s], y[s], xout=sort(unique(x[s]))),
                  intervals = doint(x[s], y[s], m=mobs, ifun=ifun,
                    fun=if(missing(fun)) function(x) x else fun)
                  )

      if(missing(trim))
        trim <- if(sum(s) > 200) 10 / sum(s) else 0

      if(method == 'intervals') {
        trim <- 0
        evaluate <- NULL
      }
      
      if(trim > 0 && trim < 1) {
        xq <- quantile(x[s], c(trim, 1 - trim))
        s <- z$x >= xq[1] & z$x <= xq[2]
        z <- list(x=z$x[s], y=z$y[s])
      }
      
      if(length(evaluate)) {
        rx   <- range(z$x)
        xseq <- seq(rx[1], rx[2], length.out=evaluate)
        z <- approx(z, xout=xseq)
      }

      if(!missing(fun)) {
        yy <- fun(z$y)
        s <- !is.infinite(yy) & !is.na(yy)
        z <- list(x=z$x[s], y=yy[s])
      }
      
      clev[ic] <- g
      lab <- if(p == 1) g
       else if(nlev == 1 & p == 1) '1'
       else if(nlev == 1 & p > 1) colnames(Y)[k]
       else paste(colnames(Y)[k], g)

      curves[[lab]] <- z
      xmin <- min(xmin, z$x); xmax <- max(xmax, z$x)
      ymin <- min(ymin, z$y); ymax <- max(ymax, z$y)
    }
  }
  if(add) {
    if(missing(xlim))
      xlim <- if(grid) lattice::current.panel.limits()$xlim else par('usr')[1:2]
  }
  else {
    if(missing(xlab))
      xlab <- label(x, units=TRUE, plot=TRUE, default=nam[1])
 
    if(missing(xlim)) xlim <- if(method == 'intervals')
                                range(x, na.rm=TRUE) else c(xmin, xmax)
    if(missing(ylim)) ylim <- c(ymin, ymax)
    plot(xmin, ymin, xlim=xlim, ylim=ylim,
         type='n', xlab=xlab, ylab=ylab)
  }
  
  lty <- rep(lty, length.out=lc)
  col <- rep(col, length.out=lc)
  if(missing(lwd) && is.list(label.curves) && length(label.curves$lwd))
    lwd <- label.curves$lwd
  
  lwd <- rep(lwd, length.out=lc)

  if(method == 'intervals')
    for(i in 1 : lc) {
      cu   <- curves[[i]]
      seg1 <- cu$seg1
      seg2 <- cu$seg2
      acol <- adjustcolor(col[i], alpha.f=.15)
      gfun$points(cu$xbar, cu$y, col=acol, pch=3)
      with(cu$seg1, gfun$segments(x1, y1, x2, y2, col=col[i]))
      with(cu$seg2, gfun$segments(x1, y1, x2, y2, col=acol))
    }
  else {
    for(i in 1 : lc) {
      cu <- curves[[i]]
      s <- cu$x >= xlim[1] & cu$x <= xlim[2]
      curves[[i]] <- list(x=cu$x[s], y=cu$y[s])
    }
    if(lines.)
      for(i in 1 : lc)
        gfun$lines(curves[[i]], lty=lty[i], col=col[i], lwd=lwd[i])
    
    if(datadensity) {
      for(i in 1 : nlev) {
        s <- group == lev[i]
        x1 <- x[s]
        for(ii in which(clev == lev[i])) {
          y.x1 <- approx(curves[[ii]], xout=x1)$y
          sopts <- c(list(x=x1, y=y.x1, col=col[ii], grid=grid), scat1d.opts)
          do.call('scat1d', sopts)
        }
      }
    }
  }

  if((is.list(label.curves) || label.curves) && 
     lc > 1 && (!missing(prefix) | !add | !missing(label.curves))) 
    labcurve(curves, lty=lty, col.=col, opts=label.curves, grid=grid)
  
  invisible(curves)
}


panel.plsmo <- function(x, y, subscripts, groups=NULL, type='b', 
                        label.curves=TRUE,
                        lwd  = superpose.line$lwd, 
                        lty  = superpose.line$lty, 
                        pch  = superpose.symbol$pch, 
                        cex  = superpose.symbol$cex, 
                        font = superpose.symbol$font, 
                        col  = NULL, scat1d.opts=NULL, ...)
{
  sRequire('lattice')
  superpose.symbol <- lattice::trellis.par.get("superpose.symbol")
  superpose.line   <- lattice::trellis.par.get("superpose.line")
  if(length(groups)) groups <- as.factor(groups)
  
  g  <- unclass(groups)[subscripts]
  ng <- if(length(groups)) max(g) else 1
  
  lty  <- rep(lty, length = ng)
  lwd  <- rep(lwd, length = ng)
  pch  <- rep(pch, length = ng)
  cex  <- rep(cex, length = ng)
  font <- rep(font, length = ng)
  if(!length(col))
    col <- if(type == 'p') superpose.symbol$col else superpose.line$col
  
  col <- rep(col, length = ng)
  lc <-
    if(is.logical(label.curves)) {
      if(label.curves)
        list(lwd=lwd, cex=cex[1])
      else FALSE
    } else c(list(lwd=lwd, cex=cex[1]), label.curves)
  
  if(type != 'p') if(ng > 1)
    plsmo(x, y, group=groups[subscripts, drop=FALSE], 
          add=TRUE, lty=lty, col=col, label.curves=lc, grid=TRUE,
          scat1d.opts=scat1d.opts, ...)
  else
    plsmo(x, y, add=TRUE, lty=lty, col=col, label.curves=lc, grid=TRUE,
          scat1d.opts=scat1d.opts, ...)

  if(type != 'l') {
    if(ng > 1)
      lattice::panel.superpose(x, y, subscripts,
                      as.integer(groups),
                      lwd=lwd, lty=lty, pch=pch, cex=cex, 
                      font=font, col=col)
    else
      lattice::panel.xyplot(x, y, 
                   lwd=lwd, lty=lty, pch=pch, cex=cex, 
                   font=font, col=col)
    
    if(ng > 1) {
      Key <- function(x=NULL, y=NULL, lev, cex, col, font, pch){
		oldpar <- par('usr', 'xpd')
        par(usr=c(0, 1, 0, 1), xpd=NA)
        on.exit(par(oldpar))
        if(is.list(x)) {
          y <- x[[2]]
          x <- x[[1]]
        }
            
        ## Even though par('usr') shows 0,1,0,1 after lattice draws
        ## its plot, it still needs resetting
        if(!length(x))
          x <- 0
        
        if(!length(y))
          y <- 1  ## because of formals()
        
        rlegend(x, y, legend=lev, cex=cex, col=col, pch=pch)
        invisible()
      }
      
      formals(Key) <- list(x=NULL,y=NULL,lev=levels(groups), cex=cex,
                           col=col, font=font, pch=pch)
      .setKey(Key)
    }
  }
}
