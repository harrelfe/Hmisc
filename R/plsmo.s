plsmo <- function(x,y,method=c("lowess","supsmu","raw"),
		xlab,ylab,add=FALSE,lty=1:nlev,col=par('col'),lwd=par('lwd'),
        iter=if(length(unique(y))>2) 3 else 0, bass=0,
		trim, fun, group=rep(1,length(x)), prefix, xlim, ylim, 
		label.curves=TRUE,	datadensity=FALSE, lines.=TRUE,
		subset=TRUE, grid=FALSE, ...) {

  gfun <- ordGridFun(grid)
  nam <- as.character(sys.call())[2:3]
  method <- match.arg(method)
  if(!missing(subset)) {  ## 20jul02
    x <- x[subset]
    y <- y[subset]
    group <- group[subset]
  }
    
  group <- as.factor(group)
  if(!missing(prefix)) levels(group) <- paste(prefix,levels(group))
  group <- as.factor(group)
  nna <- !(is.na(x+y)|is.na(group))
  x <- x[nna]
  y <- y[nna]
  group <- group[nna]

  lev <- levels(group)
  nlev <- length(lev)
  curves <- vector('list',nlev)
  names(curves) <- lev

  xmin <- ymin <- 1e30; xmax <- ymax <- -1e30
  for(g in lev) {
	s <- group==g
	z <- switch(method, 
                lowess=lowess(x[s],y[s],iter=iter),
                supsmu=supsmu(x[s],y[s], bass=bass),
                raw=approx(x[s],y[s],xout=sort(unique(x[s]))))
    
	if(missing(trim))trim <- if(sum(s)>200) 10/sum(s) else 0
	if(trim>0 && trim<1) {
      xq <- quantile(x[s],c(trim,1-trim))
      s <- z$x>=xq[1] & z$x<=xq[2]
      z <- list(x=z$x[s],y=z$y[s])
    }
	if(!missing(fun)) {
	  yy <- fun(z$y)
	  s <- !is.infinite(yy) & !is.na(yy)   ## was is.inf 11Apr02
	  z <- list(x=z$x[s],y=yy[s])
	}
	curves[[g]] <- z
	xmin <- min(xmin, z$x); xmax <- max(xmax, z$x)
	ymin <- min(ymin, z$y); ymax <- max(ymax, z$y)
	}

	if(!add) {
      if(grid) stop('add=T not implemented under grid/lattice in R')
#	  if(missing(xlab)) xlab <- if(label(x)!='') label(x) else nam[1] 26sep02
#	  if(missing(ylab)) ylab <- if(label(y)!='') label(y) else nam[2]
      if(missing(xlab))
        xlab <- label(x, units=TRUE, plot=TRUE, default=nam[1])
      if(missing(ylab))
        ylab <- label(y, units=TRUE, plot=TRUE, default=nam[2])
	  plot(xmin,ymin,xlim=if(missing(xlim))c(xmin,xmax) else xlim,
		   ylim=if(missing(ylim))c(ymin,ymax) else ylim, type='n',
		   xlab=xlab, ylab=ylab)
	}
	lty <- rep(lty, length=nlev)
	col <- rep(col, length=nlev)
    if(missing(lwd) && is.list(label.curves) &&
       length(label.curves$lwd)) lwd <- label.curves$lwd  # 20Feb00
    lwd <- rep(lwd, length=nlev)

	if(lines.) for(i in 1:nlev)
      gfun$lines(curves[[i]], lty=lty[i], col=col[i], lwd=lwd[i])  # 20Feb00

	if(datadensity) {
	  for(i in 1:nlev) {
		s <- group==lev[i]
		x1 <- x[s]
		y.x1 <- approx(curves[[i]], xout=x1)$y
		scat1d(x1, y=y.x1, col=col[i], grid=grid, ...)
	  }
	}

	if((is.list(label.curves) || label.curves) && 
	   nlev>1 && (!missing(prefix) | !add | !missing(label.curves))) 
	  labcurve(curves, lty=lty, col=col, opts=label.curves, grid=grid)
	invisible(curves)
}



panel.plsmo <- function(x, y, subscripts, groups=NULL, type='b', 
						label.curves=TRUE,
						lwd = superpose.line$lwd, 
						lty = superpose.line$lty, 
						pch = superpose.symbol$pch, 
						cex = superpose.symbol$cex, 
						font = superpose.symbol$font, 
						col = NULL,...) {

  superpose.symbol <- trellis.par.get("superpose.symbol")
  superpose.line <- trellis.par.get("superpose.line")
  if(length(groups)) groups <- as.factor(groups)
  g <- oldUnclass(groups)[subscripts]
  ng <- if(length(groups)) max(g) else 1
  lty  <- rep(lty, length = ng)
  lwd  <- rep(lwd, length = ng)
  pch  <- rep(pch, length = ng)
  cex  <- rep(cex, length = ng)
  font <- rep(font, length = ng)
  if(!length(col)) col <- if(type=='p') superpose.symbol$col else
    superpose.line$col
  col <- rep(col, length = ng)
  lc <- if(is.logical(label.curves)) {
	if(label.curves) list(lwd=lwd, cex=cex[1]) else FALSE
  } else c(list(lwd=lwd, cex=cex[1]), label.curves)
  if(type!='p') if(ng > 1)
	plsmo(x, y, group=groups[subscripts,drop=FALSE], 
          add=TRUE, lty=lty, col=col, label.curves=lc, grid=.R., ...) else
    plsmo(x, y, add=TRUE, lty=lty, col=col, label.curves=lc, grid=.R.,
          ...)

  if(type!='l') {
	if(ng > 1) panel.superpose(x, y, subscripts,
                               if(.R.)as.integer(groups) else groups, 
							   lwd=lwd, lty=lty, pch=pch, cex=cex, 
							   font=font, col=col) else
	  panel.xyplot(x, y, 
				   lwd=lwd, lty=lty, pch=pch, cex=cex, 
				   font=font, col=col)
	if(ng > 1) {
      Key <- if(.R.) function(x=NULL, y=NULL, lev, cex, col, font, pch) {
        oldpar <- par(usr=c(0,1,0,1),xpd=NA)
        on.exit(par(oldpar))
        if(is.list(x)) { y <- x[[2]]; x <- x[[1]] }
        ## Even though par('usr') shows 0,1,0,1 after lattice draws
        ## its plot, it still needs resetting
        if(!length(x)) x <- 0
        if(!length(y)) y <- 1  ## because of formals()
        rlegend(x, y, legend=lev, cex=cex, col=col, pch=pch)
        invisible()
      } else function(x=NULL, y=NULL, lev, cex, col, font, pch, ...) {
		if(length(x)) {
		  if(is.list(x)) {y <- x$y; x <- x$x}
		  key(x=x, y=y, text=list(lev, col=col), 
			  points=list(cex=cex,col=col,font=font,pch=pch),
			  transparent=TRUE, ...) } else
		key(text=list(lev, col=col), 
			points=list(cex=cex,col=col,font=font,pch=pch),
            transparent=TRUE, ...)
        invisible()
      }
      formals(Key) <- list(x=NULL,y=NULL,lev=levels(groups), cex=cex,
                           col=col, font=font, pch=pch)
	  storeTemp(Key)
    }
  }
}
