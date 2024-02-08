Ecdf <- function(x, ...) UseMethod('Ecdf')


Ecdf.default <- function(x, what=c('F','1-F','f','1-f'), 
                         weights=rep(1, length(x)), normwt=FALSE,
                         xlab, ylab, q, pl=TRUE, add=FALSE, lty=1,
                         col=1, group=rep(1, length(x)), 
                         label.curves=TRUE, xlim, subtitles=TRUE, 
                         datadensity=c('none','rug','hist','density'), 
                         side=1, 
                         frac=switch(datadensity,
                                     none=NA,rug=.03,hist=.1,density=.1),
                         dens.opts=NULL, lwd=1, log='', ...)
{
  datadensity <- match.arg(datadensity)
  what        <- match.arg(what)
  colspec <- FALSE
  if(datadensity != 'none') {
    if(side %in% c(2,4))
      stop('side must be 1 or 3 when datadensity is specified')
    
    if('frac' %nin% names(dens.opts))
      dens.opts$frac <- frac
    
    if('side' %nin% names(dens.opts))
      dens.opts$side <- side
    
    if('col' %in%   names(dens.opts))
      colspec <- TRUE
  }
  
  if(missing(xlab))
    xlab <- label(x, units=TRUE, plot=TRUE, default=deparse(substitute(x)))
  
  what <- match.arg(what)
  if(missing(ylab)) ylab <- switch(what,
                                   'F'='Proportion <= x',
                                   '1-F'='Proportion > x',
                                   'f'='Frequency <= x')
  
  group <- as.factor(group)
  group <- group[drop=TRUE]
  if(length(x) != length(group))
    stop('length of x != length of group')

  nna <- !(is.na(x) | is.na(group) | is.na(weights))
  
  X <- x[nna]
  group <- group[nna]

  lev <- levels(group)
  nlev <- length(lev)
  curves <- vector('list',nlev)
  names(curves) <- lev

  lty <- rep(lty, length.out=nlev)
  col <- rep(col, length.out=nlev)
  lwd <- rep(lwd, length.out=nlev)

  if(missing(xlim)) xlim <- range(X)

  n <- if(normwt) length(X) else sum(weights[nna])
  
  m <- (if(normwt) length(nna) else sum(weights, na.rm=TRUE)) - n
  
  weights <- weights[nna]

  for(i in 1:nlev) {
    s <- group == lev[i]
    x <- X[s]
    wt <- weights[s]
    xorig <- x
    
    z <- wtd.Ecdf(x, wt, type='i/n', normwt=normwt, na.rm=FALSE)
    x <- z$x; y <- z$ecdf
    switch(what,
           '1-F' = {y <- 1 - y},
           'f'   = {y <- y * sum(wt)},
           '1-f' = {x <- x[-1]
                    y <- as.vector((1 - y[- length(y)]) * sum(wt)) } )
    
    if(pl) {
      if(i==1 && !add)
        plot(x, y, xlab=xlab, ylab=ylab, xlim=xlim, type='n', log=log, ...)
      
      lines(x, y, type="s", lty=lty[i], col=col[i], lwd=lwd[i])
      if(subtitles && i == 1) {
        pm <- paste("n:", n, " m:", m, sep="")
        title(sub=pm, adj=0, cex=.5)
      }
      
      if(!missing(q)) {
        if(what == '1-f') stop('what="1-f" not yet implemented with q')
        if(what=='f') q <- q * y[length(y)]
        else
          if(what == '1-F') q <- 1 - q
        q <- switch(what,
                    'f'   = q * sum(wt),
                    '1-F' = 1 - q,
                    'F'   = q)
        
        a <- par("usr")
        for(w in q) {
          quant <-
            if(what=='1-F') min(x[y <= w]) else min(x[y >= w])
          
          lines(c(a[1],  quant), c(w, w),    lty=2, col=1)
          lines(c(quant, quant), c(w, a[3]), lty=2, col=col[i])
        }
      }
    }
    
    curves[[i]] <- list(x=x, y=y)
    if(datadensity!='none') {
      if(!colspec)
        dens.opts$col <- col[i]
      
      do.call(switch(datadensity, 
                     rug    = 'scat1d',
                     hist   = 'histSpike',
                     density= 'histSpike'),
              c(list(x=xorig, add=TRUE),
                if(datadensity=='density') list(type='density'),
                dens.opts))
    }
  }

  if(nlev > 1 && (is.list(label.curves) || label.curves))
    labcurve(curves, type='s', lty=lty, col.=col, opts=label.curves)
  
  invisible(structure(if(nlev==1) list(x = x, y = y) else curves, 
                      N=list(n=n, m=m)))
}


Ecdf.data.frame <- function(x, group=rep(1, nrows), 
                            weights=rep(1,nrows), normwt=FALSE,
                            label.curves=TRUE, n.unique=10, na.big=FALSE, 
                            subtitles=TRUE,  vnames=c("labels","names"),
                            ...)
{
  vnames <- match.arg(vnames)
  mf <- par('mfrow')
  if(length(mf) == 0)
    mf <- c(1, 1)
  
  g <- function(v, n.unique) {
    if(is.character(v) || is.factor(v))
      return(FALSE)
    
    length(unique(v[!is.na(v)])) >= n.unique
  }
  
  use <- sapply(x, g, n.unique=n.unique)
  automf <- FALSE
  if((la <- sum(use)) > 1 & max(mf) == 1) {
    mf <-
      if(la<=4) c(2,2)
      else if(la<=6) c(2,3)
      else if(la<=9) c(3,3)
      else if(la<=12)c(3,4)
      else if(la<=16)c(4,4)
      else           c(4,5)
    
    automf <- TRUE
  }
  
  oldmf <- par('mfrow')
  par(mfrow=mf)
  on.exit(par(oldmf))
  
  nam <- names(x)
  nrows <- nrow(x)
  i <- 0
  j <- 0

  group <- as.factor(group)
  
  for(j in (1 : length(x))[use]) {
    v <- x[[j]]
    i <- i + 1
    lab <- if(vnames == 'names') nam[j] else
    label(v, units=TRUE, plot=TRUE, default=nam[j])
    
    z <- Ecdf(v, group=group, weights=weights, normwt=normwt, 
              xlab=lab, label.curves=label.curves, 
              subtitles=subtitles, ...)
    if(na.big) {
      m <- attr(z, 'N')$m
      if(m > 0)
        mtext(paste(m,"NAs"), line=-2, cex=1)
    }
    
    if(automf && interactive() && 
       all(names(dev.list()) %nin% c('postscript','win.printer')) &&
       (i %% prod(mf)==0)) {
      cat("click left mouse button to proceed\n")
      locator(1)
    }
  }
  
  invisible(ceiling(sum(use) / prod(mf)))
}


prepanel.Ecdf <- function(x, y, fun, what, ...) {
  xlim <- range(x, na.rm=TRUE)
  l <- length(x[! is.na(x)])
  ylim <- switch(what,
                 F     = c(0, 1),
                 '1-F' = c(0, 1),
                 f     = c(0, l),
                 '1-f' = c(0, l))
  ylim <- fun(ylim)
  if(any(is.infinite(ylim))) ylim <- fun(c(.001, .999))
  list(xlim=xlim, ylim=ylim, dx=diff(xlim), dy=diff(ylim))
}


panel.Ecdf <- function(x, y, subscripts, groups=NULL, 
                       q=NULL, type='s',
                       method=c('i/n','(i-1)/(n-1)','i/(n+1)'), fun,
                       what = c('F', '1-F', 'f', '1-f'),
                       label.curves=TRUE, 
                       lwd = plot.line$lwd, 
                       lty = plot.line$lty,
                       pch = plot.symbol$pch, 
                       cex = plot.symbol$cex, 
                       font= plot.symbol$font, 
                       col = NULL, ...)
{
  sRequire('lattice')

  method <- match.arg(method)
  what   <- match.arg(what)
  if(length(groups)) groups <- as.factor(groups)

  type <- 's'   # lattice histogram sets to 'percent'

  g <- unclass(groups)[subscripts]
  ng <- if(length(groups)) max(g, na.rm=TRUE) else 1

  plot.symbol <- lattice::trellis.par.get(
    if(ng > 1) "superpose.symbol" else "plot.symbol")
  
  plot.line   <- lattice::trellis.par.get(
    if(ng > 1) "superpose.line" else "plot.line")

  qrefs <- function(x, q, col, fun, llines, grid) {
    quant <- quantile(x, probs=q, na.rm=TRUE)
    a <- parGrid(grid)$usr
    for(i in 1 : length(q)) {
      llines(c(a[1],     quant[i]), fun(c(q[i], q[i])), lty=2, col=1)
      llines(c(quant[i], quant[i]), fun(c(q[i], a[3])), lty=2, col=col)
    }
  }
  
  ppanel <- function(x, y, type, cex, pch, font, lwd, lty, col, q, 
                     qrefs, ecdf.type, fun=fun, what,
                     datadensity=c('none','rug','hist','density'), 
                     side=1, 
                     frac=switch(datadensity,
                       none=NA,
                       rug=.03,
                       hist=.1,
                       density=.1),
                     dens.opts=NULL, llines, ...) {

    ## y ignored
    z <- wtd.Ecdf(x, type=ecdf.type, na.rm=FALSE)
    zx <- z$x
    y  <- z$ecdf
    switch(what,
           '1-F' = {y <- 1 - y},
           'f'   = {y <- y * length(x)},
           '1-f' = {zx <- zx[-1]
             y <- as.vector((1 - y[- length(y)]) * length(x)) } )
    
    
    ## For some reason S-Plus will not plot anything the following way
    ## when lwd is a variable
    ##llines(z$x, fun(z$ecdf), lwd = lwd, lty = lty, col = col,
    ##       type = type, ...)
    do.call(llines,
            list(zx, fun(y), lwd = lwd, lty = lty, col = col,
                 type = type, ...))
    if(length(q))
      qrefs(x, q, col, fun=fun, llines=llines, grid=TRUE)
    
    datadensity <- match.arg(datadensity)
    if(datadensity != 'none') {
      if(side %in% c(2,4))
        stop('side must be 1 or 3 when datadensity is specified')
      
      if('frac' %nin% names(dens.opts))
        dens.opts$frac <- frac
      
      if('side' %nin% names(dens.opts))
        dens.opts$side <- side
      
      if('col'  %nin% names(dens.opts))
        dens.opts$col  <- col
      
      if('lwd'  %nin% names(dens.opts))
        dens.opts$lwd  <- lwd
      
      do.call(switch(datadensity, 
                     rug    ='scat1d',
                     hist='histSpike',
                     density='histSpike'),
              c(list(x=x, add=TRUE, grid=TRUE),
                if(datadensity == 'density')
                  list(type='density'),
                dens.opts))
    }
  }   # end ppanel
  
  pspanel <- function(x, subscripts, groups, type, lwd, lty,
                      pch, cex, font, col, q, qrefs, 
                      ecdf.type, fun, what, llines, ...) {

    ## y ignored
    lev <- levels(groups)
    groups <- as.numeric(groups)[subscripts]
    N <- seq(along = groups)
    curves <- list()
    
    for(i in 1:length(lev)) {
      which <- N[groups == i]
      ## sort in x
      j <- which # no sorting
      if(any(j)) {
        z <- wtd.Ecdf(x[j], type=ecdf.type, na.rm=FALSE)
        zx <- z$x
        y  <- z$ecdf
        switch(what,
               '1-F' = {y <- 1 - y},
               'f'   = {y <- y * length(x[j])},
               '1-f' = {zx <- zx[-1]
                 y <- as.vector((1 - y[- length(y)]) * length(x[j])) } )
        
        
        do.call(llines,
                list(zx, fun(y),
                     col = col[i], lwd = lwd[i], lty = lty[i], 
                     type = type, ...))
        if(length(q)) qrefs(x[j], q, col[i], fun=fun, llines=llines,
                            grid=TRUE)
        curves[[lev[i]]] <- list(x=zx, y=fun(y))
      }
    }
      
    curves
  }   # end pspanel
  
  lty  <- rep(lty, length = ng)
  lwd  <- rep(lwd, length = ng)
  pch  <- rep(pch, length = ng)
  cex  <- rep(cex, length = ng)
  font <- rep(font,length = ng)
  if(!length(col)) col <- plot.line$col

  col <- rep(col, length = ng)

  if(ng > 1) {
    levnum <- sort(unique(g))
    curves <- pspanel(x, subscripts, groups,
                      lwd=lwd, lty=lty, pch=pch, cex=cex, 
                      font=font, col=col, type=type, q=q, qrefs=qrefs, 
                      ecdf.type=method, fun=fun, what=what,
                      llines=lattice::llines)
    if(!(is.logical(label.curves) && !label.curves)) {
      lc <-
        if(is.logical(label.curves))
          list(lwd=lwd, cex=cex[1])
        else
          c(list(lwd=lwd, cex=cex[1]), label.curves)
      
      labcurve(curves, lty=lty[levnum], lwd=lwd[levnum], col.=col[levnum], 
               opts=lc, grid=TRUE, ...)
    }
  }
  else ppanel(x, lwd=lwd, lty=lty, pch=pch, cex=cex, 
              font=font, col=col, type=type, q=q, qrefs=qrefs, 
              ecdf.type=method, fun=fun, what=what, llines=lattice::llines, ...)

  if(ng > 1) { ##set up for key() if points plotted
    .Key <- function(x=0, y=1, lev, col, lty, lwd, ...)
      {
		oldpar <- par('usr', 'xpd')
        par(usr=c(0,1,0,1),xpd=NA)
        
        ## Even though par('usr') shows 0,1,0,1 after lattice draws
        ## its plot, it still needs resetting
        on.exit(par(oldpar))
        if(is.list(x))
          {
            y <- x[[2]]; x <- x[[1]]
          }
        
        if(!length(x)) x <- 0
        if(!length(y)) y <- 1  ## because of formals()
        rlegend(x, y, legend=lev, lty=lty, lwd=lwd, col=col)
        invisible()
      }
    
    
    formals(.Key) <- list(x=NULL, y=NULL, lev=levels(groups), col=col,
                          lty=lty, lwd=lwd,...=NULL)
    .setKey(.Key)
  }
}


Ecdf.formula <- function(x, data = sys.frame(sys.parent()), 
                         groups = NULL, 
                         prepanel=prepanel.Ecdf, panel=panel.Ecdf, ..., 
                         xlab, ylab, fun=function(x)x,
                         what=c('F', '1-F', 'f', '1-f'),
                         subset=TRUE)
{
  sRequire('lattice')
  what <- match.arg(what)
  vars <- all.vars(x)
  xname <- vars[1]
  if(missing(xlab))
    xlab <- label(eval(parse(text=xname), data),
                  units=TRUE, plot=TRUE, default=xname, grid=TRUE)
  if(missing(ylab)) 
    ylab <-
      if(missing(fun))
        paste(switch(what,
                     F = 'Proportion <=',
                     '1-F' = 'Proportion >=',
                     'f' = 'Number <=',
                     '1-f' = 'Number >='), xname)
      else ''
  
  subset <- eval(substitute(subset), data)

  lh <- lattice::histogram
  do.call(lh,
          c(list(x, data=data, prepanel=prepanel, panel=panel,
                 ylab=ylab, xlab=xlab, fun=fun, what=what),
            if(!missing(groups))
            list(groups=eval(substitute(groups), data)),
            if(!missing(subset))
            list(subset=subset),
            list(...)))
}
