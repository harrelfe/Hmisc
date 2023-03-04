Cbind <- function(...)
{    # See llist function with Hmisc label function
  dotlist <- list(...)
  if(is.matrix(dotlist[[1]]))
    {
      y <- dotlist[[1]]
      ynam <- dimnames(y)[[2]]
      if(!length(ynam))
        stop('when first argument is a matrix it must have column dimnames')
      
      other <- y[,-1,drop= FALSE]
      return(structure(y[,1], class='Cbind', label=ynam[1], other=other))
    }
  
  lname <- names(dotlist)
  name <- vname <- as.character(sys.call())[-1]
  for(i in 1:length(dotlist))
    {
      vname[i] <- if(length(lname)) lname[i] else ''
      ## Added length() and '' 12Jun01, remove length(vname[i])==0 below
      if(vname[i]=='') vname[i] <- name[i]
    }

  lab <- attr(y <- dotlist[[1]], 'label')
  if(!length(lab)) lab <- vname[1]
  if(!is.matrix(other <- dotlist[[2]]) || ncol(other)<2)
    {
      other <- as.matrix(as.data.frame(dotlist))[,-1,drop= FALSE]
      dimnames(other)[[2]] <- vname[-1]
    }
  
  structure(y, class='Cbind', label=lab, other=other)
}

as.numeric.Cbind <- as.double.Cbind <- function(x, ...) x
## Keeps xyplot from stripping off "other" attribute in as.numeric


'[.Cbind' <- function(x, ...)
{
  structure(unclass(x)[...], class='Cbind',
            label=attr(x,'label'),
            other=attr(x,'other')[...,,drop= FALSE])
}


prepanel.xYplot <- function(x, y, ...)
{
  xlim <- range(x, na.rm=TRUE)
  ylim <- range(y, attr(y,'other'), na.rm=TRUE)
  list(xlim=xlim, ylim=ylim, dx=diff(xlim), dy=diff(ylim))
}


## MB add method="filled bands" 
## MB use col.fill to specify colors for filling bands
panel.xYplot <-
  function(x, y, subscripts, groups = NULL, 
           type = if(is.function(method) || method == "quantiles") "b"
                  else "p", 
           method = c("bars", "bands", "upper bars", "lower bars", 
                      "alt bars", "quantiles", "filled bands"), 
           methodArgs = NULL, label.curves = TRUE, abline, 
           probs = c(0.5, 0.25, 0.75), nx=NULL, cap = 0.015, lty.bar = 1, 
           lwd = plot.line$lwd, lty = plot.line$lty, 
           pch = plot.symbol$pch, cex = plot.symbol$cex, 
           font = plot.symbol$font, col = NULL, 
           lwd.bands = NULL, lty.bands = NULL, col.bands = NULL, 
           minor.ticks = NULL, col.fill = NULL,
           size=NULL, rangeCex=c(.5,3), ...)
{
  sRequire('lattice')
  if(missing(method) || !is.function(method))
    method <- match.arg(method)   # was just missing() 26Nov01

  type <- type   # evaluate type before method changes 9May01
  sizeVaries <- length(size) && length(unique(size)) > 1
  if(length(groups)) groups <- as.factor(groups)

  g <- as.integer(groups)[subscripts]
  ng <- if(length(groups)) max(g)
  else 1

  plot.symbol <- lattice::trellis.par.get(if(ng > 1) "superpose.symbol"
                                 else "plot.symbol")

  plot.line <- lattice::trellis.par.get(if(ng > 1) "superpose.line"
                               else "plot.line")

  lty <- rep(lty, length = ng)
  lwd <- rep(lwd, length = ng)
  if(length(rangeCex) != 1) pch <- rep(pch, length = ng)

  if(!sizeVaries) cex <- rep(cex, length = ng)

  font <- rep(font, length = ng)
  if(!length(col))
    col <- if(type == "p") plot.symbol$col
           else plot.line$col

  col <- rep(col, length = ng)
  pchVaries <- FALSE
  ## Thanks to Deepayan Sarkar for the following size code
  if(sizeVaries)
    {
      if(length(rangeCex) > 1)
        srng <- range(size, na.rm=TRUE)

      size <- size[subscripts]
      if(length(rangeCex)==1)
        {
          pch <- as.character(size)
          cex <- rangeCex
          sizeVaries <- FALSE
          pchVaries  <- TRUE
        }
      else
        {
          cex <- rangeCex[1] + diff(rangeCex)*(size - srng[1])/diff(srng)
          sKey <- function(x=0, y=1, cexObserved, cexCurtailed, col, pch,
                           other)
            {
              if(!length(x))
                x <- 0.05
              
              if(!length(y))
                y <- 0.95  ## because of formals()
              
              ## had to multiply cex by 1.4 when using rlegend instead of rlegendg
              rlegendg(x, y, legend=format(cexObserved), cex=cexCurtailed,
                       col=col, pch=pch, other=other)
              invisible()
            }
        
          formals(sKey) <- list(x=NULL, y=NULL, cexObserved=srng,
                                cexCurtailed=rangeCex,
                                col=col[1], pch=pch, other=NULL)
          .setsKey(sKey)
        }
    }

  other <- attr(y, "other")
  if(length(other))
    {
      nother <- ncol(other)
      if(nother == 1)
        {
          lower <- y - other
          upper <- y + other
        }
      else
        {
          lower <- other[, 1]
          upper <- other[, 2]
        }
    }
  else nother <- 0

  y <- unclass(y)
  levnum <- if(length(groups)) sort(unique(g))
  else 1

  if(is.function(method) || method == "quantiles")
    {
      if(!is.function(method))
        {
          method <- quantile  # above: methodArgs=NULL
          if(!length(methodArgs))
            methodArgs <- list(probs = probs)
        }
      
      if(length(methodArgs)) methodArgs$na.rm <- TRUE
      else
        methodArgs <- list(na.rm = TRUE)

    if(ng == 1)
      {
        if(!length(nx)) nx <- min(length(x)/4, 40)    
        xg <-
          if(nx)
            as.numeric(as.character(cut2(x, m = nx,
                                         levels.mean = TRUE)))
          else x
        
        dsum <- do.call("summarize",
                        c(list(y, llist(xg = xg), method, type = "matrix", 
                               stat.name = "Z"), methodArgs))
      }
    else
      {
        xg <- x
        if(missing(nx) || nx)
          for(gg in levnum)
            {
              w <- g == gg
              if(missing(nx)) nx <- min(sum(w)/4, 40)
              xg[w] <-
                as.numeric(as.character(cut2(xg[w], m = nx,
                                             levels.mean = TRUE)))
            }
        
        dsum <- do.call("summarize",
                        c(list(y, by = llist(g, xg),
                               method, type = "matrix", stat.name = "Z"), 
                          methodArgs))
        g <- dsum$g
        groups <- factor(g, 1:length(levels(groups)),
                         levels(groups))
        subscripts <- TRUE
      }

      x <- dsum$xg
      y <- dsum$Z[, 1, drop = TRUE]
      other <- dsum$Z[, -1, drop=FALSE]
      nother <- 2
      method <- "bands"
    }

  ## MB 04/17/01 default colors for filled bands
  ## 'pastel' colors matching superpose.line$col
  plot.fill <- c(9, 10, 11, 12, 13, 15, 7) 
  ## The following is a fix of panel.xyplot to work for type='b'
  ppanel <- function(x, y, type, cex, pch, font, lwd, lty, col, ...)
    {
      gfun <- ordGridFun(TRUE)
      if(type != 'p')
        gfun$lines(x, y, lwd = lwd, lty = lty, col = col, ...)
    
      if(type !='l')
        gfun$points(x=x, y=y,
                    pch = pch, font = font,
                    cex = cex, col = col, 
                    type = type, lwd=lwd, lty=lty, ...)
    }

  ##The following is a fix for panel.superpose for type='b' 
  pspanel <- function(x, y, subscripts, groups, type, lwd, lty, 
                      pch, cex, font, col, sizeVaries, pchVaries, ...)
    {
      gfun <- ordGridFun(TRUE)
    
      groups <- as.numeric(groups)[subscripts]
      N <- seq(along = groups)
      for(i in sort(unique(groups)))
        {
          which <- N[groups == i]	# j <- which[order(x[which])]	
										# sort in x
          j <- which	# no sorting
          if(type != "p")
            gfun$lines(x[j], y[j],
                       col = col[i], lwd = lwd[i], lty = lty[i], 
                       ...)

          if(type !='l')
            gfun$points(x[j], y[j],
                        col = col[i],
                        pch = pch[if(pchVaries)j
                        else i], 
                        cex = cex[if(sizeVaries)j
                        else i],
                        font = font[i], lty=lty[i], lwd=lwd[i], ...)
        }
    }
  
  ## 14Apr2001 MB changes: set colors for method = "filled bands"
  if(!length(col.fill)) col.fill <- plot.fill
  col.fill <- rep(col.fill, length = ng)
  ## end MB

  if(ng > 1) {
    ## MB 14Apr2001: if method == "filled bands"
    ## have to plot filled bands first, otherwise lines/symbols
    ## would be hidden by the filled band
    if(method == "filled bands")
      {
        gfun <- ordGridFun(TRUE)
        for(gg in levnum)
          {
            s <- g == gg
            gfun$polygon(x=c(x[s],rev(x[s])),
                         y=c(lower[s], rev(upper[s])),
                         col=col.fill[gg], ...)
          }
      }  ## end MB
    
    pspanel(x, y, subscripts, groups, lwd = lwd, lty = 
            lty, pch = pch, cex = cex, font = font, col
            = col, type = type, sizeVaries=sizeVaries, pchVaries=pchVaries)
    if(type != "p" && !(is.logical(label.curves) && !
         label.curves))
      {
        lc <- if(is.logical(label.curves)) list(lwd  = lwd, cex = cex[1])
        else c(list(lwd = lwd, cex = cex[1]), label.curves)
        
        curves <- vector("list", length(levnum))
        names(curves) <- levels(groups)[levnum]
        i <- 0
        for(gg in levnum)
          {
            i <- i + 1
            s <- g == gg
            curves[[i]] <- list(x[s], y[s])
          }

        labcurve(curves, lty = lty[levnum], lwd = lwd[levnum],
                 col. = col[levnum], opts = lc, grid=TRUE, ...)
      }
  }

  ## MB 14Apr2001: if method == "filled bands"
  ## plot filled bands first, otherwise lines/symbols
  ## would be hidden by the filled band
  else
    {
      if(method == "filled bands")
            grid.polygon(x = c(x, rev(x)), y = c(lower, rev(upper)),
                         gp=gpar(fill = col.fill, col='transparent'),
                         default.units='native')
      ## end MB

      ppanel(x, y, lwd = lwd, lty = lty, pch = pch, cex = cex,
             font = font, col = col, type = type)
    } 
  ## 14Apr2001 MB
  ## final change for filled bands: just skip the rest
  ## if method = filled bands, remaining columns of other are ignored
  
  if(nother && method != "filled bands")
    {
      if(method == "bands")
        {
          dob <- function(a, def, ng, j)
            {
              if(!length(a)) return(def)
              
              if(!is.list(a)) a <- list(a)

              a <- rep(a, length = ng)
              sapply(a, function(b, j)
                     b[j], j = j)
            }
          for(j in 1:ncol(other))
            {
              if(ng == 1)
                ppanel(x, other[, j], 
                       lwd = dob(lwd.bands, lwd, ng, j),
                       lty = dob(lty.bands, lty, ng, j), 
                       col = dob(col.bands, col, ng, j), 
                       pch = pch, cex = cex, font = 
                       font, type = "l")
              else pspanel(x, other[, j], 
                           subscripts, groups, 
                           lwd = dob(lwd.bands, lwd, ng, j),
                           lty = dob(lty.bands, lty, ng, j), 
                           col = dob(col.bands, col, ng, j), 
                           pch = pch, cex = cex, font = 
                           font, type = "l", 
                           sizeVaries=sizeVaries, pchVaries=pchVaries)
            }
        }
      else
        {
          errbr <- function(x, y, lower, upper, cap, 
                            lty, lwd, col, connect)
            {
              gfun    <- ordGridFun(TRUE) ## see Misc.s
              segmnts <- gfun$segments
              gun     <- gfun$unit
              
              smidge <- 0.5 * cap * unit(1,'npc')

              switch(connect,
                     all = {
                       segmnts(x, lower, x, upper,
                               lty = lty, lwd = lwd, col = col)
                       segmnts(gun(x)-smidge, lower,
                               gun(x)+smidge, lower,
                               lwd = lwd, lty = 1, col = col)
                       segmnts(gun(x)-smidge, upper,
                               gun(x)+smidge, upper,
                               lwd = lwd, lty = 1, col = col)
                     },
                     upper = {
                       segmnts(x, y, x, upper, lty = lty, lwd = lwd, col = col)
                       segmnts(gun(x)-smidge,  upper,
                               gun(x)+smidge,  upper,
                               lwd = lwd, lty = 1, col = col)
                     },
                     lower = {
                       segmnts(x, y, x, lower, lty = lty, lwd = lwd, col = col)
                       segmnts(gun(x)-smidge,  lower,
                               gun(x)+smidge,  lower,
                               lwd = lwd, lty = 1, col = col)
                     }
                     )
            }

          if(ng == 1)
            errbr(x, y, lower, upper, cap, 
                  lty.bar, lwd, col, switch(method,
                                            bars = "all",
                                            "upper bars" = "upper",
                                            "lower bars" = "lower",
                                            "alt bars" = "lower"))
          else
            {
              if(method == "alt bars")
                medy <- median(y, na.rm = TRUE)
              for(gg in levnum)
                {
                  s <- g == gg
                  connect <- switch(method,
                                    bars = "all",
                                    "upper bars" = "upper",
                                    "lower bars" = "lower",
                                    "alt bars" = if(median(y[s], 
                                      na.rm = TRUE) > medy) "upper"
                                    else "lower")
                  
                  errbr(x[s], y[s], lower = lower[s],
                        upper = upper[s], cap, lty.bar, 
                        lwd[gg], col[gg], connect)
                }
            }
        }
    }

  if(length(minor.ticks))
    {
      minor.at <-
        if(is.list(minor.ticks)) minor.ticks$at
        else
          minor.ticks

      minor.labs <-
        if(is.list(minor.ticks) && length(minor.ticks$labels))
          minor.ticks$labels
        else
          FALSE

      gfun$axis(side = 1, at = minor.at, labels = FALSE,
                tck = par("tck") * 0.5, outer = TRUE, cex = par("cex") * 
                0.5)
      
      if(!is.logical(minor.labs))
        gfun$axis(side = 1, at = minor.at, labels = 
                  minor.labs, tck = 0, cex = par("cex") * 0.5, line = 1.25)
    }

  if(ng > 1)
    {
      ##set up for key() if points plotted
      Key1 <- function(x=0, y=1, lev, cex, col, font, pch, other)
        {
          ## Even though par('usr') shows 0,1,0,1 after lattice draws
          ## its plot, it still needs resetting
          if(!length(x)) x <- 0.05
          if(!length(y)) y <- 0.95  ## because of formals()
          rlegendg(x, y, legend=lev, cex=cex, col=col, pch=pch, other=other)
          invisible()
        }
      
      formals(Key1) <- list(x=NULL,y=NULL,lev=levels(groups),
                           cex=if(sizeVaries) 1 else cex,
                           col=col, font=font, pch=pch, other=NULL)
      .setKey(Key1)
      rm(Key1)
    }
  
  if(!missing(abline)) {
    pabl <- lattice::panel.abline
    if(length(names(abline))) do.call(pabl, abline)
    else for(i in 1:length(abline)) do.call(pabl, abline[[i]])
  }
  

  if(type == "l" && ng > 1)
    {
      ## Set up for legend (key() or rlegendg()) if lines drawn
      Key2 <- function(x=0, y=1, lev, cex, col, lty, lwd, other)
        {
          ## Even though par('usr') shows 0,1,0,1 after lattice draws
          ## its plot, it still needs resetting
          if(!length(x)) x <- 0.05
          
          if(!length(y)) y <- 0.95  ## because of formals()
          
          rlegendg(x, y, legend=lev, cex=cex, col=col, lty=lty, lwd=lwd,
                   other=other)
          invisible()
        }
      
      formals(Key2) <- list(x=NULL,y=NULL,lev=levels(groups), col=col,
                           lty=lty, lwd=lwd, other=NULL)
      .setKey(Key2)
      rm(Key2)
    }
}


xYplot <- 
  function (formula, data=sys.frame(sys.parent()),
            groups, subset,
            xlab=NULL, ylab=NULL, ylim=NULL,
            panel=panel.xYplot, prepanel=prepanel.xYplot,
            scales=NULL, minor.ticks=NULL, sub=NULL, ...)
{
  sRequire('lattice')
  yvname <- as.character(formula[2])  # tried deparse
  y <- eval(parse(text=yvname), data)
  if(!length(ylab)) ylab <- label(y, units=TRUE, plot=TRUE,
                                  default=yvname, grid=TRUE)
  
  if(!length(ylim))
    {
      yother <- attr(y,'other')
      if(length(yother)) ylim <- range(y, yother, na.rm=TRUE)
    }

  xvname <- formula[[3]]
  if(length(xvname)>1 && as.character(xvname[[1]])=='|') 
    xvname <- xvname[[2]]  # ignore conditioning var

  xv <- eval(xvname, data)
  if(!length(xlab))
    xlab <- label(xv, units=TRUE, plot=TRUE,
                  default=as.character(xvname)[1],
                  grid=TRUE)
  
  if(!length(scales$x))
    {
      if(length(maj <- attr(xv,'scales.major')))
        scales$x <- maj
    }

  if(!length(minor.ticks))
    {
      if(length(minor <- attr(xv,'scales.minor')))
        minor.ticks <- minor
    }

  if(!missing(groups)) groups <- eval(substitute(groups),data)
  
  if(!missing(subset)) subset <- eval(substitute(subset),data)

  ## Note: c(list(something), NULL) = list(something)
  ## The following was c(list(formula=formula,...,panel=panel),if()c(),...)
  lxyp <- lattice::xyplot
  do.call(lxyp,
          c(list(x = formula, data=data, prepanel=prepanel,
                 panel=panel),
            if(length(ylab))list(ylab=ylab),
            if(length(ylim))list(ylim=ylim),
            if(length(xlab))list(xlab=xlab),
            if(length(scales))list(scales=scales),
            if(length(minor.ticks))list(minor.ticks=minor.ticks),
            if(!missing(groups))list(groups=groups),
            if(!missing(subset))list(subset=subset),
            if(!missing(sub))   list(sub=sub),
            list(...)))
}


prepanel.Dotplot <- function(x, y, ...)
{
  xlim <- range(x, attr(x,'other'), na.rm=TRUE)
  ylim <- range(as.numeric(y), na.rm=TRUE)  ## as.numeric 25nov02
  list(xlim=xlim, ylim=ylim) #, dx=diff(xlim), dy=diff(ylim))
}

panel.Dotplot <- function(x, y, groups = NULL,
                          pch  = dot.symbol$pch, 
                          col  = dot.symbol$col, cex = dot.symbol$cex, 
                          font = dot.symbol$font, abline, ...)
{
  sRequire('lattice')
  gfun <- ordGridFun(TRUE) ## see Misc.s
  segmnts <- gfun$segments
  pabl <- lattice::panel.abline
  y <- as.numeric(y)

  gp <- length(groups)
  dot.symbol <- lattice::trellis.par.get(if(gp)'superpose.symbol'
                                else 'dot.symbol')
  
  dot.line   <- lattice::trellis.par.get('dot.line')
  plot.line  <- lattice::trellis.par.get(if(gp)'superpose.line'
                                else 'plot.line')

  gfun$abline(h = unique(y), lwd=dot.line$lwd, lty=dot.line$lty, 
              col=dot.line$col)
  if(!missing(abline))
    {
      if(length(names(abline))) do.call(pabl, abline)
      else for(i in 1:length(abline)) do.call(pabl, abline[[i]])
    }

  other <- attr(x,'other')
  x <- unclass(x)
  attr(x,'other') <- NULL
  if(length(other))
    {
      nc <- ncol(other)
      segmnts(other[,1], y, other[,nc], y, lwd=plot.line$lwd[1],
              lty=plot.line$lty[1], col=plot.line$col[1])
      if(nc==4)
        {
          segmnts(other[,2], y, other[,3], y, lwd=2*plot.line$lwd[1],
                  lty=plot.line$lty[1], col=plot.line$col[1])
          gfun$points(other[,2], y, pch=3, cex=cex, col=col, font=font)
          gfun$points(other[,3], y, pch=3, cex=cex, col=col, font=font)
        }
      
      if(gp) lattice::panel.superpose(x, y, groups=as.numeric(groups), pch=pch,
                             col=col, cex=cex, font=font, ...)
      else
        gfun$points(x, y, pch=pch[1], cex=cex, col=col, font=font)
    }
  else
    {
      if(gp) 
        lattice::panel.superpose(x, y, groups=as.numeric(groups),
                        pch=pch, col=col, cex=cex,
                        font=font, ...)
      else
        lattice::panel.dotplot(x, y, pch=pch, col=col, cex=cex, font=font, ...)
    }
  if(gp)
    {
      Key <- function(x=0, y=1, lev, cex, col, font, pch, other)
        {
          if(!length(x)) x <- 0.05
          if(!length(y)) y <- 0.95  ## because of formals()
          rlegendg(x, y, legend=lev, cex=cex, col=col, pch=pch, other=other)
          invisible()
        }
      
      lev <- levels(as.factor(groups))
      ng <- length(lev)
      formals(Key) <- list(x=NULL,y=NULL,lev=lev,
                           cex=cex[1:ng], col=col[1:ng],
                           font=font[1:ng], pch=pch[1:ng], other=NULL)
      .setKey(Key)
    }
}


Dotplot <-
  function (formula, data=sys.frame(sys.parent()),
            groups, subset,
            xlab=NULL, ylab=NULL, ylim=NULL,
            panel=panel.Dotplot, prepanel=prepanel.Dotplot,
            scales=NULL, xscale=NULL, ...)
{
  sRequire('lattice')
  yvname <- as.character(formula[2])  # tried deparse
  yv <- eval(parse(text=yvname), data)
  if(!length(ylab))
    ylab <- label(yv, units=TRUE, plot=TRUE,
                  default=yvname, grid=TRUE)

  if(!length(ylim))
    {
      yother <- attr(yv,'other')
      if(length(yother)) ylim <- range(yv, yother, na.rm=TRUE)
    }

  if(is.character(yv)) yv <- factor(yv)
  if(!length(scales) && is.factor(yv))
    scales <- list(y=list(at=1:length(levels(yv)),labels=levels(yv)))
  if(length(xscale)) scales$x <- xscale
  
  xvname <- formula[[3]]
  if(length(xvname)>1 && as.character(xvname[[1]])=='|') 
    xvname <- xvname[[2]]  # ignore conditioning var
  xv <- eval(xvname, data)
  if(!length(xlab)) xlab <- label(xv, units=TRUE, plot=TRUE,
                                  default=as.character(xvname)[1], grid=TRUE)

  if(!missing(groups)) groups <- eval(substitute(groups),data)
  
  if(!missing(subset)) subset <- eval(substitute(subset),data)
  
  dul <- options('drop.unused.levels')
  options(drop.unused.levels=FALSE)   ## for empty cells
  on.exit(options(dul))                      ## across some panels

  lxyp <- lattice::xyplot
  do.call(lxyp,
          c(list(x = formula, data=data, prepanel=prepanel,
                 panel=panel),
            if(length(ylab))list(ylab=ylab),
            if(length(ylim))list(ylim=ylim),
            if(length(xlab))list(xlab=xlab),
            if(!missing(groups))list(groups=groups),
            if(!missing(subset))list(subset=subset),
            if(length(scales))list(scales=scales),
            list(...)))
}


setTrellis <- function(strip.blank=TRUE, lty.dot.line=2,
                       lwd.dot.line=1)
{
  sRequire('lattice')
  if(strip.blank) trellis.strip.blank()   # in Hmisc Misc.s
  dot.line <- lattice::trellis.par.get('dot.line')
  dot.line$lwd <- lwd.dot.line
  dot.line$lty <- lty.dot.line
  lattice::trellis.par.set('dot.line',dot.line)
  invisible()
}


numericScale <- function(x, label=NULL, ...)
{
  xn <- as.numeric(x)
  attr(xn,'label') <- if(length(label)) label
  else
    deparse(substitute(x))
  xn
}

## See proc.scale.trellis, render.trellis, axis.trellis for details of
## how scale is used
