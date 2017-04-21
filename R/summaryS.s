summaryS <- function(formula, fun=NULL,
                     data=NULL, subset=NULL, na.action=na.retain,
                     continuous=10,
                     ...) {
  
  formula <- Formula(formula)

  Y <- if(length(subset))
    model.frame(formula, data=data, subset=subset, na.action=na.action)
  else
    model.frame(formula, data=data, na.action=na.action)
  X <- model.part(formula, data=Y, rhs=1)
  Y <- model.part(formula, data=Y, lhs=1)

  nY <- NCOL(Y)
  nX <- NCOL(X)
  namY <- names(Y)
  namX <- names(X)
  if(nX == 0) X <- data.frame(x=rep(1, NROW(Y)))
  ux <- unique(X)
  Z <- NULL
  n <- nrow(X)
  
  g <- function(y) {
    y <- y[! is.na(y)]
    if(is.character(y)) c(NA, NA)
    else if(is.factor(y)) c(1, length(levels(y)))
    else if(length(unique(y)) < continuous) range(y)
    else if(! is.matrix(y)) quantile(y, c(0.01, 0.99))
  }
  ylim <- lapply(Y, g)

  w <- reshape(cbind(X, Y), direction='long', v.names='y',
               varying=namY, times=namY, timevar='yvar')

  if(is.Surv(Y[[1]])) {
    at <- attributes(Y[[1]])
    at <- at[setdiff(names(at), c('dim', 'dimnames'))]
    attributes(w$y) <- c(attributes(w$y), at)
  }
  w$yvar <- factor(w$yvar, namY)
  funlabel <- NULL
  if(length(fun)) {
    by <- c('yvar', if(length(namX)) namX else 'x')
    y <- w$y
    w <- summarize(y, w[by], fun, type='matrix', keepcolnames=TRUE)
    funlabel <- if(is.matrix(w$y)) colnames(w$y)[1]
  }

  gg <- function(x) if(is.character(x) || is.factor(x))
                      'categorical' else 'numeric'
  xlabels <- sapply(X, label)
  xlabels <- ifelse(xlabels == '', names(xlabels), xlabels)
  ylabels <- sapply(Y, label)
  ylabels <- ifelse(ylabels == '', names(ylabels), ylabels)

  structure(w, class=c('summaryS', 'data.frame'), formula=formula, fun=fun,
            xnames=names(X), xlabels=xlabels, xunits=sapply(X, units),
            xtype=sapply(X, gg),
            ynames=namY, ylabels=ylabels, yunits=sapply(Y, units),
            ylim=ylim, funlabel=funlabel)
}

plot.summaryS <-
  function(x, formula=NULL, groups=NULL, panel=NULL,
           paneldoesgroups=FALSE, datadensity=NULL, ylab='',
           funlabel=NULL, textonly='n', textplot=NULL, digits=3, custom=NULL,
           xlim=NULL, ylim=NULL, cex.strip=1, cex.values=0.5, pch.stats=NULL,
           key=list(columns=length(groupslevels),
             x=.75, y=-.04, cex=.9,
             col=trellis.par.get('superpose.symbol')$col, corner=c(0,1)),
           outerlabels=TRUE, autoarrange=TRUE, scat1d.opts=NULL, ...)
{
  xtype <- attr(x, 'xtype')
  nn    <- sum(xtype == 'numeric')
  if(nn > 1) stop('does not handle more than one numeric continuous x')
  X       <- x
  at      <- attributes(x)
  Form    <- at$formula
  nX      <- at$nX
  nY      <- at$nY
  ylabels <- at$ylabels
  yunits  <- at$yunits
  ylims   <- at$ylim
  xnames  <- at$xnames
  xlabels <- at$xlabels
  xunits  <- at$xunits
  fun     <- at$fun
  funlabel <- if(length(at$funlabel)) at$funlabel else funlabel
  Panel    <- panel
  
  ptype <- if(length(fun)) {  # used to always be 'dot'
    if(length(Panel)) 'xy.special' else 'dot'
  } else 'xy'
  if(ptype %in% c('xy', 'xy.special') && ! any(xtype == 'numeric'))
    stop('must have a numeric x variable to make x-y plot')

  groupslevels <- if(length(groups)) levels(x[[groups]])
  condvar <- xnames[xtype == 'categorical']
  ## Reorder condvar in descending order of number of levels
  numu <- function(x) if(is.factor(x)) length(levels(x))
                       else length(unique(x[! is.na(x)]))

  if(autoarrange && length(condvar) > 1) {
    nlev <- sapply(X[condvar], numu)
    condvar <- condvar[order(nlev)]
  }
  
  form <- if(length(formula)) formula
  else {
    ## Non-groups conditioning variables
    ngcond <- setdiff(condvar, groups)
    ## Collapsed non-group conditioning variables
    ccv <- paste('|', paste(c(ngcond, 'yvar'), collapse=' * '))
    ## Collapsed non-group cond var after the first
    ccv1 <- if(length(ngcond > 1))
      paste('|', paste(c(ngcond[-1], 'yvar'), collapse=' * '))
    f <- if(ptype %in% c('xy', 'xy.special'))
      paste('y ~', xnames[xtype == 'numeric'], ccv, sep='')
      else paste(ngcond[1], '~ y', ccv1, sep='')
    as.formula(f)
  }

  pst <- list(cex=cex.strip)
  yvarlev <- NULL
   for(v in levels(X$yvar)) {
    un <- yunits[v]
    l <- if(ylabels[v] == v && un == '') v else
         labelPlotmath(ylabels[v], un)
    yvarlev <- c(yvarlev, l)
  }
  
  strip <- function(which.given, which.panel, var.name, factor.levels, ...) {
    current.var <- var.name[which.given]
    levs <- if(current.var == 'yvar') yvarlev else factor.levels
    strip.default(which.given, which.panel, var.name, factor.levels=levs, ...)
  }

  ylev <- levels(X$yvar)
  ## lims <- if(length(xlim)) xlim else ylims[ylev]
  lims <- ylims[ylev]
  ## lims needs to be repeated according to layout
  vars <- all.vars(form)
  cond <- vars[- (1 : 2)]
  if(! all(cond %in% 'yvar') && cond[1] != 'yvar') {
    ngny <- setdiff(cond, 'yvar')
    nr <- length(unique(do.call('paste', X[ngny])))
    lims <- rep(lims, each=nr)
  }
  if(length(ylim)) lims <- ylim
  
  
  d <- if(ptype == 'xy') {
    pan <- if(! length(datadensity)) function(...) {}
    else
      function(x, y, subscripts, groups=NULL, ...) {
        gp <- length(groups)
        plot.line <-
          trellis.par.get(if(gp) "superpose.line" else "plot.line")
        col <- plot.line$col
        gr <- if(gp) groups[subscripts] else factor(rep('', length(x)))
        ypos <- unit(1, 'npc')
        for(i in 1:length(levels(gr))) {
          j <- which(gr == levels(gr)[i])
          ypos <- ypos -
            if(length(scat1d.opts) && 'nhistSpike' %in% names(scat1d.opts))
              unit(2, 'mm') else unit(1, 'mm')
          do.call('scat1d', c(list(x=x[j], y=ypos, col=col[i], grid=TRUE),
                              scat1d.opts))
        }
      }
    scal <-  list(y=list(relation='free', limits=lims, rot=0))
    if(length(xlim)) scal$x <- list(limits=xlim)

    xlab <- labelPlotmath(xlabels[xtype == 'numeric'],
                          xunits [xtype == 'numeric'])
    if(! length(groups)) {
      if(! length(Panel)) Panel <- panel.xyplot
      xyplot(form, data=X, 
             panel=function(...) {Panel(...); pan(...)},
             xlab=xlab, ylab=ylab, scales=scal, strip=strip,
             par.strip.text=pst, ...)
    } else {
      panel.groups <- if(paneldoesgroups) NULL else
       if(length(Panel)) Panel else panel.xyplot
      if(! paneldoesgroups) Panel <- panel.superpose
      g <- if(length(panel.groups))
        "xyplot(form, groups=%s, data=X, scales=scal, panel=function(...) {if(length(Panel)) Panel(..., scat1d.opts=scat1d.opts); pan(...)}, panel.groups=panel.groups, auto.key=key, xlab=xlab, ylab=ylab, strip=strip, par.strip.text=pst, ...)"
      else
        "xyplot(form, groups=%s, data=X, scales=scal, panel=function(...) {if(length(Panel)) Panel(..., scat1d.opts=scat1d.opts); pan(...)}, auto.key=key, xlab=xlab, ylab=ylab, strip=strip, par.strip.text=pst, ...)"
      eval(parse(text=sprintf(g, groups)))
  }
 } else {  # Dot chart or xy.special
   ## If > 1 calculated statistics, y is a matrix.
   ## Save first column as y and save remaining columns that are not
   ## text only as yother
   y      <- X$y
   yother <- NULL
   yText  <- NULL
   if(is.matrix(y) && length(c(textonly, textplot))) {
     ptext <- colnames(y) %in% c(textonly, textplot)
     if(any(ptext)) yText <- y[, ptext, drop=FALSE]
     ponly <- colnames(y) %in% textonly
     if(any(ponly)) y     <- y[, ! ponly]
   }
   if(is.matrix(y) && ncol(y) > 1) {
     yother <- y[, -1, drop=FALSE]
     X$y    <- y[, 1]
   }
   # ylev <- levels(X$yvar)
   # lims <- if(length(xlim)) xlim else ylims[ylev]
   # ## lims needs to be repeated according to layout
   # vars <- all.vars(form)
   # cond <- vars[- (1 : 2)]
   # if(! all(cond %in% 'yvar') && cond[1] != 'yvar') {
   #   ngny <- setdiff(cond, 'yvar')
   #   nr <- length(unique(do.call('paste', X[ngny])))
   #   lims <- rep(lims, each=nr)
   # }
   # if(length(ylim)) lims <- ylim
   scal <-  list(x=list(relation='free', limits=xlim))   # limits=lims))
   if(ptype == 'xy.special') names(scal) <- 'y'

   if(ptype == 'dot') {
     pan <- function(x, y, subscripts, groups=NULL, yother=NULL,...) {
     
       gp <- length(groups)
       dot.symbol <-
         trellis.par.get(if(gp)'superpose.symbol'  else 'dot.symbol')
       plot.line <-
         trellis.par.get(if(gp) "superpose.line" else "plot.line")
       pch  = dot.symbol$pch
       col  = dot.symbol$col
       cex  = dot.symbol$cex
       font = dot.symbol$font
       segmnts <- function (x0, y0, x1, y1, ...) 
         grid::grid.segments(x0, y0, x1, y1, default.units = "native",
                       gp = grid::gpar(...))
       
       if(length(yother)) {
         snames <- colnames(yother)
         nc     <- ncol(yother)
         yoth   <- yother[subscripts,, drop=FALSE]
         gr <- if(gp) groups[subscripts] else factor(rep('', length(x)))
         if(length(yText)) {
           yText <- yText[subscripts,, drop=FALSE]
           if(length(custom)) {
             k <- custom(yText)
             pasted <- k$result
             llong  <- unit(1, 'strwidth', k$longest)
           } else {
             pasted <- rep('', length(y))
             for(i in 1 : ncol(yText)) {
               if(i > 1) pasted <- paste(pasted, ' ', sep='')
               pasted <- paste(pasted, colnames(yText)[i], '=',
                               format(round(yText[, i], digits=digits)),
                               sep='')
             }
             llong <- unit(1, 'strwidth',
                           paste('  ', pasted[which.max(nchar(pasted))],
                                 sep=''))
           }
           xpos  <- unit(1, 'npc') - unit(1, 'mm') - length(levels(gr)) * llong
         }
         for(i in 1:length(levels(gr))) {
           j <- which(gr == levels(gr)[i])
           if(nc > 1)
             segmnts(yoth[j, 1], y[j], yoth[j, nc], y[j],
                     lwd=2 * plot.line$lwd[1],
                     lty=plot.line$lty[i], col=plot.line$col[i])
           if(nc == 4)
             segmnts(yoth[j ,2], y[j], yoth[j ,3], y[j],
                     lwd=2*plot.line$lwd[1],
                     lty=plot.line$lty[i], col=plot.line$col[i])
           for(k in 1 : nc) {
             if(length(pch.stats)) {
               p <- pch.stats[snames[k]]
               if(!is.na(p)) 
                 lpoints(yoth[j, k], y[j], pch=p, cex=cex, col=col[i],
                         font=font)
             }
           }
           ## Show selected statistics just under dot lines
           if(length(yText)) {
             xpos <- xpos + llong
             grid::grid.text(pasted[j], xpos,
                       unit(y[j], 'native') - unit(1.75, 'mm'), just='right',
                       gp=grid::gpar(cex=cex.values, col=col[i]))
           }
         }       
         if(gp) panel.superpose(x, y, groups=as.numeric(groups),
                                subscripts=subscripts,
                                pch=pch, col=col, cex=cex, font=font, ...)
         else
           panel.dotplot(x, y, subscripts=subscripts,
                         pch=pch, col=col, cex=cex, font=font, ...)
       }
       else {
         if(gp) 
           panel.superpose(x, y, groups=as.numeric(groups),
                           subscripts=subscripts,
                           pch=pch, col=col, cex=cex,
                           font=font, ...)
         else
           panel.dotplot(x, y, subscripts=subscripts,
                         pch=pch, col=col, cex=cex, font=font, ...) 
       }
     }
   
     d <- if(!length(groups))
       dotplot(form, data=X, panel=pan, strip=strip, par.strip.text=pst,
               xlab=funlabel, scale=scal, yother=yother,...)
     else eval(parse(text=
                     sprintf("dotplot(form, groups=%s, data=X, panel=pan, strip=strip, par.strip.text=pst, auto.key=key, xlab=funlabel, scale=scal, yother=yother, ...)", groups) ))
   }  # end ptype 'dot'
   else {  # ptype 'xy.special'
     xl <- labelPlotmath(xlabels[1], xunits[1])
     yl <- if(ylab == '') funlabel else ylab
     d <- if(!length(groups))
       xyplot(form, data=X, panel=Panel, strip=strip,
              par.strip.text=pst, xlab=xl, ylab=yl, scale=scal, yother=yother,
             ...)
     else {
       panel.groups <- if(paneldoesgroups) NULL else Panel
       if(! paneldoesgroups) panel <- panel.superpose
       g <- if(length(panel.groups))
         "xyplot(form, groups=%s, data=X, panel=Panel, panel.groups=panel.groups, strip=strip, par.strip.text=pst, auto.key=key, xlab=xl, ylab=yl, scale=scal, yother=yother, ...)"
       else "xyplot(form, groups=%s, data=X, panel=Panel, strip=strip, par.strip.text=pst, auto.key=key, xlab=xl, ylab=yl, scale=scal, yother=yother, ...)"
       eval(parse(text=sprintf(g, groups)))
     }
   }
 }
  if(outerlabels && length(dim(d)) == 2)
    d <- latticeExtra::useOuterStrips(d, strip=strip, strip.left=strip)
  d
}

mbarclPanel <- function(x, y, subscripts, groups=NULL, yother, ...) {
  gp <- length(groups)
  plot.line <-
    trellis.par.get(if(gp) "superpose.line" else "plot.line")
  yother <- yother[subscripts, , drop=FALSE]
  se     <- if('se' %in% colnames(yother)) yother[, 'se']
  yother <- yother[, colnames(yother) %nin% c('n', 'se'), drop=FALSE]

  if(all(c('0.375', '0.625') %in% colnames(yother))) {
    ## If HD median estimate is not between 0.375 and 0.625 quantiles
    ## take it to be the closer of the two
    y375 <- yother[, '0.375']
    y625 <- yother[, '0.625']
    y <- pmin(y, y625)
    y <- pmax(y, y375)
  }
  yother <- cbind(y=y, yother)
  gr <- if(gp) groups[subscripts] else factor(rep('', length(x)))
  lev <- levels(gr)
  for(i in 1 : length(lev)) {
    j <- which(gr == levels(gr)[i])
    multLines(x[j], yother[j,, drop=FALSE],
              col=plot.line$col[i],
              lty=plot.line$lty[i],
              lwd=plot.line$lwd[i],
              grid=TRUE, pos=c('left', 'right')[i])
  }
  if(length(lev) == 2 && length(se)) {
    xu <- sort(unique(x))
    j1 <- gr == lev[1]
    j2 <- gr == lev[2]
    Y <- matrix(NA, nrow=length(xu), ncol=4,
                dimnames=list(as.character(xu), c('y1', 'y2', 'se1', 'se2')))
    x1 <- as.character(x)[j1]
    x2 <- as.character(x)[j2]
    Y[x1, 'y1' ] <- y [j1]
    Y[x1, 'se1'] <- se[j1]
    Y[x2, 'y2' ] <- y [j2]
    Y[x2, 'se2'] <- se[j2]
    ymid <- (Y[, 'y1'] + Y[, 'y2']) / 2.
    halfwidthci <- qnorm(0.975) * sqrt(Y[, 'se1']^2 + Y[, 'se2']^2)
    col <- adjustcolor('black', alpha.f=0.7)
    grid::grid.segments(xu, ymid - 0.5 * halfwidthci,
                  xu, ymid + 0.5 * halfwidthci,
                  default.units='native',
                  gp=grid::gpar(col=col, lwd=1.5))
  }
}

medvPanel <-
  function(x, y, subscripts, groups=NULL, violin=TRUE, quantiles=FALSE,
           ...) {
  gp <- length(groups)
  plot.line <-
         trellis.par.get(if(gp) "superpose.line"   else "plot.line")
  sym <- trellis.par.get(if(gp) "superpose.symbol" else "plot.symbol")

  quant <- function(y) {
    probs <- c(0.05, 0.125, 0.25, 0.375)
    probs <- sort(c(probs, 1. - probs))
    y <- y[! is.na(y)]
    if(length(y) < 3) {
      if(quantiles) {
        w <- c(median(y), rep(NA, 9), length(y))
        names(w) <- c('Median', format(probs), 'se', 'n')
      }
      else w <- c(Median=median(y), se=NA, n=length(y))
      return(w)
    }
    w  <- if(quantiles) hdquantile(y, probs)
    m  <- hdquantile(y, 0.5, se=TRUE)
    se <- as.numeric(attr(m, 'se'))
    c(Median=as.numeric(m), w, se=se, n=length(y))
  }

  denpoly <- function(x, y, col, n=50, pos, ...) {
    y <- y[! is.na(y)]
    n <- length(y)
    if(n < 2) return()
    den <- density(y, n=n, ...)
    d <- den$y
    y <- den$x
    ## Scale density of 0-3 mm
    d <- 3 * d / max(d)
    d <- c(d, d[length(d)])
    mm <- grid::convertUnit(unit(d, 'mm'), 'mm', typeFrom='dimension')
    kol <- if(n < 5 ) adjustcolor(col, alpha.f=0.2)
     else  if(n < 10) adjustcolor(col, alpha.f=0.4)
     else col
    grid::grid.polygon(y=unit(c(y, y[1]), 'native'),
                 x=if(pos == 'left') unit(x, 'native') - mm
                   else              unit(x, 'native') + mm,
                 gp=grid::gpar(col=FALSE, fill=kol))
  }
    

  gr <- if(gp) groups[subscripts] else factor(rep('', length(x)))
  lev <- levels(gr)
  W <- NULL
  for(i in 1 : length(lev)) {
    j  <- which(gr == levels(gr)[i])
    xj <- x[j]
    yj <- y[j]
    w <- summarize(yj, xj, quant, type='matrix', keepcolnames=TRUE)
    Y  <- w$yj
    xu <- w$xj
    lpoints(xu, Y[,'Median'], cex=sym$cex[i], pch=sym$pch[i], col=sym$col[i],
            alpha=sym$alpha[i])
    llines(xu, Y[,'Median'], col=plot.line$col[i], lty=plot.line$lty[i],
           lwd=plot.line$lwd[i], alpha=plot.line$alpha)
    col <- plot.line$col[i]
    if(violin) for(xx in sort(unique(xj)))
      denpoly(xx, yj[xj == xx],
              col=adjustcolor(plot.line$col[i], alpha.f=0.4),
              pos=c('left', 'right')[i])
      
    if(quantiles)
      multLines(xu, Y[, colnames(Y) %nin% c('se', 'n'), drop=FALSE],
                col=plot.line$col[i],
                lty=plot.line$lty[i],
                lwd=plot.line$lwd[i],
                grid=TRUE, pos=c('left', 'right')[i])
    W <- rbind(W, cbind(gr=levels(gr)[i], w))
  }
  if(length(lev) == 2) {
    x <- W$xj
    xu <- sort(unique(W$xj))
    j1 <- W$gr == lev[1]
    j2 <- W$gr == lev[2]
    Y <- matrix(NA, nrow=length(xu), ncol=4,
                dimnames=list(as.character(xu), c('y1', 'y2', 'se1', 'se2')))
    x1 <- as.character(x)[j1]
    x2 <- as.character(x)[j2]
    Y[x1, 'y1' ] <- W$yj[j1, 'Median']
    Y[x1, 'se1'] <- W$yj[j1, 'se']
    Y[x2, 'y2' ] <- W$yj[j2, 'Median']
    Y[x2, 'se2'] <- W$yj[j2, 'se']
    ymid <- (Y[, 'y1'] + Y[, 'y2']) / 2.
    halfwidthci <- qnorm(0.975) * sqrt(Y[, 'se1']^2 + Y[, 'se2']^2)
    col <- adjustcolor('black', alpha.f=0.7)
    grid::grid.segments(xu, ymid - 0.5 * halfwidthci,
                  xu, ymid + 0.5 * halfwidthci,
                  default.units='native',
                  gp=grid::gpar(col=col, lwd=1.5))
  }
}
