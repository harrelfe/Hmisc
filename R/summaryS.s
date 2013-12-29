summaryS <- function(formula, fun=NULL,
                     data=NULL, subset=NULL, na.action=na.retain,
                     ...) {
  
  nobs <- nobsY(formula, data=data, subset=subset, na.action=na.action)
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

  w <- reshape(cbind(X, Y), direction='long', v.names='y',
               varying=namY, times=namY, timevar='yvar')

  if(is.Surv(Y[[1]])) {
    at <- attributes(Y[[1]])
    at <- at[setdiff(names(at), c('dim', 'dimnames'))]
    attributes(w$y) <- c(attributes(w$y), at)
  }
  w$yvar <- factor(w$yvar, namY)
  if(length(fun)) {
    by <- c('yvar', if(length(namX)) namX else 'x')
    y <- w$y
    w <- summarize(y, w[by], fun, type='matrix', keepcolnames=TRUE)
  }

  g <- function(x) if(is.character(x) || is.factor(x) ||
                      length(unique(x[!is.na(x)])) < 5) 'categorical' else 'numeric'
  xlabels <- sapply(X, label)
  xlabels <- ifelse(xlabels == '', names(xlabels), xlabels)
  ylabels <- sapply(Y, label)
  ylabels <- ifelse(ylabels == '', names(ylabels), ylabels)

  structure(w, class=c('summaryS', 'data.frame'), formula=formula, fun=fun,
            xnames=names(X), xlabels=xlabels, xunits=sapply(X, units),
            xtype=sapply(X, g),
            ynames=namY, ylabels=ylabels, yunits=sapply(Y, units), nobs=nobs)
}

plot.summaryS <-
  function(x, formula=NULL, groups=NULL, panel=NULL,
           paneldoesgroups=FALSE, datadensity=NULL, ylab='',
           funlabel=NULL, textonly='n', textplot=NULL, digits=3, custom=NULL,
           xlim=NULL, cex.strip=1, cex.values=0.5, pch.stats=NULL,
           key=list(columns=length(groupslevels),
             x=.75, y=-.04, cex=.9,
             col=trellis.par.get('superpose.symbol')$col, corner=c(0,1)),
           outerlabels=TRUE, autoarrange=TRUE, scat1d.opts=NULL, ...)
{
  xtype <- attr(x, 'xtype')
  nn <- sum(xtype == 'numeric')
  if(nn > 1) stop('does not handle more than one numeric continuous x')
  X <- x
  at      <- attributes(x)
  Form    <- at$formula
  nX      <- at$nX
  nY      <- at$nY
  ylabels <- at$ylabels
  yunits  <- at$yunits
  xnames  <- at$xnames
  xlabels <- at$xlabels
  xunits  <- at$xunits
  fun     <- at$fun
  
  ptype   <- if(length(fun)) 'dot' else 'xy'
  if(ptype == 'xy' && ! any(xtype == 'numeric'))
    stop('must have a numeric x variable to plot non-summarized data')

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
    ccv <- if(length(ngcond))
      paste('|', paste(c(ngcond, 'yvar'), collapse=' * '))
    ## Collapsed non-group cond var after the first
    ccv1 <- if(length(ngcond > 1))
      paste('|', paste(c(ngcond[-1], 'yvar'), collapse=' * '))
    f <- switch(ptype,
                xy = paste('y ~', xnames[xtype == 'numeric'], ccv, sep=''),
                dot= paste(ngcond[1], '~ y', ccv1, sep=''))
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
    xlab <- labelPlotmath(xlabels[xtype == 'numeric'],
                          xunits [xtype == 'numeric'])
    if(! length(groups)) {
      if(! length(panel)) panel <- panel.xyplot
      xyplot(form, data=X, scales=list(y='free', rot=0),
             panel=function(...) {panel(...); pan(...)},
             xlab=xlab, ylab=ylab, strip=strip, par.strip.text=pst, ...)
    } else {
      panel.groups <- if(paneldoesgroups) NULL else
       if(length(panel)) panel else panel.xyplot
      if(! paneldoesgroups) panel <- panel.superpose
      g <- if(length(panel.groups))
        "xyplot(form, groups=%s, data=X, scales=list(y='free', rot=0), panel=function(...) {panel(..., scat1d.opts=scat1d.opts); pan(...)}, panel.groups=panel.groups, auto.key=key, xlab=xlab, ylab=ylab, strip=strip, par.strip.text=pst, ...)"
      else
        "xyplot(form, groups=%s, data=X, scales=list(y='free', rot=0), panel=function(...) {panel(..., scat1d.opts=scat1d.opts); pan(...)}, auto.key=key, xlab=xlab, ylab=ylab, strip=strip, par.strip.text=pst, ...)"
      eval(parse(text=sprintf(g, groups)))
  }
 } else {  # Dot chart
    
   if(!missing(panel)) stop('cannot specify panel for categorical display using dotchart')

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
   ylev <- levels(X$yvar)
   if(length(xlim)) lims <- xlim else {
     lims <- vector('list', length(ylev))
     names(lims) <- ylev
     
     for(i in 1 : length(ylev)) {
       j <- X$yvar == ylev[i]
       r <- if(is.matrix(y))
         range(y[j, ], na.rm=TRUE) else range(y[j], na.rm=TRUE)
       lims[[i]] <- r
     }
   }
   ## lims needs to be repeated according to layout
   vars <- all.vars(form)
   cond <- vars[- (1 : 2)]
   if(! all(cond %in% 'yvar') && cond[1] != 'yvar') {
     ngny <- setdiff(cond, 'yvar')
     nr <- length(unique(do.call('paste', X[ngny])))
     lims <- rep(lims, each=nr)
   }
   scal <-  list(x=list(relation='free', limits=lims))

   pan <- function(x, y, subscripts, groups=NULL, ...) {
     
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
       grid.segments(x0, y0, x1, y1, default.units = "native",
                     gp = gpar(...))

     if(length(yother)) {
       snames <- colnames(yother)
       nc <- ncol(yother)
       yoth <- yother[subscripts,, drop=FALSE]
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
                         paste('  ', pasted[which.max(nchar(pasted))], sep=''))
         }
         xpos  <- unit(1, 'npc') - unit(1, 'mm') - length(levels(gr)) * llong
       }
       for(i in 1:length(levels(gr))) {
         j <- which(gr == levels(gr)[i])
         if(nc > 1)
           segmnts(yoth[j, 1], y[j], yoth[j, nc], y[j], lwd=2*plot.line$lwd[1],
                   lty=plot.line$lty[i], col=plot.line$col[i])
         if(nc == 4)
           segmnts(yoth[j ,2], y[j], yoth[j ,3], y[j], lwd=2*plot.line$lwd[1],
                   lty=plot.line$lty[i], col=plot.line$col[i])
         for(k in 1 : nc) {
           if(length(pch.stats)) {
             p <- pch.stats[snames[k]]
             if(!is.na(p)) 
               lpoints(yoth[j, k], y[j], pch=p, cex=cex, col=col[i], font=font)
           }
         }
         ## Show selected statistics just under dot lines
         if(length(yText)) {
           xpos <- xpos + llong
           grid.text(pasted[j], xpos,
                     unit(y[j], 'native') - unit(1.75, 'mm'), just='right',
                     gp=gpar(cex=cex.values, col=col[i]))
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
             xlab=funlabel, scale=scal, ...)
   else eval(parse(text=
                   sprintf("dotplot(form, groups=%s, data=X, panel=pan, strip=strip, par.strip.text=pst, auto.key=key, xlab=funlabel, scale=scal, ...)", groups) ))
 }
  
  if(outerlabels && length(dim(d)) == 2)
    d <- useOuterStrips(d, strip=strip, strip.left=strip)
  d
}
