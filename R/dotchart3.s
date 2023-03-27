dotchart3 <-
  function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
            pch = 21, gpch = pch, bg = par("bg"), color = par("fg"),
            gcolor = par("fg"), lcolor = "gray",
            xlim = range(c(x, gdata), na.rm=TRUE), main = NULL, 
            xlab = NULL, ylab = NULL, auxdata=NULL, auxtitle=NULL,
            auxgdata=NULL, axisat=NULL, axislabels=NULL,
            cex.labels = cex, cex.group.labels = cex.labels*1.25,
            cex.auxdata = cex, groupfont=2,
            auxwhere=NULL, height=NULL, width=NULL, ...) 
{
  opar <- par("mai", "mar", "cex", "yaxs")
  on.exit(par(opar))
  par(cex = cex, yaxs = "i")
  if (! is.numeric(x)) 
    stop("'x' must be a numeric vector or matrix")
  x    <- as.matrix(x)
  n    <- nrow(x)
  nc   <- ncol(x)
  pch  <- rep(pch, length.out=nc)
  
  if(! length(labels)) labels <- rownames(x)
  if(! length(labels)) stop('labels not defined')
  if(length(groups)) groups <- as.factor(groups)
  glabels <- levels(groups)

  plot.new()
  linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
  if (! length(glabels)) {
    ginch <- 0
    goffset <- 0
  }
  else {
    ginch <- max(strwidth(glabels, "inch", cex=cex.group.labels,
                          font=groupfont),
                 na.rm = TRUE)
    goffset <- 0.4
  }
  if(length(labels) + length(glabels) > 0) {
    nmai     <- par("mai")
    nmai[2L] <- max(nmai[2L], nmai[4L] + max(linch + goffset, ginch) + 0.1)
    ## Run strwidth separately because on of the 3 variables might
    ## be an expression, in which case an overall c(...) would combine the
    ## widths of character vectors
    if(length(auxdata) + length(auxgdata) > 0)
      nmai[4L] <- .2 + 1.1 * max(strwidth(auxtitle, 'inch', cex=cex.auxdata),
                                 strwidth(auxdata,  'inch', cex=cex.auxdata),
                                 strwidth(auxgdata, 'inch', cex=cex.auxdata))
    par(mai = nmai)
  }
  if (! length(groups)) {
    o      <- n:1L
    y      <- o
    ylim   <- c(.5, n + .5)
    x      <- x[o, , drop=FALSE]
    labels <- labels[o]
    if(length(auxdata)) auxdata <- auxdata[o]
  }
  else {
    # Added: For each group reverse order of data so plotting will
    # put first levels at top
    o <- sort.list(as.numeric(groups), decreasing = TRUE)
    groups <- groups[o]
#    for(g in levels(groups)) {
#      i <- groups == g
#      o[i] <- rev(o[i])
#    }
    x      <- x[o, , drop=FALSE]  # ascending within region
    labels <- labels[o]
    if(length(auxdata)) auxdata <- auxdata[o]
    # End added
    # groups <- groups[o]  (put earlier)
    color  <- rep(color,  length.out = length(groups))[o]
    lcolor <- rep(lcolor, length.out = length(groups))[o]
    offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
    y      <- 1L:n + 2 * offset
    ylim <- range(0.5, y + 1.5)  # range(0, y + 2)
  }
  
  plot.window(xlim = xlim, ylim = ylim, log = "")
  lheight <- par("csi")
  if(length(labels)) {
    linch <- max(strwidth(labels, "inch", cex=cex.labels), na.rm = TRUE)
    loffset <- (linch + 0.1) / lheight
    # was line=loffset
    mtext(labels, side = 2, line = .1*loffset, at = y, adj = 1,
          col = color, las = 2, cex = cex.labels, ...)
  }
  abline(h = y, lty = "dotted", col = lcolor)
  if(length(auxtitle)) {
    upedge <- par('usr')[4]
    outerText(auxtitle,
              upedge + strheight(auxtitle, cex=cex) / 2,
              cex=cex)
  }
  gpos <- if(length(groups)) 
    rev(cumsum(rev(tapply(groups, groups, length)) + 2) - 1)
  if(length(auxdata) + length(auxgdata) > 0)
    outerText(c(auxdata, auxgdata), c(y, if(length(auxgdata)) gpos),
              cex=cex.auxdata)
    
  for(i in 1:nc)
    points(x[,i], y, pch = pch[i], col = color, bg = bg)
  
  if(length(groups)) {
    ginch <- max(strwidth(glabels, "inch", font=groupfont,
                          cex=cex.group.labels),
                 na.rm = TRUE)
    goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
    mtext(glabels, side = 2, line = .2, at = gpos, adj = 1, # was adj=0
          col = gcolor, las = 2, cex = cex.group.labels, font=groupfont, ...)
    if (length(gdata)) {
      abline(h = gpos, lty = "dotted")
      if(is.matrix(gdata))
        for(j in 1:ncol(gdata))
          points(gdata[, j], gpos, pch=gpch[j], col=gcolor, bg=bg, ...)
      else
        points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
               ...)
    }
  }
  if(length(axisat)) axis(1, at=axisat, labels=axislabels)
    else
      axis(1)
  box()
  title(main = main, xlab = xlab, ylab = ylab, ...)
  invisible()
}

dotchartp <-
  function (x, labels = NULL, groups = NULL, gdata = NULL,
            xlim = range(c(x, gdata), na.rm=TRUE), main=NULL,
            xlab = NULL, ylab = '', auxdata=NULL, auxtitle=NULL,
            auxgdata=NULL, auxwhere=c('right', 'hover'),
            symbol='circle', col=colorspace::rainbow_hcl,
            legendgroup=NULL,
            axisat=NULL, axislabels=NULL, sort=TRUE, digits=4, dec=NULL,
            height=NULL, width=700, layoutattr=FALSE, showlegend=TRUE,
            ...) 
{
  if (!requireNamespace("plotly", quietly=TRUE))
    stop("This function requires the 'plotly' package.")

  auxwhere <- match.arg(auxwhere)
  
  fmt <- if(length(dec)) function(x) format(round(x, dec))
         else
           function(x) format(x, digits=digits)

  mu <- markupSpecs$html
  lspace <- mu$lspace

  if (! is.numeric(x)) 
    stop("'x' must be a numeric vector or matrix")
  x    <- as.matrix(x)
  n    <- nrow(x)
  nc   <- ncol(x)

  symbol <- rep(symbol, length.out=nc)
  col <- if(length(col)) {
           if(! is.function(col)) col
           else col(ncol(x))
         }
  col    <- rep(col, length.out=nc)
  
  if(length(gdata)) {
    gdata <- as.matrix(gdata)
    if(ncol(gdata) != nc) stop('gdata must have same columns as x')
  }

  if(! length(labels)) labels <- rownames(x)
  if(! length(labels)) stop('labels not defined')
  if(length(groups)) groups <- as.factor(groups)
  glabels <- levels(groups)
  
  groups.pres <- length(groups) > 0
  if(groups.pres && is.character(sort))
    warning('specifying sort does not makes sense with groups present')
  if(! groups.pres) {
    y <- n : 1
    if(is.logical(sort)) sort <- if(sort) 'descending' else 'none'
    if(sort != 'none') {
      o <- order(if(sort == 'ascending') x[, 1] else - x[, 1])
      x       <- x[o, , drop=FALSE]
      labels  <- labels[o]
      if(length(auxdata))
        auxdata <- if(is.matrix(auxdata))
                     auxdata[o,, drop=FALSE] else auxdata[o]
    }
  } else {    # groups present
      if(is.character(sort) || sort) {
        o <- if(is.character(sort)) {
               if(sort == 'ascending') order(x[, 1])
               else
                 order(-x[, 1])
             } else order(as.integer(groups)) ###, as.integer(labels))
        groups  <- groups[o]
        x       <- x[o, , drop=FALSE]
        labels  <- labels[o]
        if(length(auxdata))
          auxdata <- if(is.matrix(auxdata))
                       auxdata[o,, drop=FALSE] else auxdata[o]
      }
      lgroups <- Lag(as.character(groups))
      lgroups[1] <- 'NULL'
      first.in.group <- groups != lgroups
      y  <- cumsum(1 + 1.5 * first.in.group)
      yg <- y[first.in.group] - 1
      y  <- -y
      yg <- -yg
    }    # end groups present

  X <- x[, 1]
  tly <- y
  auxd <- NULL
  auxh <- auxwhere == 'hover'
  auxt <- if(length(auxtitle) && auxtitle != '')
#            ifelse(auxh, paste0(auxtitle, '<br>'), paste0(auxtitle, ':'))
  paste0(auxtitle, ':')
            else ''
  if(auxh)
    auxd <- if(length(auxdata))
              paste0(auxt,
                     if(is.matrix(auxdata)) auxdata[, 1] else auxdata)
            else rep('', length(X))
  
  if(length(gdata) || (auxh && length(auxgdata))) {
    X    <- c(X, if(length(gdata)) gdata[, 1] else rep(NA, length(auxgdata)))
    tly  <- c(tly, yg)
    if(auxh) auxd <- c(auxd,
                       if(length(auxgdata))
                         paste0(auxt,  # was lspace after auxt,
                                if(is.matrix(auxgdata)) auxgdata[, 1]
                                else auxgdata)
                        else rep('', length(yg)))
    }
  nx <- if(nc == 1) '' else colnames(x)[1]
  ht <- if(nx == '')   fmt(X)
        else paste(nx, '<br>', fmt(X))
  if(auxh && any(auxd != '')) ht <- paste0(ht, '<br>', auxd) # <br> was lspace
  d <- data.frame(X, y=tly, ht=ht)

  if(length(height) && height == 'auto')
    height <- plotlyParm$heightDotchart(n)
  auto <- .Options$plotlyauto
  if(length(auto) && auto) height <- width <- NULL
  p <- plotly::plot_ly(d, x=~ X, y=~ y, mode='markers', type='scatter',
                       marker=list(symbol=symbol[1], color=col[1]),
                       text = ~ ht,
                       hoverinfo = 'text',
                       name=nx,
                       legendgroup=if(length(legendgroup)) legendgroup[1],
                       width = width, height=height)

  if(nc > 1)
    for(i in 2 : nc) {
      X   <- x[, i]
      tly <- y
      if(length(gdata)) {
        X   <- c(X, gdata[, i])
        tly <- c(tly, yg)
      }
      ax <- if(length(auxdata) && is.matrix(auxdata)) auxdata[, i] else ''
      d <- data.frame(X=X, y=tly,
                      ht=paste0(colnames(x)[i], '<br>',
                                fmt(X), lspace, ax))

      p <- plotly::add_markers(p, data=d, x=~ X, y=~ y, #mode='markers',
                             marker=list(symbol=symbol[i], color=col[i]),
                             text = ~ ht, hoverinfo='text',
                             legendgroup=if(length(legendgroup)) legendgroup[i],
                             name=colnames(x)[i])
    }

  dx    <- 0.1 * diff(xlim)

  lenaux <- length(auxdata) + length(auxgdata)
  if(auxwhere == 'right' && lenaux) {
    yb <- tb <- NULL
    if(length(auxdata)) {
      yb <- y
      tb <- auxdata
    }
    if(groups.pres && length(auxgdata)) {
      yb <- c(yb, yg)
      tb <- c(tb, auxgdata)
    }
    if(length(auxtitle)) {
      yb <- c(yb, min(yb) - 2)
      tb <- c(tb, auxtitle)
    }
    if(length(auxgdata)) {
      yb <- c(yb, yg)
      tb <- c(tb, paste('<b>', auxgdata, '</b>', sep=''))
      }
    z <- data.frame(xb=xlim[2] + dx, yb, tb)
    p <- plotly::add_text(p, data=z, x=~ xb, y=~ yb, text=~ tb,  # mode='text',
                          textposition='left',
                          textfont=list(size=10), hoverinfo='none', name='')
  }

  if(length(axisat)) {tlx <- axisat; ttx <- axislabels}
  else {
    tlx  <- pretty(xlim, 10)
    tlxs <- pretty(xlim, 5)
    ttx  <- format(tlx)
    for(j in 1 : length(tlx))
      if(! any(abs(tlxs - tlx[j]) < 1e-10)) ttx[j] <- ''
    }
  
  tly <-y
  tty <- as.character(labels)
  if(groups.pres) {
    tly <- c(tly, yg)
    tty <- c(tty, paste('<b>', glabels, '</b>', sep=''))
  }
  if(! length(ylab)) ylab <- ''
  tty <- ifelse(nchar(tty) >= 40,  mu$smaller2(tty),
           ifelse(nchar(tty) > 20, mu$smaller(tty),  tty))
  leftmargin <- plotlyParm$lrmargin(tty)
  rx <- if(auxwhere == 'right' && lenaux > 0) dx else dx / 2

  ylim <- c(min(y) - .15, max(y) + 1.5)
  lo <- list(title=main,
             xaxis=list(title=xlab,
                        range=c(xlim[1] - 0.2 * dx,
                                xlim[2] + rx),
                        zeroline=FALSE,
                        tickvals=tlx, ticktext=ttx),
             yaxis=list(title=ylab, range=ylim,
                        zeroline=FALSE,
                        tickvals=tly, ticktext=tty),
#             width=width,
#             height=if(length(height) && height == 'auto')
#                      plotlyParm$heightDotchart(n) else height,
             autosize=(length(width) + length(height)) == 0,
             margin=list(l=leftmargin, t=5),
             showlegend=showlegend)

  if(layoutattr) {
    attr(p, 'layout') <- lo
    return(p)
  }
  plotly::layout(p,
                 title = main,
                 xaxis = list(title=xlab,
                            range=c(xlim[1] - 0.2 * dx,
                                    xlim[2] + rx),
                            zeroline=FALSE,
                            tickvals=tlx, ticktext=ttx),
                 yaxis = list(title=ylab, range=ylim,
                            zeroline=FALSE,
                            tickvals=tly, ticktext=tty),
 #                width = width,
 #                height= if(length(height) && height == 'auto')
 #                         plotlyParm$heightDotchart(n) else height,
                 # autosize=(length(width) + length(height)) == 0,
                 margin = list(l=leftmargin, t=5),
                 legendgroup=legendgroup, showlegend = showlegend)
}

summaryD <- function(formula, data=NULL, fun=mean, funm=fun,
                     groupsummary=TRUE, auxvar=NULL, auxtitle='',
                     auxwhere=c('hover', 'right'),
                     vals=length(auxvar) > 0, fmtvals=format,
                     symbol=if(use.plotly) 'circle' else 21,
                     col=if(use.plotly) colorspace::rainbow_hcl else 1:10,
                     legendgroup=NULL,
                     cex.auxdata=.7, xlab=v[1], ylab=NULL,
                     gridevery=NULL, gridcol=gray(.95), sort=TRUE, ...) {

  use.plotly <- grType() == 'plotly'
  auxwhere <- match.arg(auxwhere)

  if(! missing(fmtvals)) vals <- TRUE
  data <- if(! length(data)) environment(formula)
   else                      list2env(data, parent=environment(formula))
  if(length(auxvar) && is.character(auxvar) && missing(auxtitle))
    auxtitle <- auxvar
  v   <- all.vars(formula)
  m   <- length(v) - 1
  yn  <- v[1]; xn <- v[-1]
  two <- length(xn) == 2
  y   <-         get(yn,    envir=data)
  x1  <-         get(xn[1], envir=data)
  x2  <- if(two) get(xn[2], envir=data)

  s   <- summarize(y, if(two) llist(x1, x2) else llist(x1), fun,
                   type='matrix', keepcolnames=TRUE)
  ## if(is.matrix(s$y)) colnames(s$y) <- colnames(y)

  cx1 <- if(is.factor(s$x1)) as.integer(s$x1)
         else
           s$x1
  yy <- if(is.matrix(s$y)) s$y[, 1, drop=FALSE] else s$y
  if(sort) s <- if(two) s[order(cx1, - yy), ] else s[order(- yy), ]

  auxd <- function(z) {
    sy <- z$y
    if(length(auxvar)) {
      if(! is.matrix(sy))
        stop('auxvar is only used when fun returns > 1 statistic')

      f <- if(vals) fmtvals(sy[, auxvar])
      sy <- if(is.numeric(auxvar)) sy[, -auxvar, drop=FALSE]
      else
        sy[, setdiff(colnames(sy), auxvar), drop=FALSE]
    }
    else
      f <- if(vals) fmtvals(if(is.matrix(sy)) sy[, 1] else sy)
    list(sy=sy, fval=f)   # sy = remaining y, fval = formatted auxvar
  }

  z <- auxd(s)
  if(two) {
    if(groupsummary) {
      s2 <- summarize(y, llist(x1), funm, type='matrix', keepcolnames=TRUE)
      z2 <- auxd(s2)
    }
    z  <- auxd(s)

    col <- if(length(col)) {
             if(! is.function(col)) col
             else
               col(if(is.matrix(z$sy)) ncol(z$sy) else 1)
             }


    ## if already sorted (group variable order first) don't re-sort
    ## sort causes problems to dotchart3

    res <- if(use.plotly)
             dotchartp(z$sy, s$x2, groups=s$x1,
                       auxdata=z$fval, auxtitle=if(vals) auxtitle,
                       auxwhere=auxwhere,
                       cex.auxdata=cex.auxdata,
                       gdata   =if(groupsummary) z2$sy,
                       auxgdata=if(groupsummary) z2$fval,
                       xlab=xlab, ylab=ylab, symbol=symbol, col=col,
                       legendgroup=legendgroup, sort=FALSE, ...)
           else
             dotchart3(z$sy, s$x2, groups=s$x1,
                       auxdata=z$fval, auxtitle=if(vals) auxtitle,
                       cex.auxdata=cex.auxdata,
                       gdata   =if(groupsummary) z2$sy,
                       auxgdata=if(groupsummary) z2$fval,
                       xlab=xlab, ylab=ylab, pch=symbol, ...) 
  }
  else
    res <- if(use.plotly)
             dotchartp(z$sy, s$x1, auxdata=z$fval,
                       auxtitle=if(vals) auxtitle,
                       auxwhere=auxwhere,
                       cex.auxdata=cex.auxdata, xlab=xlab, ylab=ylab,
                       symbol=symbol, col=col, legendgroup=legendgroup,
                       sort=FALSE, ...)
           else
             dotchart3(z$sy, s$x1, auxdata=z$fval,
                       auxtitle=if(vals) auxtitle, pch=symbol,
                       cex.auxdata=cex.auxdata, xlab=xlab, ylab=ylab, ...)

  if(! use.plotly && length(gridevery)) {
    xmin <- par('usr')[1]
    xmin <- ceiling(xmin / gridevery) * gridevery
    xmax <- if(length(xn) == 1) max(s$y, na.rm=TRUE)
    else
      max(c(s$y, s2$y), na.rm=TRUE)
    abline(v=seq(xmin, xmax, by=gridevery), col=gridcol)
  }

  if(use.plotly) res else invisible(res)
}


summaryDp <-
  function(formula,
           fun=function(x) c(Mean=mean(x, na.rm=TRUE),
                             N=sum(! is.na(x))),
           overall=TRUE, xlim=NULL, xlab=NULL,
           data=NULL, subset=NULL, na.action=na.retain,
           ncharsmax=c(50, 30), digits=4, ...) {
  
    Y <- if(length(subset))
           model.frame(formula, data=data, subset=subset, na.action=na.action)
         else
           model.frame(formula, data=data, na.action=na.action)
    X    <- Y[-1]
    y    <- Y[[1]]
    
    swr <- function(w, ...) 
      sapply(strwrap(w, ..., simplify=FALSE),
             function(x) paste(x, collapse='<br>'))
    addbr <- markupSpecs$html$addBreak
    width <- ncharsmax[1]; minbreak <- ncharsmax[2]

    if(! length(xlab)) xlab <- swr(label(y, default=names(Y)[1]), width=width)

    major <- minor <- ht <- character(0)
    x     <- numeric(0)

    funlabs <- names(fun(y))
    nx <- names(X)
    if(overall) nx <- c(nx, 'Overall')
    
    for(v in nx) {
      if(v == 'Overall') {
        by <- rep('Overall', length(y))
        bylab <- 'Overall'
      } else {
        by     <- X[[v]]
        bylab  <- addbr(label(by, default=v), minbreak=minbreak)
      }
      s <- summarize(y, by, fun)
      i <- order(- s[, 2])
      s <- s[i, ]
      m <- s[, 2]
      faux <- paste0(funlabs[1], ': ', format(m, digits=digits))
      if(NCOL(s) > 2) {
        j <- 0
        aux <- s[-(1:2)]
        for(a in names(aux)) {
          j <- j + 1
          faux <- paste0(faux, '<br>', funlabs[j + 1], ': ',
                             format(aux[[a]], digits=digits))
          }
      }

      major <- c(major, rep(bylab, length(m)))
      minor <- c(minor, if(v == 'Overall') '' else as.character(s[, 1]))
      ht    <- c(ht, faux)
      x     <- c(x, unname(m))
    }

    if(! length(xlim)) {
      r    <- range(x)
      xlim <- r + c(-1, 1) * diff(r) / 20
    }
    
    dotchartpl(x, major, minor, htext=ht, xlim=xlim, xlab=xlab, ...)
  }
