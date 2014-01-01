dotchart3 <-
  function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
            pch = 21, gpch = pch, bg = par("bg"), color = par("fg"),
            gcolor = par("fg"), lcolor = "gray",
            xlim = range(c(x, gdata), na.rm=TRUE), main = NULL, 
            xlab = NULL, ylab = NULL, auxdata=NULL, auxtitle=NULL,
            auxgdata=NULL, axisat=NULL, axislabels=NULL,
            cex.labels = cex, cex.group.labels = cex.labels*1.25,
            cex.auxdata = cex, groupfont=2, ...) 
{
  opar <- par("mai", "mar", "cex", "yaxs")
  on.exit(par(opar))
  par(cex = cex, yaxs = "i")
  if (!is.numeric(x)) 
    stop("'x' must be a numeric vector or matrix")
  x    <- as.matrix(x)
  n    <- nrow(x)
  nc   <- ncol(x)
  pch  <- rep(pch,  length=nc)
  
  if(!length(labels)) labels <- rownames(x)
  if(!length(labels)) stop('labels not defined')
  if(length(groups)) groups <- as.factor(groups)
  glabels <- levels(groups)

  plot.new()
  linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
  if (!length(glabels)) {
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
  if (!length(groups)) {
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

summaryD <- function(formula, data=NULL, fun=mean, funm=fun,
                     groupsummary=TRUE, auxvar=NULL, auxtitle='',
                     vals=length(auxvar) > 0, fmtvals=format,
                     cex.auxdata=.7, xlab=v[1], ylab=NULL,
                     gridevery=NULL, gridcol=gray(.95), sort=TRUE, ...) {
  if(!missing(fmtvals)) vals <- TRUE
  if(!length(data)) data <- environment(formula)
  else data <- list2env(data, parent=environment(formula))
  if(length(auxvar) && is.character(auxvar) && missing(auxtitle))
    auxtitle <- auxvar
  v   <- all.vars(formula)
  m   <- length(v) - 1
  yn  <- v[1]; xn <- v[-1]
  two <- length(xn) == 2
  y   <-         get(yn,    envir=data)
  x1  <-         get(xn[1], envir=data)
  x2  <- if(two) get(xn[2], envir=data)
  
  s   <- summarize(y, if(two) llist(x1, x2) else llist(x1), fun, type='matrix')
  if(sort) s <- s[order(if(is.matrix(s$y)) s$y[, 1, drop=FALSE] else s$y), ]

  auxd <- function(z) {
    sy <- z$y
    if(length(auxvar)) {
      if(!is.matrix(sy))
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
      s2 <- summarize(y, llist(x1), funm, type='matrix')
      z2 <- auxd(s2)
    }
    z  <- auxd(s)
    dotchart3(z$sy, s$x2, groups=s$x1,
              auxdata=z$fval, auxtitle=if(vals) auxtitle,
              cex.auxdata=cex.auxdata,
              gdata   =if(groupsummary) z2$sy,
              auxgdata=if(groupsummary) z2$fval,
              xlab=xlab, ylab=ylab, ...)
  }
  else
    dotchart3(z$sy, s$x1, auxdata=z$fval,
              auxtitle=if(vals) auxtitle,
              cex.auxdata=cex.auxdata, xlab=xlab, ylab=ylab, ...)
  
  if(length(gridevery)) {
    xmin <- par('usr')[1]
    xmin <- ceiling(xmin/gridevery)*gridevery
    xmax <- if(length(xn) == 1) max(s$y, na.rm=TRUE)
    else
      max(c(s$y, s2$y), na.rm=TRUE)
    abline(v=seq(xmin, xmax, by=gridevery), col=gridcol)
  }
}
