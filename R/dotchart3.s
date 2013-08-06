dotchart3 <-
  function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
            pch = 21, gpch = 21, bg = par("bg"), color = par("fg"),
            gcolor = par("fg"), lcolor = "gray",
            xlim = range(c(x, gdata), na.rm=TRUE), main = NULL, 
            xlab = NULL, ylab = NULL, auxdata=NULL, auxtitle=NULL,
            auxgdata=NULL,
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
  if (is.null(glabels)) {
    ginch <- 0
    goffset <- 0
  }
  else
    {
      ginch <- max(strwidth(glabels, "inch", cex=cex.group.labels,
                            font=groupfont),
                   na.rm = TRUE)
      goffset <- 0.4
    }
  if(length(labels) + length(glabels) > 0) {
    nmai     <- par("mai")
    nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 0.1
    if(length(auxdata) + length(auxgdata) > 0)
      nmai[4L] <- .2 + 1.1 * max(strwidth(c(auxtitle, auxdata, auxgdata),
                                          "inch", cex=cex.auxdata))
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
  auxdata <- c(auxdata, auxgdata)
  
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
  if(length(auxdata)) outerText(auxdata, c(y, gpos), cex=cex.auxdata)
    
  for(i in 1:nc)
    points(x[,i], y, pch = pch[i], col = color, bg = bg)
  if(length(groups)) {
    ginch <- max(strwidth(glabels, "inch", font=groupfont,
                          cex=cex.group.labels),
                 na.rm = TRUE)
    goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
    # was line=goffset
    mtext(glabels, side = 2, line = .2, at = gpos, adj = 1, # was adj=0
          col = gcolor, las = 2, cex = cex.group.labels, font=groupfont, ...)
    if (!is.null(gdata)) {
      abline(h = gpos, lty = "dotted")
      points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
             ...)
    }
  }
  axis(1)
  box()
  title(main = main, xlab = xlab, ylab = ylab, ...)
  invisible()
}

summaryD <- function(formula, data=NULL, fun=function(x) length(x),
                     auxtitle='N', cex.auxdata=.7, xlab='N',
                     gridevery=NULL,
                     sort=TRUE, ...) {
  if(!length(data)) data <- parent.env()
  v <- all.vars(formula)
  m <- length(v) - 1
  yn <- v[1]; xn <- v[-1]
  s <- summarize(data[[yn]], data[xn], fun, stat.name='Y')
  if(sort) s <- s[order(s$Y), ]
  if(length(xn) == 2) {
    s2 <- summarize(data[[yn]], data[xn[1]], fun, stat.name='Y')
    dotchart3(s$Y, s[[xn[2]]], groups=s[[xn[1]]],
                     auxdata=s$Y, auxtitle=auxtitle, cex.auxdata=cex.auxdata,
                     gdata=s2$Y, auxgdata=s2$Y, xlab=xlab, ...)
            }
  else
    dotchart3(s$Y, s[[xn]], auxdata=s$Y, auxtitle=auxtitle,
              cex.auxdata=cex.auxdata, xlab=xlab, ...)

    if(length(gridevery)) {
    xmin <- par('usr')[1]
    xmin <- ceiling(xmin/gridevery)*gridevery
    xmax <- if(length(xn) == 1) max(s$Y, na.rm=TRUE)
    else max(c(s$Y, s2$Y), na.rm=TRUE)
    abline(v=seq(xmin, xmax, by=gridevery), col=gray(.95))
  }
}
