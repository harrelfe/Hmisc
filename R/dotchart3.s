dotchart3 <-
  function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
            pch = 21, gpch = 21, bg = par("bg"), color = par("fg"),
            gcolor = par("fg"), 
            lcolor = "gray", xlim = range(x[is.finite(x)]), main = NULL, 
            xlab = NULL, ylab = NULL, auxdata=NULL, auxtitle=NULL,
            cex.labels = cex, cex.group.labels = cex.labels*1.25,
            groupfont=2, ...) 
{
  opar <- par("mai", "mar", "cex", "yaxs")
  on.exit(par(opar))
  par(cex = cex, yaxs = "i")
  if (!is.numeric(x)) 
    stop("'x' must be a numeric vector or matrix")
  ## n <- length(x)
  x <- as.matrix(x)
  n <- nrow(x)
  nc <- ncol(x)
  pch  <- rep(pch,  length=nc)
  
  if(!length(labels)) labels <- rownames(x)
#  if (is.matrix(x)) {
#    if (is.null(labels)) 
#      labels <- rownames(x)
#    if (is.null(labels)) 
#      labels <- as.character(1L:nrow(x))
    ## labels <- rep(labels, length.out = n)
    ## if (is.null(groups)) 
    ##   groups <- col(x, as.factor = TRUE)
  glabels <- levels(groups)

  plot.new()
  linch <- if (!is.null(labels)) 
    max(strwidth(labels, "inch"), na.rm = TRUE)
  else 0
  if (is.null(glabels)) {
    ginch <- 0
    goffset <- 0
  }
  else {
    ginch <- max(strwidth(glabels, "inch", cex=cex.group.labels, font=groupfont),
                 na.rm = TRUE)
    goffset <- 0.4
  }
  if (!(is.null(labels) && is.null(glabels))) {
    nmai <- par("mai")
    nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 0.1
    if(length(auxdata))
      nmai[4L] <- .2 + 1.1 * max(strwidth(c(auxtitle, auxdata), "inch",
                                      cex=cex))
    par(mai = nmai)
  }
  if (is.null(groups)) {
    o <- n:1L   # was 1L:n
    y <- o
    ylim <- c(.5, n + .5)  # c(0, n + 1)
  }
  else {
    # Added: For each group reverse order of data so plotting will
    # put first levels at top
    o <- sort.list(as.numeric(groups), decreasing = TRUE)
    groups <- groups[o]
    for(g in levels(groups)) {
      i <- groups == g
      o[i] <- rev(o[i])
    }
    x <- x[o,,drop=FALSE]
    if(length(auxdata)) auxdata <- auxdata[o]
    # End added
    # groups <- groups[o]  (put earlier)
    color <- rep(color, length.out = length(groups))[o]
    lcolor <- rep(lcolor, length.out = length(groups))[o]
    offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
    y <- 1L:n + 2 * offset
    
    ylim <- range(0.5, y + 1.5)  # range(0, y + 2)
  }
  plot.window(xlim = xlim, ylim = ylim, log = "")
  lheight <- par("csi")
  if (!is.null(labels)) {
    linch <- max(strwidth(labels, "inch", cex=cex.labels), na.rm = TRUE)
    loffset <- (linch + 0.1)/lheight
    labs <- labels[o]
    # was line=loffset
    mtext(labs, side = 2, line = .1*loffset, at = y, adj = 1, # was adj=0 
          col = color, las = 2, cex = cex.labels, ...)
  }
  abline(h = y, lty = "dotted", col = lcolor)
  if(length(auxtitle)) {
    upedge <- par('usr')[4]
    outerText(auxtitle,
              upedge + strheight(auxtitle, cex=cex)/2,
              cex=cex)
  }
  if(length(auxdata)) outerText(auxdata, y, cex=cex)
    
  for(i in 1:nc)
    points(x[,i], y, pch = pch[i], col = color, bg = bg)
  if (!is.null(groups)) {
    gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
                       2) - 1)
    ginch <- max(strwidth(glabels, "inch", font=groupfont, cex=cex.group.labels),
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

