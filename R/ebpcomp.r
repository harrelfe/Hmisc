##' Computation of Coordinates of Extended Box Plots Elements
##'
##' For an extended box plots computes all the elements needed for plotting it.  This is typically used when adding to a `ggplot2` plot.
##' @title ebpcomp
##' @param x a numeric variable
##' @param qref quantiles for major corners
##' @param probs quantiles for minor corners
##' @return list with elements `segments`, `lines`, `points`, `points2`
##' @author Frank Harrell
##' @md
##' @examples
##' ebpcomp(1:1000)
ebpcomp <- function(x, qref=c(.5, .25, .75),
                    probs= c(.05, .125, .25, .375)) {
  w <- 1.

  x <- x[! is.na(x)]
  probs2 <- sort(c(probs, 1. - probs))

  m  <- length(probs)
  m2 <- length(probs2)
  j  <- c(1, sort(rep(2 : m2, 2)), - sort(- rep(1 : (m2 - 1),2)))
  z  <- c(sort(rep(probs, 2)),     - sort(- rep(probs[1 : (m - 1)], 2)))
  z  <- c(z, -z, probs[1])
  k  <- max(z)
  k  <- if(k > .48) .5 else k
  
  if(length(qref)) {
    size.qref <- pmin(qref, 1 - qref)
    size.qref[qref == .5] <- k
  }
  
  q  <- quantile(x, c(probs2, qref))
  qo <- quantile(x, c(0.01, 0.99))
  Segs <- if(length(qref)) list(x=q[-(1 : m2)],
                                y1= -w * size.qref / k,
                                y2=  w * size.qref / k)
  Lines <- list(x=q[j], y=w * z / k)
  Mean  <- list(x=mean(x), y=0, N=length(x))
  Extreme <- list(x=qo, y=0)
  return(list(segments=Segs, lines=Lines, points=Mean, points2=Extreme))
}

##' Compute Elements of a Spike Histogram
##'
##' Derives the line segment coordinates need to draw a spike histogram.  This is useful for adding elements to `ggplot2` plots.  Date/time variables are handled by doing calculations on the underlying numeric scale then converting back to the original class.  For them the left endpoint of the first bin is taken as the minimal data value instead of rounded using `pretty()`.
##' @title spikecomp
##' @param x a numeric variable
##' @param method specifies the binning and output method.  The default is `'tryactual'` and is intended to be used for spike histograms plotted in a way that allows for random x-coordinates and data gaps.  No binning is done if there are less than 100 distinct values and the closest distinct `x` values are distinguishable (not with 1/500th of the data range of each other).  Binning uses `pretty`.  When `trans` is specified to transform `x` to reduce long tails due to outliers, `pretty` rounding is not done, and `lumptails` is ignored.  `method='grid'` is intended for sparkline spike histograms drawn with bar charts, where plotting is done in a way that x-coordinates must be equally spaced.  For this method, extensive binning information is returned.  For either `'tryactual'` or `'grid'`, the default if `trans` is omitted is to put all values beyond the 0.01 or 0.99 quantiles into a single bin so that outliers will not create long nearly empty tails.  When `y` is specified, `method` is ignored.
##' @param lumptails the quantile to use for lumping values into a single left and a single right bin for two of the methods
##' @param normalize set to `FALSE` to not divide frequencies by maximum frequency
##' @param y a vector of frequencies corresponding to `x` if you want the (`x`, `y`) pairs to be taken as a possibly irregular-spaced frequency tabulation for which you want to convert to a regularly-spaced tabulation like `count='tabulate'` produces.  If there is a constant gap between `x` values, the original pairs are return, with possible removal of `NA`s.
##' @param trans a list with three elements: the name of a transformation to make on `x`, the transformation function, and the inverse transformation function.  The latter is used for `method='grid'`.  When `trans` is given `lumptails` is ignored.  `trans` applies only to `method='tryactual'`.
##' @param tresult applies only to `method='tryactual'`.  The default `'list'` returns a list with elements `x`, `y`, and `roundedTo`.  `method='segments'` returns a list suitable for drawing line segments, with elements `x, y1, y2`.  `method='roundeddata'` returns a list with elements `x` (non-tabulated rounded data vector after excluding `NA`s) and vector `roundedTo`.
##' @return when `y` is specified, a list with elements `x` and `y`.  When `method='tryactual'` the returned value depends on `tresult`.  For `method='grid'`, a list with elements `x` and `y` and scalar element `roundedTo` containing the typical bin width.  Here `x` is a character string.
##' @author Frank Harrell
##' @md
##' @examples
##' spikecomp(1:1000)
##' spikecomp(1:1000, method='grid')
spikecomp <- function(x, method=c('tryactual', 'simple', 'grid'),
                      lumptails=0.01, normalize=TRUE, y, trans=NULL,
                      tresult=c('list', 'segments', 'roundeddata')) {

  method  <- match.arg(method)
  tresult <- match.arg(tresult)
  
  cx <- intersect(class(x),
                  c("Date", "POSIXt", "POSIXct", "dates", "times", "chron"))
  isdot <- testDateTime(x, 'either')    # date, time, or date-time var.
  if(length(cx)) x <- unclass(x)
  reclass <-
    if(length(cx)) function(x) structure(x, class=cx) else function(x) x

  ## floor(x) accounting for inexact decimal arithmetic, e.g.
  ## (0.58 - 0.48) / 0.01 = 0.99999999999998, signif -> 10.0
  floors <- function(x) floor(signif(x, 12))
  
  tpres  <- length(trans)
  ftrans <- itrans <- function(x) x
  if(tpres) {
    ftrans <- trans[[2]]
    itrans <- trans[[3]]
    lumptails <- 0.0
  }

  if(! missing(y)) {
    i <- ! is.na(x + y)
    x <- x[i]; y <- y[i]
    } else x <- x[! is.na(x)]

  xo <- x           # original x before rounding/curtailing/trans
  x  <- ftrans(unclass(x))
  
  ## y is specified; re-align a grid
  if(! missing(y)) {
    if(length(unique(diff(sort(x)))) == 1)
      return(list(x=reclass(x), y=y))
    if(! all(y == round(y))) stop('y must be integer for spikecomp')
    p <- pretty(range(x), 100)
    r <- if(length(cx)) range(x) else range(p)
    delta <- p[2] - p[1]
    xg <- seq(r[1], r[2], by=delta)
    xi <- 1 + round((x - r[1]) / delta)
    if(any(xi < 1))
      warning('possible logic error in spikecomp when y was specified')
    f <- tabulate(rep(xi, y))
    l <- min(length(f), length(xg))
    return(list(x=reclass(itrans(xg[1:l])), y=f[1:l]))
  }
  
  ux      <- sort(unique(x))
  nd      <- length(ux)  # no. distinct x
  closest <- min(diff(ux))
  qu      <- quantile(x, c(lumptails, 1. - lumptails))
  ## ux curtailed by outer quantiles:
  uxc     <- if(lumptails == 0.) ux else ux[ux > qu[1] & ux < qu[2]]

  if(method == 'tryactual') {
    r <- range(x)
    d <- diff(r) / 100.
    if(tpres) p <- seq(r[1], r[2], d)
    else if(nd >= 100 || (nd > 20 && closest < d / 5.)) {
      p <- pretty(uxc, 100)
      d <- p[2] - p[1]
      r <- range(p)
    }
    ix <- 1 + floors((x - r[1]) / d)
    x  <- r[1] + (ix - 1) * d
    if(tresult == 'roundeddata')
      return(list(x = reclass(itrans(x)), roundedTo=d))

    tab <- table(x)
    y   <- unname(tab)
    if(normalize) y <- y / max(y)
    x   <- reclass(itrans(as.numeric(names(tab))))
    return(
    switch(tresult,
           list     = list(x = x, y = y, roundedTo = d),
           segments = list(x = x, y1 = rep(0., length(y)), y2 = y,
                           roundedTo = d) ) )
  }

  ## method = 'grid'

  p <- pretty(uxc, 100)
  r <- range(p)
  ## If a pretty limit on curtailed data is at or interior to a quantile,
  ## ignore that quantile.  Also ignore if the width of outer quantile
  ## interval is less than 1/20th of the inter-quantile range
  iqrwidth <- diff(qu)
  lshort <- min(xo) > qu[1] - iqrwidth / 20.
  rshort <- max(xo) < qu[2] + iqrwidth / 20.
  if(qu[1] >= r[1] || lshort) uxc <- c(uxc, ux[ux <= qu[1]])
  if(qu[2] <= r[2] || rshort) uxc <- c(uxc, ux[ux >= qu[2]])
  p <- pretty(uxc, 100)
  r <- range(p)
  d <- p[2] - p[1]
  ## Make last bin upper limit be inclusive
  mx   <- length(p) - 1
  ix   <- pmin(mx, 1 + floors((x - r[1]) / d))
  loc <- rep('main', length(ix))
  if(lumptails > 0.) {
    ## If outer quantile equals first or last p, don't
    ## make special outer intervals
    loc[! lshort & qu[1] < r[1] & xo <= qu[1]] <- 'left'
    loc[! rshort & qu[2] > r[2] & xo >= qu[2]] <- 'right'
  }
  iuse <- loc == 'main'
  ix <- ifelse(loc=='left', 1,
        ifelse(loc=='main', ix +          any(loc=='left'),
               mx + 1 +      any(loc=='left')) )
  freq <- tabulate(ix)
  
  gr <- function(x) {
    lu <- length(unique(x))
    if(lu <= 10) {
      tab <- table(if(isdot) x else signif(x, 6))
      paste(paste0(names(tab),
                   ifelse(tab == 1, '', paste0(' (', tab, ')'))),
            collapse='; ')
    }
    else {
      xr <- if(isdot) range(x) else signif(range(x), 6)
      if(xr[1] == xr[2]) xr[1]
      else paste0(xr[1], ' - ', xr[2], ' (',
                  lu, ' distinct)')
    }
  }
  
  class(xo)    <- cx
  fqu          <- if(isdot) itrans(qu) else signif(itrans(qu))
  class(fqu)   <- cx
  fp           <- if(isdot) itrans(p) else signif(itrans(p))
  class(fp)    <- cx
  xrange       <- rep('', length(freq))
  xrnz         <- as.vector(tapply(xo, ix, gr))
  xrange[freq != 0] <- xrnz
  closep <- rep(')', length(fp) - 1)
  lf <- any(loc == 'left')
  rt <- any(loc == 'right')
  if(! rt) closep[length(closep)] <- ']'
  bins <- c(if(lf)
              paste0('[', min(xo), ', ', fqu[1], ']'),
            paste0('[', fp[-length(fp)], ', ', fp[-1], closep),
            if(rt)
              paste0('[', fqu[2], ', ', max(xo), ']') )
  xrange <- paste0('Bin: ', bins, '<br>Observed:<br>', xrange)
  quf <- paste0('Q<sub>', c(lumptails, 1. - lumptails), '</sub>:',
                if(isdot) fqu else signif(fqu))
  if(lf) xrange[1] <- paste0(quf[1], '<br>', xrange[1])
  if(rt) xrange[length(xrange)] <-
           paste0(quf[2], '<br>', xrange[length(xrange)])
  if(length(xrange) != length(freq))
    stop('program logic error in spikecomp; lengths: ',
         length(xrange), ' ', length(freq))
  list(x=xrange, y=freq / (if(normalize) max(freq) else 1.),
       roundedTo=d)
  }
