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
##' Derives the line segment coordinates need to draw a spike histogram.  This is useful for adding elements to `ggplot2` plots.
##' @title spikecomp
##' @param x a numeric variable
##' @param count default is `"table"` to use `table` resulting in no zero cells; use `"tabulate"` to compute counts over a rigid grid, suitable for bar charts with sparklines.  
##' @param normalize set to `FALSE` to not divide frequencies by maximum frequency
##' @return a list with element `segments` which has elements `x`, `y1`, `y2` if `count='table'`, otherwise a list with elements `x` and `y`
##' @author Frank Harrell
##' @md
##' @examples
##' spikecomp(1:1000)
spikecomp <- function(x, count=c('table', 'tabulate'), normalize=TRUE) {
  count <- match.arg(count)
  x        <- x[! is.na(x)]
  ux       <- sort(unique(x))
  n.unique <- length(ux)
  
  if(count == 'tabulate' || (n.unique >= 100 ||
     (n.unique > 20 && 
      min(diff(ux)) < diff(range(x)) / 500))) {
    pret <- pretty(ux, if(n.unique >= 100 || count == 'tabulate') 100 else 500)
    incr <- pret[2] - pret[1]
    r    <- range(pret)
    xi   <- 1 + round((x - r[1]) / incr)
    x    <- r[1] + (xi - 1.) * incr
  }
  if(count == 'table') {
    f <- table(x)
    x <- as.numeric(names(f))
    y <- unname(f / (if(normalize) max(f) else 1.))
    return(list(segments=list(x=x, y1=0, y2=y)))
  }
  if(any(xi < 1))                  stop('program logic error 1')
  f <- tabulate(xi)
  if(length(f) > length(pret))     stop('program logic error 2')
  if(length(f) < length(pret) - 1) stop('program logic error 3')
  pret <- pret[1 : length(f)]
  list(x=pret, y=f / (if(normalize) max(f) else 1.))
}
