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
##' @return a list with element `segments` which has elements `x`, `y1`, `y2` 
##' @author Frank Harrell
##' @md
##' @examples
##' spikecomp(1:1000)
spikecomp <- function(x) {
  x <- x[! is.na(x)]
  n.unique <- length(unique(x))
  if(n.unique >= 100 ||
     (n.unique > 20 && 
      min(diff(sort(unique(x)))) < diff(range(x)) / 500)) {
    pret <- pretty(x, if(n.unique >= 100) 100 else 500)
    dist <- pret[2] - pret[1]
    r    <- range(pret)
    x     <- r[1] + dist * round((x - r[1]) / dist)
  }
  f <- table(x)
  x <- as.numeric(names(f))
  y <- unname(f / max(f))
  list(segments=list(x=x, y1=0, y2=y))
}
