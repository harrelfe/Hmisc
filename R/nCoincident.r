##' Number of Coincident Points
##'
##' Computes the number of x,y pairs that are likely to be obscured in a regular scatterplot, in the sense of overlapping pairs after binning into `bins` x `bins` squares where `bins` defaults to 400.  `NA`s are removed first.
##' @title nCoincident
##' @param x numeric vector
##' @param y numeric vector
##' @param bins number of bins in both directions
##' @return integer count
##' @author Frank Harrell
##' @md
##' @examples
##' nCoincident(c(1:5, 4:5), c(1:5, 4:5)/10)
nCoincident <- function(x, y, bins=400) {
  i  <- ! is.na(x + y)   # exclude points missing on x or y
  x  <- x[i]
  y  <- y[i]
  rx <- range(x)
  ry <- range(y)
  x  <- round((x - rx[1]) / diff(rx) * 300)
  y  <- round((y - ry[1]) / diff(ry) * 300)
  z  <- paste(x, y)
  length(z) - length(unique(z))
}
