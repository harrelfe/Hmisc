#' Pseudomedian
#' 
#' Uses fast Fortran code to compute the pseudomedian of a numeric vector.  The pseudomedian is the median of all possible midpoints of two observations.  The pseudomedian is also called the Hodges-Lehmann one-sample estimator.  The Fortran code is was originally from JF Monahan, and was converted to C++ in the `DescTools` package.  It has been converted to Fortran 2018 here.
#' @title pMedian
#' @param x a numeric vector
#' @param na.rm set to `TRUE` to exclude `NA`s before computing the pseudomedian
#'
#' @return a scalar numeric value
#' @export
#' @md
#' @seealso <https://dl.acm.org/toc/toms/1984/10/3/>, <https://www4.stat.ncsu.edu/~monahan/jul10/>
#' @examples
#' x <- c(1:4, 10000)
#' pMedian(x)
#' # Compare with brute force calculation and with wilcox.test
#' w <- outer(x, x, '+')
#' median(w[lower.tri(w, diag=TRUE)]) / 2
#' wilcox.test(x, conf.int=TRUE)

pMedian <- function(x, na.rm = FALSE) {
  if(na.rm) x <- x[! is.na(x)]
  n <- length(x)
  if(n == 0) return(NA_real_)
  if(n == 1) return(as.double(x))
  .Fortran(F_hlqest, as.double(sort(x)), as.double(runif(1000)), as.integer(n), result=double(1))$result

}
