#' rmClose
#' 
#' Remove close values from a numeric vector that are not at the outer limtis.  This is useful for removing axis breaks that overlap when plotting.
#'
#' @param x a numeric vector with no `NA`s
#' @param mindist minimum allowed spacing between consecutive ordered `x`
#'
#' @returns a sorted numeric vector of non-close values of `x`
#' @export
#' @md
#' @author Frank Harrell
#' @examples
#' rmClose(c(1, 2, 4, 47, 48, 49, 50), mindist=3)
rmClose <- function(x, mindist=diff(range(x)) / 20.) {
  x <- unique(sort(x))
  repeat {
    n <- length(x)
    if(n < 3) return(x)
    if(x[n - 1] > x[n] - mindist) {
      x <- x[- (n - 1)]
      next
    }
    gaps <- diff(c(- mindist * 2, x))
    mind <- min(gaps[- c(1, n)])
    if(mind >= mindist) return(x)
    candidates <- setdiff(which(gaps == mind), c(1, n))
    x <- x[- candidates[1]]
  }
}
