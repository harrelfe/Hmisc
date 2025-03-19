#' rmClose
#' 
#' Remove close values from a numeric vector that are not at the outer limtis.  This is useful for removing axis breaks that overlap when plotting.
#'
#' @param x a numeric vector with no `NA`s
#' @param minfrac minimum allowed spacing between consecutive ordered `x`, as a fraction of the range of `x`
#'
#' @returns a sorted numeric vector of non-close values of `x`
#' @export
#' @md
#' @author Frank Harrell
#' @examples
#' rmClose(c(1, 2, 4, 47, 48, 49, 50), minfrac=0.07)
rmClose <- function(x, minfrac=0.05) {
  x <- unique(sort(x))
  mindist <- minfrac * diff(range(x)) 
  n <- length(x)
  selected      <- x[1]
  last_selected <- x[1]  # Always select the first point
  
  for (i in 2 : n) {
    if (x[i] - last_selected >= mindist) {
      selected <- c(selected, x[i])
      last_selected <- x[i]
    }
  }
  return(selected)
}

# Code not used:
 # repeat {
 #   prn(x)
 #   n <- length(x)
 #   if(n < 3) return(x)
 #   if(x[n - 1] > x[n] - mindist) {
 #     x <- x[- (n - 1)]
 #     next
 #   }
 #   gaps <- diff(c(- mindist * 2, x))
 #   mind <- min(gaps[- c(1, n)])
 #   prn(gaps); prn(mind)
 #   if(mind >= mindist) return(x)
 #   candidates <- setdiff(which(gaps == mind), c(1, n))
 #   prn(candidates)
 #   x <- x[- candidates[1]]
 # }
 #}
