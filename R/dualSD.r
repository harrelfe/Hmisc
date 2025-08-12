#' Dual Standard Deviations
#'
#' Computes one standard deviation for the lower half of the distribution of a numeric vector and another SD for the upper half.  By default the center of the distribution for purposes of splitting into "halves" is the mean.  The user may override this with `center`.  When splitting into halves, observations equal to the `center` value are included in both subsets.  
#' 
#' The purpose of dual SDs is to describe variability for asymmetric distributions.  Symmetric distributions are also handled, though slightly less efficiently than a single SD does.
#' 
#' @param x a numeric vector
#' @param na.rm set to `TRUE` to find any `NA` values and remove them before computing SDs.
#' @param nmin the minimum number of non-`NA` obesrvations that must be present for two SDs to be computed.  If the mumber of non-missing values falls below `nmin`, the regular SD is duplicated in the result.
#' @param center center point for making the two subsets.  The sample mean is used to compute the two SDs no matter what is specified for `center`.
#'
#' @returns a 2-vector of SDs with names `bottom` and `top`
#' @md
#' @seealso [pMedian()]
#' @author Frank Harrell
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(20000)
#' sd(x)
#' dualSD(x)
#' y <- exp(x)
#' s1 <- sd(y)
#' s2 <- dualSD(y)
#' s1
#' s2
#' quantile(y, c(0.025, 0.975))
#' mean(y) + 1.96 * c(-1, 1) * s1
#' mean(y) + 1.96 * c(- s2['bottom'], s2['top'])
#' c(mean=mean(y), pseudomedian=pMedian(y), median=median(y))

dualSD <- function(x, na.rm=FALSE, nmin=10, center=xbar) {
  if(na.rm) x <- x[! is.na(x)]
  n <- length(x)
  if(n < nmin) {
    s <- sd(x)
    return(c(bottom=s, top=s))
  }

  xbar <- mean(x)
  s   <- function(x) sqrt(sum((x - xbar) ^ 2) / (length(x) - 1))
  a   <- s(x[x <= center])
  b   <- s(x[x >= center])
  return(c(bottom=a, top=b))
}
