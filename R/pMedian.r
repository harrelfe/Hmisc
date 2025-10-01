#' Pseudomedian
#' 
#' Uses fast Fortran code to compute the pseudomedian of a numeric vector.  The pseudomedian is the median of all possible midpoints of two observations.  The pseudomedian is also called the Hodges-Lehmann one-sample estimator.  The Fortran code is was originally from JF Monahan, and was converted to C++ in the `DescTools` package.  It has been converted to Fortran 2018 here.  Bootstrap confidence intervals are optionally computed.
#' 
#' If n > 250,000 a random sample of 250,000 values of `x` is used to limit execution time.  For n > 1,000 only the percentile bootstrap confidence interval is computed.
#' 
#' Bootstrapping uses the Fortran subroutine directly, for efficiency.
#' 
#' @title pMedian
#' @param x a numeric vector
#' @param na.rm set to `TRUE` to exclude `NA`s before computing the pseudomedian
#' @param conf.int confidence level, defaulting to 0 so that no confidence limits are computed.  Set to a number between 0 and 1 to compute bootstrap confidence limits
#' @param B number of bootstrap samples if `conf.int > 0`
#' @param type type of bootstrap interval, defaulting to `'percentile'` for n >= 150 or `'bca'` for n < 150
#
#' @return a scalar numeric value if `conf.int = 0`, or a 3-vector otherwise, with named elements `estimate, lower, upper` and attribute `type`.  If the number of non-missing values is less than 5, `NA` is returned for both lower and upper limits.
#' @export
#' @md
#' @seealso <https://dl.acm.org/toc/toms/1984/10/3/>, <https://www4.stat.ncsu.edu/~monahan/jul10/>, <https://www.fharrell.com/post/aci/>
#' @examples
#' x <- c(1:4, 10000)
#' pMedian(x)
#' pMedian(x, conf.int=0.95)
#' # Compare with brute force calculation and with wilcox.test
#' w <- outer(x, x, '+')
#' median(w[lower.tri(w, diag=TRUE)]) / 2
#' wilcox.test(x, conf.int=TRUE)

pMedian <- function(x, na.rm = FALSE, conf.int=0, B=1000, type=c('percentile', 'bca')) {
  mtype <- missing(type)
  type  <- match.arg(type)
  if(na.rm) x <- x[! is.na(x)]
  n <- length(x)
  g <- function(z) if(conf.int == 0) z else c(estimate=z, lower=NA_real_, upper=NA_real_)
  if(n == 0) return(g(NA_real_))
  if(n == 1) return(g(as.double(x)))
  if(! na.rm && anyNA(x)) return(g(NA_real_))
  nmax <- 250000L
  if(n > nmax) {
    n <- nmax
    x <- sample(x, nmax)
    warning('n > 250000; pMedian is using a random sample of 250000 values')
  }
  w   <- as.double(runif(1000))
  n   <- as.integer(n)
  res <- .Fortran(F_hlqest, as.double(sort(x)), w, n,
                  result=double(1))$result
  if(conf.int == 0) return(res)
  if(n < 5) return(c(estimate=res, lower=NA, upper=NA))
  if(! requireNamespace('boot', quietly=TRUE))
    stop('conf.int > 0 requires the boot package to be installed')
  comp <- function(x, i)
    .Fortran(F_hlqest, as.double(sort(x[i])), w, n,
                  result=double(1))$result
  b  <- boot::boot(x, statistic=comp, R=B)
  if(mtype) type <- if(n >= 150) 'percentile' else 'bca'
  if(type == 'bca' && n > 1000) {
    warning('type="bca" is too slow for n > 1000; reverting to percentile method')
    type <- 'percentile'
  }
  cl <- boot::boot.ci(b, type=if(type == 'percentile') 'perc' else 'bca',
                      conf=conf.int)[[if(type == 'percentile') 'percent' else 'bca']]
  l <- length(cl)
  res <- c(estimate=res, lower=cl[l - 1], upper=cl[l])
  attr(res, 'type') <- type
  res
}
