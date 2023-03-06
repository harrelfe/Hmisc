##' Compute Coordinates of an Empirical Distribution Function
##'
##' For a numeric vector uses the R built-in `ecdf` function to compute
##' coordinates of the ECDF, with extension slightly below and above the
##' range of `x` by default.  This is useful for `ggplot2` where the ECDF may need to be transformed.  The returned object is suitable for creating stratified statistics using `data.table` and other methods.
##' @title ecdfSteps
##' @param x numeric vector, possibly with `NA`s that are ignored
##' @param extend a 2-vector do extend the range of x (low, high).  Set `extend=FALSE` to not extend `x`, or leave it missing to extend it 1/20th of the observed range on other side.
##' @return a list with components `x` and `y`
##' @author Frank Harrell
##' @md
##' @seealso [stats::ecdf()]
##' @examples
##' ecdfSteps(0:10)
##' \dontrun{
##' # Use data.table for obtaining ECDFs by country and region
##' w <- d[, ecdfSteps(z, extend=c(1,11)), by=.(country, region)]  # d is a DT
##' # Use ggplot2 to make one graph with multiple regions' ECDFs
##' # and use faceting for countries
##' ggplot(w, aes(x, y, color=region)) + geom_step() +
##'        facet_wrap(~ country)
##' }
ecdfSteps <- function(x, extend) {
  u <- sort(unique(x))
  if(missing(extend) || is.numeric(extend) || extend) {
    if(missing(extend)) {
      r <- range(u)
      eps <- diff(r) / 20.
      extend <- c(r[1] - eps, r[2] + eps)
    }
    u <- c(extend[1], u, extend[2])
  }
  list(x=u, y=ecdf(x)(u))
}
