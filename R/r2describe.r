#' Summarize Strength of Relationships Using R-Squared From Linear Regression
#'
#' Function to use `leaps::regsubsets()` to briefly describe which variables more strongly predict another variable.  Variables are in a numeric matrix and are assumed to be transformed so that relationships are linear (e.g., using `redun()` or `transcan()`.)
#' 
#' @title r2describe
#' @param x numeric matrix with 2 or more columns
#' @param nvmax maxmum number of columns of x to use in predicting a given column
#'
#' @return nothing
#' @export
#' @md
#' @author Frank Harrell
#'
#' @examples
#' \dontrun{
#' r <- redun(...)
#' r2describe(r$scores)
#' }
r2describe <- function(x, nvmax=10) {
    if(! requireNamespace('leaps', quietly=TRUE))
    stop('You must install the leaps package to use r2describe')
   
   if(! is.numeric(x) || ! is.matrix(x)) 
    stop('x must be a numeric matrix')
    if(ncol(x) < 2) stop('x must have at least 2 columns')

    p <- ncol(x)
    nam <- colnames(x)

    cat('\nStrongest Predictors of Each Variable With Cumulative R^2\n')
 
    for(k in 1 : p) {
      fchar <- capture.output(  # don't allow regular output
        f <- leaps::regsubsets(x[, -k], x[, k], method='forward',
                               nbest=1, nvmax=min(p - 1, nvmax)))
      s <- summary(f)
      w <- s$which[, -1, drop=FALSE]   # omit intercept
      xnm <- colnames(w)
      xadded <- character(0)
      for(l in 1 : nrow(w)) {
        varnow <- xnm[w[l,]]
        varnew <- setdiff(varnow, xadded)
        xadded <- c(xadded, varnew)
        }
      rsq <- structure(s$rsq, names=xadded)
      l <- which(rsq >= 0.985)
      if(length(l)) rsq <- rsq[1 : min(l)]
      cat('\n', nam[k], '\n', sep='')
      fw <- character(0)
      xadded <- names(rsq)
      for(l in 1 : length(rsq))
        fw <- paste0(fw, if(l > 1) ' + ', xadded[l],
                         ' (', round(rsq[l], 3), ')')
    cat(strwrap(fw), sep='\n')
    }
    invisible()
}
