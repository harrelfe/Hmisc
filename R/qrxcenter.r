#' Mean-center a data matrix and QR transform it
#'
#' For a numeric matrix `x` (or a numeric vector that is automatically changed to a one-column matrix), computes column means and subtracts them from `x` columns, and passes this matrix to [base::qr()] to orthogonalize columns.  Columns of the transformed `x` are negated as needed so that original directions are preserved (which are arbitrary with QR decomposition).  Instead of the default `qr` operation for which sums of squares of column values are 1.0, `qrxcenter` makes all the transformed columns have standard deviation of 1.0.
#' @title qrxcenter
#' @param x a numeric matrix or vector with at least 2 rows
#' @param ... passed to [base::qr()]
#'
#' @return a list with components `x` (transformed data matrix), `R` (the matrix that can be used to transform raw `x` and to transform regression coefficients computed on transformed `x` back to the original space), `Ri` (transforms transformed `x` back to original scale except for `xbar`), and `xbar` (vector of means of original `x` columns`)
#' @export
#' @md
#'
#' @examples
#' set.seed(1)
#' age <- 1:10
#' country <- sample(c('Slovenia', 'Italy', 'France'), 10, TRUE)
#' x <- model.matrix(~ age + country)[, -1]
#' x
#' w <- qrxcenter(x)
#' w
#' # Reproduce w$x
#' sweep(x, 2, w$xbar) %*% w$R
#' # Reproduce x from w$x
#' sweep(w$x %*% w$Ri, 2, w$xbar, FUN='+')
#' # See also https://hbiostat.org/r/examples/gtrans/gtrans#sec-splinebasis
qrxcenter <- function(x, ...) {
  if(! is.matrix(x)) x <- is.matrix(x)
  d    <- dim(x)
  n    <- d[1]
  p    <- d[2]
  if(n <= 2) stop('requires n > 1')

  x    <- scale(x, center=TRUE, scale=FALSE)
  xbar <- as.vector(attr(x, 'scaled:center'))  
  QR   <- qr(x, ...)
  Q    <- qr.Q(QR)
  RR   <- qr.R(QR)
  sgns <- sign(diag(RR))
  # Each column of Q squared sums to 1; make sum of squares = n - 1
  # This makes SDs 1.0
  sn <- sqrt(n - 1)
  x     <- sweep(Q,  MARGIN = 2, STATS = sgns, FUN = `*`) * sn
  RR    <- sweep(RR, MARGIN = 1, STATS = sgns, FUN = `*`) / sn

  list(x=x, R = backsolve(RR, diag(p)), Ri = RR, xbar=xbar)
}



