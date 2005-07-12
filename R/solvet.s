#FEH version of solve with argument tol passed to qr
#8 Apr 91

solvet <- function(a, b, tol=1e-9)
{
  if(!is.list(a))
    a <- qr(a, tol=tol)

  if(a$rank < ncol(a$qr))
    stop("apparently singular matrix")

  if(missing(b)) {
    b <- a$qr
    db <- dim(b)
    if(diff(db))
      stop("matrix inverse only for square matrices")

    b[] <- rep(c(1, rep(0, db[1])), length = prod(db))
  }

  qr.coef(a, b)
}
