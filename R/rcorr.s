rcorr <- function(x, y, type=c("pearson","spearman"))
{
  type <- match.arg(type)

  if(!missing(y)) x <- cbind(x, y)
  
  x[is.na(x)] <- 1e50
  storage.mode(x) <- "double"
  
  p <- as.integer(ncol(x))
  if(p < 1) stop("must have >1 column")
  
  n <- as.integer(nrow(x))
  if(n < 5) stop("must have >4 observations")
  
  h <-
      .Fortran(F_rcorr, x, n, p, itype=as.integer(1+(type=="spearman")),
               hmatrix=double(p*p),   npair=integer(p*p),
               double(n), double(n),  double(n), double(n),
               double(n), integer(n))
  
  npair <- matrix(h$npair, ncol=p)
  h <- matrix(h$hmatrix, ncol=p)
  h[h > 1e49] <- NA
  nam <- dimnames(x)[[2]]
  dimnames(h) <- list(nam, nam)
  dimnames(npair) <- list(nam, nam)
  P <- matrix(2 * (1 - pt(abs(h) * sqrt(npair - 2) / sqrt(pmax(1 - h * h, 0.)),
                          npair - 2)), ncol=p)
  P[abs(h) == 1] <- 0
  diag(P) <- NA
  dimnames(P) <- list(nam, nam)
  structure(list(r=h, n=npair, P=P, type=type), class="rcorr")
}

print.rcorr <- function(x, ...)
{
  print(round(x$r,2))
  n <- x$n
  if(all(n == n[1,1]))
    cat("\nn=", n[1,1], "\n\n")
  else {
    cat("\nn\n")
    print(n)
  }
  
  cat("\nP\n")
  P <- x$P
  P <- ifelse(P < .0001, 0, P)
  p <- format(round(P, 4))
  p[is.na(P)] <- ""
  print(p, quote=FALSE)
  invisible()
}
