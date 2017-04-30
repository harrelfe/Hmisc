hoeffd <- function(x, y)
{
  phoeffd <- function(d, n)
  {
    d <- as.matrix(d); n <- as.matrix(n)
    b <- d + 1 / 36 / n
    z <- .5 * (pi ^ 4) * n * b
    zz <- as.vector(z)
    zz[is.na(zz)] <- 1e50   # so approx won't bark
 
    tabvals <- c(5297,4918,4565,4236,3930,
                 3648,3387,3146,2924,2719,2530,2355,
                 2194,2045,1908,1781,1663,1554,1453,
                 1359,1273,1192,1117,1047,0982,0921,
                 0864,0812,0762,0716,0673,0633,0595,
                 0560,0527,0496,0467,0440,0414,0390,
                 0368,0347,0327,0308,0291,0274,0259,
                 0244,0230,0217,0205,0194,0183,0173,
                 0163,0154,0145,0137,0130,0123,0116,
                 0110,0104,0098,0093,0087,0083,0078,
                 0074,0070,0066,0063,0059,0056,0053,
                 0050,0047,0045,0042,0025,0014,0008,
                 0005,0003,0002,0001)/10000

    P <- ifelse(z < 1.1 | z > 8.5,
                pmax(1e-8, pmin(1, exp(.3885037 -1.164879 * z))),
                matrix(approx(c(seq(1.1, 5, by=.05),
                                seq(5.5,8.5,by=.5)),
                              tabvals, zz)$y,
                       ncol=ncol(d)))

    dimnames(P) <- dimnames(d)
    P
  }
  
  if(!missing(y))
    x <- cbind(x, y)
  
  x[is.na(x)] <- 1e50
  storage.mode(x) <- "double"
  
  p <- as.integer(ncol(x))
  if(p < 1) stop("must have > 1 column")
  
  n <- as.integer(nrow(x))
  if(n<5) stop("must have >4 observations")

  h <-
      .Fortran(F_hoeffd, x, n, p, hmatrix=double(p*p), aad=double(p*p),
               maxad=double(p*p), npair=integer(p*p),
               double(n), double(n),  double(n), double(n), double(n))
  
  nam <- dimnames(x)[[2]]
  npair <- matrix(h$npair, ncol=p)
  aad <- maxad <- NULL
  aad <- matrix(h$aad, ncol=p)
  maxad <- matrix(h$maxad, ncol=p)
  dimnames(aad) <- dimnames(maxad) <- list(nam, nam)

  h <- matrix(h$hmatrix, ncol=p)
  h[h > 1e49] <- NA
  dimnames(h) <- list(nam, nam)
  dimnames(npair) <- list(nam, nam)
  P <- phoeffd(h, npair)
  diag(P) <- NA
  structure(list(D=30*h, n=npair, P=P, aad=aad, maxad=maxad), class="hoeffd")
}


print.hoeffd <- function(x, ...)
{
  cat("D\n")
  print(round(x$D,2))
  if(length(aad <- x$aad)) {
    cat('\navg|F(x,y)-G(x)H(y)|\n')
    print(round(aad,4))
  }
  if(length(mad <- x$maxad)) {
    cat('\nmax|F(x,y)-G(x)H(y)|\n')
    print(round(mad,4))
  }
  n <- x$n
  if(all(n == n[1,1]))
    cat("\nn=", n[1,1], "\n")
  else {
    cat("\nn\n")
    print(x$n)
  }
  
  cat("\nP\n")
  P <- x$P
  P <- ifelse(P < .0001, 0, P)
  p <- format(round(P, 4))
  p[is.na(P)] <- ""
  print(p, quote=FALSE)
  invisible()
}
