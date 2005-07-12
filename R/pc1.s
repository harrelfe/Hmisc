pc1 <- function(x, hi)
{
  p <- ncol(x)
  x <-  x[!is.na(x %*% rep(1,p)),]
  xo <- x
  for(i in 1:p) {
    y <- x[,i]
    x[,i] <- (y-mean(y))/sqrt(var(y))
  }
  
  g <- prcomp(x)
  cat("Fraction variance explained by PC1:",format(g$sdev[1]^2/sum(g$sdev^2)),
      "\n\n")
  pc1 <- g$x[,1]
  
  f <- lsfit(xo, pc1)
  
  if(!missing(hi)) {
    if(sum(f$coef[-1]<0) >= p/2)
      pc1 <- -pc1
    
    r <- range(pc1)
    pc1 <- hi*(pc1-r[1])/diff(r)
    f <- lsfit(xo, pc1)
  }
  
  cat("Coefficients to obtain PC1:\n\n")
  print(f$coef)
  attr(pc1,"coef") <- f$coef
  invisible(pc1)
}
