## Spearman correlation test (p=1) or Spearman test extended by adding
## rank(x)^2 to model (p=2)
## F Harrell  30Sep90

spearman.test <- function(x,y,p=1)
{
  x <- as.numeric(x);
  y <- as.numeric(y)  ## 17Jul97

  if(length(x)!=length(y))
    stop("length of x must = length of y")

  nomiss <- !is.na(x+y)
  n <- sum(nomiss)
  if(n<3)
    stop("fewer than 3 non-missing x-y pairs")

  if(!(p==1 | p==2))
    stop("p must be 1 or 2")

  x <- x[nomiss]
  x <- rank(x)
  y <- y[nomiss]
  y <- rank(y)
  sst <- sum((y-mean(y))^2)
  if(p==2)
    x <- cbind(x,x^2)

  sse <- sum((lsfit(x,y)$residuals)^2)
  rsquare <- 1-sse/sst
  df2 <- n-p-1
  fstat <- rsquare/p/((1-rsquare)/df2)
  pvalue <- 1-pf(fstat,p,df2)
  x <- c(rsquare,fstat,p,df2,pvalue,n)
  names(x) <- c("Rsquare","F","df1","df2","pvalue","n")
  x
}
