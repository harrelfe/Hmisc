deff <- function(y, cluster)
{
  ss <- function(x)
  {
    n <- length(x)
    xbar <- sum(x) / n
    sum((x - xbar)^2)
  }

  if(!is.factor(cluster)) cluster <- as.factor(cluster)
  
  cluster <- unclass(cluster)
  s <- !is.na(cluster + y)
  y <- y[s]
  cluster <- as.integer(cluster[s])
  n <- length(y)
  sst <- ss(y)
  sses <- tapply(y,cluster,ss)
  k  <- length(sses)
  R2 <- 1 - sum(sses) / sst
  Fstat  <- R2 * (n - k) / (1 - R2) / k
  g  <- (Fstat - 1.) * k / n
  rho <- if(R2 == 1.) 1. else g / (1. + g)
  ng <- table(cluster)
  B  <- sum(ng^2) / n
  deff <- 1 + (B - 1) * rho
  c(n=n, clusters=k, rho=rho, deff=deff)
}
