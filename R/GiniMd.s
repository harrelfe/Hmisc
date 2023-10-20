## David HA (1968): Gini's mean difference rediscovered.  Biometrika 55 No. 3
## p. 573-575

## For binary x, is 2*n*p*(1-p)/(n-1) = approx 2*p*(1-p)
## For trinomial with value A (frequency a) B (freq b) C (freq c):
## 2*(ab|A-B| + ac|A-C|+bc|B-C|)/[n(n-1)]
## = 2n/(n-1) * [ PaPb|A-B| + PaPc|A-C| + PbPb|B-C| ]

GiniMd<- function(x, na.rm=FALSE) {
  if(na.rm) {
    k <- is.na(x)
    if(any(k)) x <- x[! k]
  }
  n <- length(x)
  if(n < 2 || any(is.na(x))) return(NA)
  w <- 4 * ((1 : n) - (n - 1) / 2) / n / (n - 1)
  # sum returns 0 if x is empty, so NA was returned above to intercept this
  sum(w * sort(x - mean(x)))  ## center for numerical stability only
}
