james.stein <- function(y, group)
{
  s <- !(is.na(y)|is.na(group))
  y <- y[s];
  group <- as.character(group[s])
  ## as.char -> unused levels OK
  k <- length(unique(group))
  if(k<3)
    stop("must have >=3 groups")
  
  stats <- function(w) {
    bar <- mean(w)
    ss  <- sum((w-bar)^2)
    n <- length(w)
    ##if(n<2)
    ##  stop("a group has n<2")
    
    c(n=length(w), mean=bar, ss=ss, var=ss/n/(n-1))
  }

  Z <- stats(y)
  st <- tapply(y, group, FUN=stats)
  nams <- names(st)
  z <- matrix(unlist(st),ncol=4,byrow=TRUE)
  ssb <- stats(z[,2])["ss"]
  shrink <- 1 - (k-3)*z[,4]/ssb
  shrink[z[,1]==1] <- 0
  shrink <- pmin(pmax(shrink,0),1)
  list(n=z[,1], mean=z[,2], 
       shrunk.mean=structure(Z["mean"]*(1-shrink)+shrink*z[,2], names=nams),
       shrink=shrink)
}
