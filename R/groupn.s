groupn <- function(x, y, m=150)
{
  s <- !is.na(x + y)
  x<-x[s]
  y<-y[s]
  i<-order(x)
  x<-x[i]
  y<-y[i]
  n<-length(x)
  if(n<m)
    stop("m<number of observations in groupn")
  
  start <- 1
  end <- m
  meanx <- NULL
  meany <- NULL
  while(end <= n) {
    meanx <- c(meanx,mean(x[start:end]))
    meany <- c(meany,mean(y[start:end]))
    start <- start+m
    end <- end+m
  }
  
  if(end > n) {
    meanx <- c(meanx,mean(x[n-m+1:n]))
    meany <- c(meany,mean(y[n-m+1:n]))
  }
  
  return(list(x=meanx,y=meany))
}
