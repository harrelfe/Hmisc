score.binary <- function(..., fun=max, points=1:p, 
                         na.rm=funtext=='max', retfactor=TRUE)
{
  x <- list(...)
  p <- length(x)
  nam <- (as.character(sys.call())[-1])[1:p]
  x <- matrix(unlist(x), ncol=p)
  if(!missing(points)) {
    if(length(points)==1)
      points <- rep(points, p)
    if(length(points)!=p)
      stop('wrong length for points')
  }

  x <- x * rep(points, rep.int(nrow(x),p))
  funtext <- as.character(substitute(fun))
  if(funtext=='max' && !missing(points) && retfactor)
    warning('points do not matter for fun=max with retfactor=T\nas long as they are in ascending order')

  if(!missing(retfactor) && retfactor && funtext!='max')
    stop('retfactor=T only applies to fun=max')

  xna <- apply(x, 1, function(x) any(is.na(x)))
  funargs <- as.list(args(fun))
  funargs <- funargs[-length(funargs)]
  
  if(any(names(funargs) == "na.rm")) {
    x <- apply(x, 1, fun, na.rm=na.rm)
  } else {
    x <- apply(x, 1, fun)
  }

  if(!na.rm)
    x[x==0 & xna] <- NA

  if(retfactor && funtext=='max') 
    factor(x, c(0,points), c("none",nam))
  else x
}
