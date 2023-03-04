## Computes rank correlation measures between a variable X and a possibly
## censored variable Y, with event/censoring indicator EVENT
## Rank correlation is extension of Somers' Dxy = 2(Concordance Prob-.5)
## See Harrell et al JAMA 1984(?)
## Set outx=T to exclude ties in X from computations (-> Goodman-Kruskal
##  gamma-type rank correlation)

rcorr.cens <- function(x, S, outx=FALSE) {
  if(inherits(S, 'Surv')) {
    if(attr(S, 'type') != 'right')
      stop('only handles right censored times')
  } else S <- cbind(S, rep(1, length(S)))
  
  y <- S[,1]
  event <- S[,2]
  if(length(y)!=length(x))
    stop("y must have same length as x")
  
  miss <- is.na(x) | is.na(y) | is.na(event)
  nmiss <- sum(miss)
  if(nmiss>0) {
    miss <- !miss
    x <- x[miss]
    y <- y[miss]
    event <- event[miss]
  }
  
  n <- length(x)
  ne <- sum(event)
  storage.mode(x) <- "double"
  storage.mode(y) <- "double"
  storage.mode(event) <- "logical"

  z <-
    .Fortran(F_cidxcn,x,y,event,length(x),nrel=double(1),nconc=double(1),
             nuncert=double(1),
             c.index=double(1),gamma=double(1),sd=double(1),as.logical(outx))
  r <- c(z$c.index,z$gamma,z$sd,n,nmiss,ne,z$nrel,z$nconc,z$nuncert)
  names(r) <- c("C Index","Dxy","S.D.","n","missing","uncensored",
                "Relevant Pairs",
                "Concordant","Uncertain")
  r
}
