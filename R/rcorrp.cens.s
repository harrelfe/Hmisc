## Computes rank correlation measures between a variable X and a possibly
## censored Surv variable Y
## Rank correlation is extension of Somers' Dxy = 2(Concordance Prob-.5)
## See Harrell et al JAMA 1984(?)
## Set outx=T to exclude ties in X from computations (-> Goodman-Kruskal
##  gamma-type rank correlation)
## No. This is the version extended to paired predictions
## method=1: concordance=delta x1 < delta x2
## method=2: concordance=x1 concordant and x2 discordant

rcorrp.cens <- function(x1, x2, S, outx=FALSE, method=1)
{
  if(!length(dim(S))) S <- cbind(S, rep(1, length(S)))
  y <- S[,1]
  event <- S[,2]

  if(length(x1)!=length(x2))
    stop("x1 and x3 must have same length")
  
  if(length(y)!=length(x1))
    stop("y must have same length as x")
  
  if(method!=1 & method!=2)
    stop("method must be 1 or 2")

  miss <- is.na(x1+x2+y+event)
  nmiss <- sum(miss)
  if(nmiss>0) {
    miss <- !miss
    x1 <- x1[miss]
    x2 <- x2[miss]
    y <- y[miss]
    event <- event[miss]
  }
  
  n <- length(x1)
  if(n<2)
    stop("<2 non-missing observations")
  
  ne <- sum(event)
  storage.mode(x1) <- if(.R.)"double"
                      else "single"
  
  storage.mode(x2) <- if(.R.)"double"
                      else "single"
  
  storage.mode(y) <- if(.R.)"double"
                     else "single"
  
  storage.mode(event) <- "logical"
  storage.mode(method) <- "integer"
  storage.mode(outx) <- "logical"

  z <-
    if(.R.)
      .Fortran("cidxcp",x1,x2,y,event,length(x1),method,outx,
               nrel=double(1),nuncert=double(1),
               c1=double(1),c2=double(1),gamma1=double(1),gamma2=double(1),
               gamma=double(1),sd=double(1),c12=double(1),c21=double(1),
               PACKAGE="Hmisc")
    else
      .Fortran("cidxcp",x1,x2,y,event,length(x1),method,outx,
               nrel=double(1),nuncert=double(1),
               c1=double(1),c2=double(1),gamma1=double(1),gamma2=double(1),
               gamma=double(1),sd=double(1),c12=double(1),c21=double(1))
  
  r <- c(z$gamma,z$sd,z$c12,z$c21,n,nmiss,ne,z$nrel,z$nuncert,z$c1,z$c2,
         z$gamma1,z$gamma2)
  names(r) <- c("Dxy","S.D.","x1 more concordant","x2 more concordant",
                "n","missing","uncensored",
                "Relevant Pairs","Uncertain","C X1","C X2","Dxy X1","Dxy X2")
  r
}
