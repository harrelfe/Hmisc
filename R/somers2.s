##S function somers2
##
##    Calculates concordance probability and Somers'  Dxy  rank  correlation
##    between  a  variable  X  (for  which  ties are counted) and a binary
##    variable Y (having values 0 and 1, for which ties are not  counted).
##    Uses short cut method based on average ranks in two groups.
## 
##    Usage:
## 
##         somers2(x, y, weights)
##
##    Returns vector whose elements are C Index, Dxy, n and missing, where
##    C Index is the concordance probability and Dxy=2(C Index-.5).
##
##    F. Harrell 28 Nov 90     6 Apr 98: added weights

somers2 <- function(x, y, weights=NULL, normwt=FALSE, na.rm=TRUE)
{
  if(length(y) != length(x)) stop("y must have same length as x")
  y <- as.integer(y)
  wtpres <- length(weights)
  if(wtpres && (wtpres != length(x)))
    stop('weights must have same length as x')

  if(na.rm) {
    miss <- if(wtpres) is.na(x + y + weights)
    else is.na(x + y)

    nmiss <- sum(miss)
    if(nmiss > 0)
      {
        miss <- !miss
        x <- x[miss]
        y <- y[miss]
        if(wtpres) weights <- weights[miss]
      }
  }
  else nmiss <- 0
  
  if(any(y %nin% 0:1)) stop('y must be binary')

  if(wtpres) {
    if(normwt)
      weights <- length(x)*weights/sum(weights)
    n <- sum(weights)
  }
  else n <- length(x)
  
  if(n < 2) stop("must have >=2 non-missing observations")

  n1 <- if(wtpres)sum(weights[y==1]) else sum(y==1)

  if(n1 == 0 || n1 == n)
    return(c(C=NA, Dxy=NA, n=n, Missing=nmiss))

  mean.rank <-
    if(wtpres)
      wtd.mean(wtd.rank(x, weights, na.rm=FALSE), weights * y)
    else 
      mean(rank(x)[y==1])

  c.index <- (mean.rank - (n1 + 1) / 2) / (n - n1)
  dxy <- 2 * (c.index - 0.5)
  r <- c(c.index, dxy, n, nmiss)
  names(r) <- c("C", "Dxy", "n", "Missing")
  r
}


if(FALSE) rcorrs <- function(x, y, weights=rep(1,length(y)),
                             method=c('exact','bin'), nbin=1000,
                             na.rm=TRUE)
{
  ## Experimental function - probably don't need
  
  method <- match.arg(method)
  
  if(na.rm) {
    s <- !is.na(x + unclass(y) + weights)
    x <- x[s]; y <- y[s]; weights <- weights[s]
  }
  
  n <- length(x)
  if(missing(method))
    method <- if(n < 1000) 'exact'
              else 'bin'

  y <- as.factor(y);
  nly <- length(levels(y))
  y <- as.integer(y)
  if(method == 'bin') {
    r <- range(x); d <- r[2] - r[1]
    x <- 1 + trunc((nbin - 1) * (x - r[1]) / d)
 
    xy <- y * nbin + x

    ## Code below is lifted from rowsum()
    storage.mode(weights) <- "double"
    temp <-
        .C('R_rowsum', dd=as.integer(dd),
           as.double(max(1,weights)*n),
           x=weights, as.double(xy), PACKAGE='base')
    new.n <- temp$dd[1]
    weights <- temp$x[1:new.n]

    uxy <- unique(xy)
    x <- uxy %% nbin
    y <- (uxy - x)/nbin
    n <- length(x)
  }

  list(x=x, y=y, weights=weights)

  #storage.mode(x) <- "single"
  #storage.mode(y) <- "single"
  #storage.mode(event) <- "logical"

  ## wcidxy doesn't exist yet
##  z <- .Fortran(F_wcidxy,as.single(x),as.single(y),as.integer(weights),as.integer(n),
#                nrel=double(1),nconc=double(1),nuncert=double(1),
#                c.index=double(1),gamma=double(1),sd=double(1),as.logical(outx))
  r <- c(z$c.index,z$gamma,z$sd,n,z$nrel,z$nconc,z$nuncert)
  names(r) <- c("C Index","Dxy","S.D.","n","missing","uncensored",
                "Relevant Pairs",	"Concordant","Uncertain")
  r
}
