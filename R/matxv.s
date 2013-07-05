## Multiply matrix by a vector
## vector can be same length as # columns in a, or can be longer,
## in which case b[kint] is added to a * b[s:length(b)], s=length(b)-ncol(a)+1
## F. Harrell 17 Oct90
## Mod         5 Jul91 - is.vector -> !is.matrix
##            16 Oct91 - as.matrix -> matrix(,nrow=1)
##            29 Oct91 - allow b to be arbitrarily longer than ncol(a), use b(1)
##            13 Nov91 - matrix(,nrow=1) -> matrix(,ncol=1)
##            14 Nov91 - changed to nrow=1 if length(b)>1, ncol=1 otherwise
##            25 Mar93 - changed to use %*%
##            13 Sep93 - added kint parameter
##            22 Jun13 - allowed null kint, matrix b (e.g. bootstrap coefs)
##             3 Jul13 - sense intercepts attribute in b which signals
##                       which subset of intercepts were retained in fit

matxv <- function(a, b, kint=1, bmat=FALSE)
{
  bi    <- attr(b, 'intercepts')
  lbi   <- length(bi)
  lkint <- length(kint)
  if(lkint > 1L) stop('kint must have length 0 or 1')
  
  if(bmat) {
    if(!is.matrix(a)) stop('a must be a matrix when b is a matrix')
    ca <- ncol(a); cb <- ncol(b)
    if(cb < ca) stop('number of columns in b must be >= number in a')
    if(cb == ca) return(a %*% t(b))
    excess <- cb - ca
    xx <- matrix(0, nrow=nrow(a), ncol=excess)
    if(lbi && lkint) {
      if(lbi != excess)
        stop('b intercepts attribute has different length from number of excess elements in b')
      bi   <- round(bi)
      kint <- round(kint)
      if(!isTRUE(all.equal(sort(bi), sort(kint))))
        stop('b intercepts attribute do not match kint')
      xx[] <- 1.
    }
    else if(lkint) {
      if(kint > excess)
        stop('kint > number of excess elements in b')
      xx[,kint] <- 1.
    }
    return(cbind(xx, a) %*% t(b))
  }

  if(!is.matrix(a))
    a <- if(length(b) == 1L) matrix(a, ncol=1L) else matrix(a, nrow=1L)

  nc <- dim(a)[2]
  lb <- length(b)
  if(lb < nc)
    stop(paste("columns in a (", nc, ") must be <= length of b (",
               length(b), ")", sep=""))

  if(nc == lb) return(drop(a %*% b))

  excess <- lb - nc
  if(lbi && lkint) {
    if(lbi != excess)
      stop('b intercepts attribute has different length from number of excess elements in b')
    bi   <- round(bi)
    kint <- round(kint)
    if(!isTRUE(all.equal(sort(bi), sort(kint))))
      stop('b intercepts attribute do not match kint')
    bkint <- b[1]
  }
  else if(lkint) {
    if(kint > excess)
      stop('kint > number excess elements in b')
    
    bkint <- b[kint]
  }
  else
    bkint <- 0.
  drop(bkint + (a %*% b[(lb - nc + 1L) : lb]))
}
