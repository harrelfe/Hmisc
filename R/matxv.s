#Multiply matrix by a vector
#vector can be same length as # columns in a, or can be longer,
#in which case b[kint] is added to a * b[s:length(b)], s=length(b)-ncol(a)+1
#F. Harrell 17 Oct90
#Mod         5 Jul91 - is.vector -> !is.matrix
#           16 Oct91 - as.matrix -> matrix(,nrow=1)
#	    29 Oct91 - allow b to be arbitrarily longer than ncol(a), use b(1)
#	    13 Nov91 - matrix(,nrow=1) -> matrix(,ncol=1)
#	    14 Nov91 - changed to nrow=1 if length(b)>1, ncol=1 otherwise
#	    25 Mar93 - changed to use %*%
#           13 Sep93 - added kint parameter

matxv <- function(a,b,kint=1) {

if(!is.matrix(a)) {
   if(length(b)==1) a <- matrix(a, ncol=1)
   else a <- matrix(a, nrow=1)	
}

nc <- dim(a)[2]
lb <- length(b)
if(lb<nc)
	stop(paste("columns in a (",nc,") must be <= length of b (",
		length(b),")",sep=""))

if(nc==lb) drop(a %*% b) else
drop(b[kint] + (a %*% b[(lb-nc+1):lb]))
}

#storage.mode(a) <- "single"
#storage.mode(b) <- "double"
#
#library.dynam(section="local", file="matxv.o")
#
#.Fortran("matxv",a,b,d[1],d[2],length(b),c=single(d[1]), NAOK=T,
#	specialsok=T)$c


