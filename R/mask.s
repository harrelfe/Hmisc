mask<- function(a)
{
  ##determine which bits are on in a vector of status bytes
  if(a>=.Machine$integer.max)
    stop("Value > integer.max")
  
  a <- as.integer(a) 
  as.logical((rep(a, 8)%/%rep(2^(0:7), rep(length(a),8)))%%2)
}

##  Rick Becker
##  Improved by Peter Melewski 14Apr02

