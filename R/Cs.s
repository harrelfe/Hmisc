#Cs <- function(...)
#{
#  if(version$major > 4) as.character(sys.call()[-1]) else {
#	y <- ((sys.frame())[["..."]])[[1]][-1]
#	unlist(lapply(y, deparse))
#  }
#}  31Mar02

Cs <- function(...)
{
  if(.SV4. || .R.) as.character(sys.call())[-1] else {
	y <- ((sys.frame())[["..."]])[[1]][-1]
	unlist(lapply(y, deparse))
  }
}
