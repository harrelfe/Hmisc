if(.R.) units <- function(x,...)  UseMethod("units")

"units<-"  <- function(x, value)
{
  attr(x, "units") <- value
  x
}

units.default <- function(x, none='', ...)
{
  lab <- attr(x, "units")
  if(is.null(lab))
    lab <- attr(attr(x,'tspar'),'units')

  if(is.null(lab))
    lab <- none

  lab
}
