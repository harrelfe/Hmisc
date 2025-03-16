units <- function(x, ...)
  UseMethod("units")

"units<-.default"  <- function(x, value)
{
  # value <- sub('s$', '', tolower(value))
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


units.Surv <- function(x, none='', ...)
{
  at  <- attributes(x)
  un  <- at$units
  ia  <- at$inputAttributes
  if(! length(un) && length(ia)) {
    un <- ia$time2$units
    if(! length(un)) un <- ia$time$units
  }
  if(! length(un)) un <- none
  un
}
