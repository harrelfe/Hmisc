## $Id$

valueTags <- function(x)
  list(unit = valueUnit(x), label = valueLabel(x),
       name = valueName(x))


"valueTags<-" <- function(x, list) {
  tagged <- FALSE

  if(!is.list(list))
    stop("value must be a named list of valueTags")

  if(!is.null(list$unit)) {
    tagged <- TRUE
    valueUnit(x) <- list$unit
  }

  if(!is.null(list$label)) {
    tagged <- TRUE
    valueLabel(x) <- list$label
  }

  if(!is.null(list$name)) {
    tagged <- TRUE
    valueName(x) <- list$name
  }

  if(tagged)
    oldClass(x) <- c('labelled', oldClass(x)[oldClass(x) != 'labelled'])

  return(x)
}

valueLabel <- function(x)
  attr(x, 'label')

"valueLabel<-" <- function(x, value) {
  if(!is.character(value) || length(value) != 1)
    stop("value label must be a character vector of length 1")
  
  attr(x, 'label') <- value

  oldClass(x) <- c('labelled', oldClass(x)[oldClass(x) != 'labelled'])

  return(x)
}

valueUnit <- function(x)
  attr(x, 'units')

"valueUnit<-" <- function(x, value) {
  if(!is.character(value) || length(value) != 1)
    stop("value unit must be a character vector of length 1")

  attr(x, 'units') <- value

  oldClass(x) <- c('labelled', oldClass(x)[oldClass(x) != 'labelled'])

  return(x)
}

valueName <- function(x)
  attr(x, 'valueName')

"valueName<-" <- function(x, value) {
  if(!is.character(value) || length(value) != 1)
    stop("value name must be a character vector of length 1")

  attr(x, 'valueName') <- value

  oldClass(x) <- c('labelled', oldClass(x)[oldClass(x) != 'labelled'])

  return(x)
}
