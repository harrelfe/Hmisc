## $Id$
.valueTagAttrs <- c(label="label", units="units", name="shortlabel")


valueTags <- function(x)
  attributes(x)[names(attributes(x)) %in% .valueTagAttrs]


"valueTags<-" <- function(x, value) {
  if(is.null(value) || length(value) == 0) {
    attributes(x)[names(attributes(x)) %in% .valueTagAttrs] <- NULL
    class(x) <- class(x)[class(x) != 'labelled']
    return(x)
  }
  
  if(!is.list(value)) {
    stop("list must be a named list of valueTags")
  }

  value[(!names(value) %in% .valueTagAttrs) |
        unlist(lapply(value, is.null))] <- NULL

  if(length(value) == 0) {
    attributes(x)[names(attributes(x)) %in% .valueTagAttrs] <- NULL
    class(x) <- class(x)[class(x) != 'labelled']
    return(x)
  }
  
  attributes(x)[setdiff(names(attributes(x))[names(attributes(x)) %in%
                                             .valueTagAttrs],
                        names(value))] <- NULL

  consolidate(attributes(x)) <- value

  if(all(class(x) != 'labelled'))
    class(x) <- c('labelled', class(x))

  return(x)
}

valueLabel <- function(x)
  attr(x, 'label')

"valueLabel<-" <- function(x, value) {
  if(!is.character(value) || length(value) != 1)
    stop("value label must be a character vector of length 1")
  
  attr(x, 'label') <- value

  class(x) <- c('labelled', class(x)[class(x) != 'labelled'])

  return(x)
}

valueUnit <- function(x)
  attr(x, 'units')

"valueUnit<-" <- function(x, value) {
  if(!is.character(value) || length(value) != 1)
    stop("value unit must be a character vector of length 1")

  attr(x, 'units') <- value

  class(x) <- c('labelled', class(x)[class(x) != 'labelled'])

  return(x)
}

valueName <- function(x)
  attr(x, 'valueName')

"valueName<-" <- function(x, value) {
  if(!is.character(value) || length(value) != 1)
    stop("value name must be a character vector of length 1")

  attr(x, 'valueName') <- value

  class(x) <- c('labelled', class(x)[class(x) != 'labelled'])

  return(x)
}
