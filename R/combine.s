.ElmtCombine <- function(x, value, protect=FALSE, ...) {
  if(is.null(x)) {
    x <- vector()
  }

  if(is.null(value)) {
    value <- vector()
  }
  
  if((is.list(x) || is.vector(x)) &&
     (is.list(value) || is.vector(value))) {
    if(length(value)) {
      value.names <- names(value)
    } else {
      value.names <- vector()
    }
    
    if(length(x)) {
      x.names <- names(x)
    } else {
      x.names <- vector()
    }

    if(is.null(x.names) || is.null(value.names)) {
      stop("objects 'x' and 'value' must have names")
    }

    if(protect) {
      target <- value
      rep.vals <- x
      rep.names <- x.names
    } else {
      target <- x
      rep.vals <- value
      rep.names <- value.names
    }
        
    target[rep.names] <- rep.vals[rep.names]
    return(target)
  }
  stop("unable to combine these objects")
}

combine <- .ElmtCombine
'combine<-' <- as.function(c(formals(.ElmtCombine)[c('x','protect','...','value')],
                             body(.ElmtCombine)),
                           environment(.ElmtCombine))
