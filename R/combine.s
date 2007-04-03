.ElmtCombine <- function(x, value, protect=FALSE, ...) {
  if((is.list(x) || is.vector(x)) &&
     (is.list(value) || is.vector(value))) {
    value.names <- names(value)
    x.names <- names(x)

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
