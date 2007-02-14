.ElmtCombine <- function(x, value, protect=FALSE, ...) {
  if(!is.list(x) || !is.vector(x) ||
     !is.list(value) || !is.vector(value)) {
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
  return(callNextMethod())
}

.ElmtCombineError <- function(x, value, ...) {
  stop("cannot combine these two objects")
}

setGeneric('combine')
setMethod(f = combine, signature=signature(x="list", value="list"), definition = .ElmtCombine, valueClass="list")
setMethod(f = combine, signature=signature(x="list", value="vector"), definition = .ElmtCombine, valueClass="list")
setMethod(f = combine, signature=signature(x="vector", value="list"), definition = .ElmtCombine, valueClass="list")
setMethod(f = combine, signature=signature(x="vector", value="vector"), definition = .ElmtCombine, valueClass="vector")
setMethod(f = combine, signature=signature(x="ANY", y="ANY"), definition = .ElmtCombineError)

setGeneric('combine<-')
setMethod(f = `combine<-`, signature=signature(x="list", value="list"), definition=.ElmtCombine, valueClass="list")
setMethod(f = `combine<-`, signature=signature(x="list", value="vector"), definition=.ElmtCombine, valueClass="list")
setMethod(f = `combine<-`, signature=signature(x="vector", value="list"), definition=.ElmtCombine, valueClass="list")
setMethod(f = `combine<-`, signature=signature(x="vector", value="vector"), definition=.ElmtCombine, valueClass="vector")
setMethod(f = `combine<-`, signature=signature(x="ANY", y="ANY"), definition = .ElmtCombineError)
