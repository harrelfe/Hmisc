combine <- function(x, value, protect, ...) {
  UseMethod("combine")
}

'combine<-' <- function(x, protect, ..., value)
  eval.parent(replace(match.call(expand.dots=FALSE), list=1,
                      values=list(as.name("combine"))))

combine.default <- function(x, value, protect=FALSE, ...) {
  if(missing(x) || is.null(x))
    x <- vector()

  if(missing(value) || is.null(value))
    value <- vector()
  
  xNames <- names(x)
  valueNames <- names(value)

  if(is.null(xNames) || is.null(valueNames) || all(valueNames == "") ||
     all(xNames == ""))
    return(c(x, value))
  
  vars <- intersect(xNames, valueNames[valueNames != ""])
  if(!protect)
    x[vars] <- value[vars]

  c(x, value[!valueNames %in% vars])
}

