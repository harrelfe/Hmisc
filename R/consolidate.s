#combine <- function(x, value, protect, ...) stop("combine() depricated due to naming conflict renamed consolidate()")
#'combine<-' <- function(x, protect, ..., value) stop("combine<-() depricated due to naming conflict renamed consolidate<-()")

consolidate <- function(x, value, protect, ...) {
  UseMethod("consolidate")
}

'consolidate<-' <- function(x, protect=FALSE, ..., value)
  consolidate(x, value, protect, ...)

consolidate.default <- function(x, value, protect=FALSE, ...) {
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

