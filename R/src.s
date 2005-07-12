##Function to source(x) if x is given, or source(last x given) otherwise
##Last x is stored in options() last.source.   x is unquoted with .s omitted.
##Author: Frank Harrell  19May91

src <- function(x) {
  if(!missing(x)) {
    y <- paste(as.character(substitute(x)),".s",sep="")
    options(last.source=y, TEMPORARY=FALSE)
  }
  else y <- options()$last.source

  if(is.null(y))
    stop("src not called with file name earlier")

  source(y)
  cat(y, "loaded\n")
  invisible()
}
