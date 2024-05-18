#' printL
#' 
#' Print an object or a named list of objects.  When multiple objects are given, their names are printed before their contents.  When an object is a vector that is not longer than `maxoneline` and its elements are not named, all the elements will be printed on one line separated by commas.  When `dec` is given, numeric vectors or numeric columns of data frames or data tables are rounded to the nearest `dec` before printing.  This function is especially helpful when printing objects in a Quarto or RMarkdown document and the code is not currently being shown to place the output in context.
#'
#' @param ... any number of objects to `print()`
#' @param dec optional decimal places to the right of the decimal point for rounding
#' @param maxoneline controls how many elements may be printed on a single line for `vector` objects
#'
#' @return nothing
#' @author Frank Harrell
#' @export
#' @md
#' @seealso [prn()]
#'
#' @examples
#' w <- pi + 1 : 2
#' printL(w=w)
#' printL(w, dec=3)
#' printL('this is it'=c(pi, pi, 1, 2),
#'        yyy=pi,
#'        z=data.frame(x=pi+1:2, y=3:4, z=c('a', 'b')),
#'        qq=1:10,
#'        dec=4)
#'        
printL <- function(..., dec=NULL, maxoneline=5) {
  z  <- list(...)
  ns <- names(z)
  if(! length(ns)) {
    if(length(z) > 1) stop('must name arguments')
    names(z) <- ' '
  }
  for(n in names(z)) {
    x <- z[[n]]
    l <- length(x)
    if(is.numeric(x) && length(dec)) x <- round(x, dec)
    if(is.vector(x) &&
       (l == 1 || (l <= maxoneline && ! length(names(x))))) {
      x <- paste(x, collapse=', ')
      cat(if(n != ' ') c(n, ': '), x, '\n\n', sep='')
    } else {
      if(is.list(x) && length(dec))
        for(j in 1 : length(x))
          if(is.numeric(x[[j]])) x[[j]] <- round(x[[j]], dec)
      if(n != ' ') cat(n, ':\n', sep='') else cat('\n')
      print(x)
      cat('\n')
    }
  }
  invisible()
}

