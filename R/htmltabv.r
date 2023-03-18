##' Simple HTML Table of Verbatim Output
##'
##' Uses `capture.output` to capture as character strings the results of
##' running `print()` on each element of `...`.  If an element of `...` has
##' length of 1 and is a blank string, nothing is printed for that cell
##' other than its name (not in verbatim).
##' @title htmltabc
##' @param ... objects to `print()`.  The arguments must be named with the labels you want to print before the verbatim `print()`.
##' @param cols number of columns in the html table
##' @param propts an option list of arguments to pass to the `print()` methods; default is to not quote character strings
##' @return character string of html
##' @author Frank Harrell
##' @md
htmltabv <- function(..., cols=2, propts=list(quote=FALSE)) {
  tab <- function(x, nam) {
    nr <- ceiling(length(x) / cols)
    w  <- character(nr)
    j <- 1
    for(i in 1 : nr) {
      xx <- x[j : min(j + cols - 1, length(x))]
      if(length(xx) < cols) xx <- c(xx, rep('', cols - length(xx)))
      w[i] <- paste0('<tr>', paste(paste0('<td>', xx, '</td>'),
                                   collapse='<td>&nbsp;</td>'),
                     '</tr>')
      j <- j + cols
    }
    w
  }
  dot <- list(...)
  nam <- names(dot)
  n <- length(dot)
  x <- character(n)
  for(i in 1 : n) {
    di <- dot[[i]]
    z <- if(length(di) == 1 && trimws(di) == '') di
    else capture.output(do.call('print', c(list(dot[[i]]), propts)))
    z <- paste(z, collapse='\n')
    x[i] <- paste0(nam[i], '<pre>', z, '</pre>')
    }
  paste0('<table>', paste(tab(x, nam), collapse=' '),  '</table>\n')
}
