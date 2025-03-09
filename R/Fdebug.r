#' Debug Printing Function Generator
#' 
#' Takes the name of a system `option(opt=)` and checks to see if option `opt` is
#' set to `TRUE`, taking its default value to be `FALSE`.  If `TRUE`, a function is
#' created that calls [prn()] to print an object with the object's name in the
#' description along with the option name and the name of the function within which
#' the generated function was called, if any.  If option `opt` is not set, a dummy function
#' is generated instead.
#'
#' @param opt an unquoted option name
#'
#' @returns a function
#' @export
#' @md
#' @author Fran Harrell
#'
#' @examples
#' dfun <- Fdebug(my_option_name)   # my_option_name not currently set
#' dfun
#' dfun(sqrt(2))
Fdebug <- function(opt) {
  opt <- as.character(substitute(opt))
  if(getOption(opt, FALSE)) {
    deb <- function(x, txt, callingfun) {
      if(length(callingfun))
        txt  <- paste0(txt, ': ', callingfun)
      head <- deparse(substitute(x), width.cutoff=500)[1]
      prn(x, txt, head=head)
    }
  formals(deb)$txt <- opt
  formals(deb)$callingfun <- as.character(sys.call(-1)[1])
  }
  else deb <- function(...) {NULL}
  deb
}
