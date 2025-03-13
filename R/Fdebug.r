#' Debug Printing Function Generator
#' 
#' Takes the name of a system `options(opt=)` and checks to see if option `opt` is
#' set to `TRUE`, taking its default value to be `FALSE`.  If `TRUE`, a function is
#' created that calls [prn()] to print an object with the object's name in the
#' description along with the option name and the name of the function within which
#' the generated function was called, if any.  If option `opt` is not set, a dummy function
#' is generated instead.  If `options(debug_file=)` is set when the generated function
#' is called, [prn()] output will be appended to that file name instead of the console.
#' At any time, set `options(debug_file='')` to resume printing to the console.
#'
#' @param opt character string containing an option name
#'
#' @returns a function
#' @export
#' @md
#' @author Fran Harrell
#'
#' @examples
#' dfun <- Fdebug('my_option_name')   # my_option_name not currently set
#' dfun
#' dfun(sqrt(2))
#' options(my_option_name=TRUE)
#' dfun <- Fdebug('my_option_name')
#' dfun
#' dfun(sqrt(2))
#' # options(debug_file='/tmp/z') to append output to /tmp/z
#' options(my_option_name=NULL)
Fdebug <- function(opt) {
  if(getOption(opt, FALSE)) {
    deb <- function(x, txt, callingfun, file=getOption('debug_file', '')) {
      if(length(callingfun))
        txt  <- paste0(txt, ': ', callingfun)
      head <- deparse(substitute(x), width.cutoff=500)[1]
      prn(x, txt, head=head, file=file)
    }
  formals(deb)$txt <- opt
  formals(deb)$callingfun <- as.character(sys.call(-1)[1])
  }
  else deb <- function(x, txt, callingfun, file) {NULL}
  deb
}
