xtfrm.labelled <- function(x) {
  newclass <-  class(x)[class(x) != 'labelled']
  if (length(newclass) == 0) {
    class(x) <- NULL
  } else {
    oldClass(x) <- newclass
  }
  xtfrm(x)
}
