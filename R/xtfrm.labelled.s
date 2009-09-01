xtfrm.labelled <- function(x) {
  newclass <-  class(x)[class(x) != 'labelled']
  if (length(newclass) == 0) {
    class(x) <- NULL
  } else {
    class(x) <- newclass
  }
  xtfrm(x)
}
