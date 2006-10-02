makeNstr <- function(char, len) {
  mapply(function(char, len) {
    if(is.na(len)) {
      '\n'
    } else if(len == 0) {
      ''
    } else {
      paste(rep.int(x=char, times=len), collapse='')
    }
  }, char, len, USE.NAMES=FALSE)
}
