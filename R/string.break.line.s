if(!exists("string.break.line", mode='function')) {
  string.break.line <- function(x) {
    if(! is.character(x)) {
      x <- as.character(x)
    }
    
    ifelse(x == '', '', strsplit(x, '\n', fixed=TRUE))
  }
}
