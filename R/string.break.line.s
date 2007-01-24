if(!exists("string.break.line", mode='function')) {
  string.break.line <- function(string) {
    if(! is.character(string)) {
      x <- as.character(string)
    }
    
    ifelse(string == '', '', strsplit(string, '\n', fixed=TRUE))
  }
}
