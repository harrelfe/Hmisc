capitalize <- function(string) {
  capped <- grep('^[^A-Z]*$', string, perl=TRUE)

  substr(string[capped], 1,1) <- toupper(substr(string[capped], 1,1))
  return(string)
}

