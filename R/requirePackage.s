requirePackage <- function(package, character.only = FALSE, ...) {
  if (!character.only) {
    package <- as.character(substitute(package))
  }
  
  if(!require(package, character.only = TRUE, ...)) {
    stop('This function requires the', package,
         'package which does not exist on this machine')
  }
}
