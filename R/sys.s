## Improvements by Sebastian Weber <Sebastian.Weber@aventis.com> 26Aug03

sys <- function(command, text=NULL, output=TRUE) {
  cmd <- if(length(text)) paste(command, text) else command
  system(cmd, intern=output)
}
