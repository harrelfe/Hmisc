## Improvements by Sebastian Weber <Sebastian.Weber@aventis.com> 26Aug03

sys <- if(.R.) function(command, text=NULL, output=TRUE) {
  cmd <- if(length(text))
    paste(command,text)
  else
    command

  if(under.unix)
    system(cmd, intern=output)
  else
    shell(cmd, wait=TRUE, intern=output)
} else if(under.unix) function(..., minimized) unix(...) else function(...,minimized=FALSE) dos(..., minimized=minimized)
