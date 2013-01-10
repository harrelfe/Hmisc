eip <- function(name)
{
  name <- as.character(substitute(name))
  f <- find(name)
  if(length(f) != 1)
    stop('object must exist in exactly one place')
  
  g <- edit(get(name))
  assign(name, g, pos=match(f,search()))
  
  cat('Object', name, 'stored in', f, '\n')
  invisible()
}
