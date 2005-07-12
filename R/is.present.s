is.present <- function(x)
{
  if(is.character(x))
    return(x!="")
  else
    return(!is.na(x))
}
