nstr <- function(string, times) {
  if(!is.atomic(string))
    stop("argument string must be an atomic vector")

  if(!is.numeric(times))
    stop("len must be a numeric vector")

  if(length(string) == 0)
    return(NULL)

  if(length(times) == 0)
    return(character(0))

  return(.Call("do_nstr", as.character(string), as.integer(times)))
}
