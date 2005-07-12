recode <- function(..., ret=c('numeric','factor'),
                   none=if(ret=='numeric')0 else 'none',na)
{
  ret <- match.arg(ret)
  w <- list(...)

  ## alternative form: recode(x, from, to), e.g. recode(x, c(1,3), c(0,1))
  if(!is.logical(w[[1]]) && length(w)==3) {
    z <- w[[3]][match(w[[1]],w[[2]])]
    if(!missing(none))
      z[if(is.numeric(none))is.na(z)
        else z==''] <- none
    
    return(z)
  }

  nam <- names(w)
  ##.Options$warn <- -1   6Aug00
  ##numnam <- as.numeric(nam)
  ##if(missing(ret)) ret <- if(any(is.na(numnam))) 'factor' else 'numeric'
  if(missing(ret))
    ret <- if(all.is.numeric(nam))'numeric'
           else 'factor'

  result <- rep(none, length(w[[1]]))

  for(i in 1:length(w))
    result[w[[i]]] <- if(ret=='numeric') numnam[i]
                      else nam[i]

  if(ret=='factor')
    result <- as.factor(result)
  
  if(!missing(na))
    result[is.na(na)] <- NA
  
  result
}
