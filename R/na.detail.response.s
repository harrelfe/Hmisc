na.detail.response <- function(mf)
{
  if(is.null(z <- .Options$na.detail.response) || !z)
    return(NULL)
  
  response <- model.extract(mf, response)
  if(is.null(response))
    return(NULL)
  
  if(!is.matrix(response))
    response <- as.matrix(response)
  
  GFUN <- options()$na.fun.response
  if(is.null(GFUN))
    GFUN <-  function(x, ...)
    {
      if(is.matrix(x)) x <- x[,ncol(x)]
      x <- x[!is.na(x)]
      c(N=length(x),Mean=mean(x))
    }
  else GFUN <- eval.parent(as.name(GFUN))
  
  w <- NULL; nam <- names(mf); wnam <- NULL
  N <- nrow(mf)
  p <- ncol(mf)
  omit <- rep(FALSE, N)
  for(i in 2:p) {
    x <- mf[,i]
    if(is.matrix(x))
      x <- x[,1]
    
    isna <- is.na(x)
    omit <- omit | isna
    nmiss <- sum(isna)
    if(nmiss) {
      w <- cbind(w, GFUN(response[isna,]))
      wnam <- c(wnam, paste(nam[i],"=NA",sep=""))
    }
    
    n <- N-nmiss
    if(n) {
      w <- cbind(w, GFUN(response[!isna,]))
      wnam <- c(wnam, paste(nam[i],"!=NA",sep=""))
    }
  }

  ## summarize responce for ANY x missing
  if(p>2) {
    nmiss <- sum(omit)
    if(nmiss) {
      w <- cbind(w, GFUN(response[omit,]))
      wnam <- c(wnam, "Any NA")
    }
    
    if(N-nmiss) {
      w <- cbind(w, GFUN(response[!omit,]))
      wnam <- c(wnam, "No NA")
    }
  }

  dimnames(w)[[2]] <- wnam
  w
}
