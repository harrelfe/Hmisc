reShape <- function(x, ..., id, colvar, base, reps,
                    times=1:reps, timevar='seqno', constant=NULL)
{
  if(!missing(base)) {
    if(!is.list(x))
      stop('x must be a list or data frame when base is given')
    
    repvars <- as.vector(outer(base,1:reps,paste,sep=''))
    nam <- names(x)
    nonrep <- nam[nam %nin% repvars]
    res <- vector('list', 1+length(nonrep)+length(base))
    names(res) <- c(timevar, nonrep, base)
    x1 <- x[[1]]
    n <- if(is.matrix(x1)) nrow(x1)
         else length(x1)
    
    res[[1]] <- rep(times[1:reps], n)

    for(i in nonrep) res[[i]] <- rep(x[[i]], rep(reps,n))

    ## Get indexes that will put unlist() in right order
    k <- as.vector(matrix(1:(reps*n), nrow=reps, byrow=TRUE))
    for(i in base) {
      bn <- paste(i, 1:reps, sep='')
      x1 <- x[[bn[1]]]
      at <- attributes(x1)
      at$names <- NULL
      x1 <- unlist(x[bn])[k]
      if(length(at)) attributes(x1) <- at
      res[[i]] <- x1
    }
    
    if(is.data.frame(x)) {
      rn <- attr(x,'row.names')
      ln <- length(rn)
      if(ln) {
        ## R calls data.frame even if specify structure, and R does
        ## not have dup.row.names argument to data.frame as does S+
        return(data.frame(res,
                          row.names=paste(rep(rn,rep(reps,ln)),
                            rep(1:reps,n))))
      }
    }
    
    return(res)
  }
    
  if(is.matrix(x)) {
    y <- as.vector(x)
    v1 <- all.is.numeric(dimnames(x)[[1]][row(x)],'vector')
    v2 <- all.is.numeric(dimnames(x)[[2]][col(x)],'vector')
    w <- list(v1, v2, y)
    names(w) <- c('rowvar','colvar',as.character(substitute(x)))
    if(length(nd <- names(dimnames(x))))
      names(w)[1:2] <- nd
    
    w
  } else {
    listid <- is.list(id)
    i <- as.factor(if(listid) do.call('paste', c(id, sep='~'))
                   else id)
    
    colvar <- as.factor(colvar)
    m <- matrix(NA, nrow=length(levels(i)), ncol=length(levels(colvar)),
                dimnames=list(levels(i), levels(colvar)))
    dotlist <- list(...)
    if(!length(dotlist)) {
      m[cbind(i, colvar)] <- x
      if(listid) {
        j <- match(as.character(dimnames(m)[[1]]), as.character(i))
        if(length(constant))
          data.frame(id[j,,drop=FALSE], constant[j,,drop=FALSE], m)
        else data.frame(id[j,,drop=FALSE], m)
      } else m
      
    } else {
      res <- vector('list',nx <- 1+length(dotlist))
      names(res) <- (as.character(sys.call())[-1])[1:nx]
      nam2 <- names(sys.call()[-1])[1:nx]
      if(length(nam2))
        names(res) <- ifelse(nam2=='',names(res),nam2)
      
      w <- m;
      w[cbind(i, colvar)] <- x;
      res[[1]] <- w
      
      for(j in 2:nx) {
        w <- m;
        w[cbind(i, colvar)] <- dotlist[[j-1]]
        res[[j]] <- w
      }
      
      res
    }
  }
}
