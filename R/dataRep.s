dataRep <- function(formula, data, subset, na.action)
{
  call <- match.call()
  nact <- NULL
  y <- match.call(expand.dots=FALSE)
  if(missing(na.action))
    y$na.action <- na.delete
  
  y[[1]] <- as.name("model.frame")
    
  X <- eval(y, sys.parent())
  nact <- attr(X,"na.action")
  n <- nrow(X)
  nam <- names(X)
  p <- length(nam)
  types <- character(p)
  parms <- character(p)
  pctl  <- vector('list',p)
  margfreq <- vector('list',p)
  Xu   <- vector('list',p)
  for(j in 1:p) {
    namj <- nam[j]
    xj <- X[[j]]
    if(is.character(xj))
      xj <- as.factor(xj)
    
    if(is.factor(xj)) {
      parms[[j]] <- paste(levels(xj),collapse=' ')
      types[j] <- 'exact categorical'
    } else if(inherits(xj,'roundN')) {
      atr <- attributes(xj)
      nam[j] <- atr$name
      types[j] <- 'round'
      parms[j] <- paste('to nearest',format(atr$tolerance))
      if(length(w <- atr$clip))
        parms[j] <- paste(parms[j],', clipped to [',
                          paste(format(w),collapse=','),']',sep='')
      
      pctl[[j]] <- atr$percentiles
    } else {
      types[j] <- 'exact numeric'
      parms[j] <- ''
      pctl[[j]] <- quantile(xj, seq(0,1,by=.01))
    }

    margfreq[[j]] <- table(xj)
    Xu[[j]] <- sort(unique(xj))
    X[[j]] <- xj
  }
  
  names(types) <- names(parms) <- names(pctl) <- names(margfreq) <- 
    names(Xu) <- nam
  
  Xu <- expand.grid(Xu)
  m <- nrow(Xu)
  count <- integer(m)
  for(i in 1:m) {
    matches <- rep(TRUE,n)
    for(j in 1:p)
      matches <- matches & (as.character(X[[j]]) ==
                            as.character(Xu[[j]][i]))
    
    count[i] <- sum(matches)
  }
  
  if(any(count==0)) {
    s     <- count > 0
    Xu    <- Xu[s,]
    count <- count[s]
    m     <- sum(s)
  }

  structure(list(call=call, formula=formula, n=n, names=nam, 
                 types=types, parms=parms, margfreq=margfreq,
                 percentiles=pctl, X=Xu, count=count, na.action=nact), 
            class='dataRep')
}

roundN <- function(x, tol=1, clip=NULL)
{
  pct <- quantile(x, seq(0,1,by=.01), na.rm=TRUE)
  name <- deparse(substitute(x))
  lab <- attr(x, 'label')
  if(!length(lab))
    lab <- name
  
  if(!missing(clip))
    x <- pmin(pmax(x,clip[1]),clip[2])
  
  structure(as.single(tol*round(x/tol)), tolerance=tol, clip=clip,
            percentiles=pct, name=name, label=lab, class='roundN')
}

as.data.frame.roundN <- as.data.frame.vector


'[.roundN' <- function(x, i, ...)
{
  atr <- attributes(x)
  x <- unclass(x)[i]
  attributes(x) <- atr
  x
}


print.dataRep <- function(x, long=FALSE, ...)
{
  cat("\n")
  cat("Data Representativeness    n=",x$n,"\n\n", sep='')
  dput(x$call)
  cat("\n")
  if(length(z <- x$na.action))
    naprint(z)
  
  specs <- data.frame(Type=x$types, 
                      Parameters=x$parms,
                      row.names=x$names)
  
  cat('Specifications for Matching\n\n')
  print.data.frame(specs)
  X <- x$X
  if(long) {
    X$Frequency <- x$count
    cat('\nUnique Combinations of Descriptor Variables\n\n')
    print.data.frame(X)
  } else cat('\n',nrow(X),
             'unique combinations of variable values were found.\n\n')
  invisible()
}


predict.dataRep <- function(object, newdata, ...)
{
  n <- object$n
  count <- object$count
  if(missing(newdata))
    return(count)

  pctl     <- object$percentiles
  margfreq <- object$margfreq
  p        <- length(margfreq)
  m        <- nrow(newdata)
  nam      <- object$names
  types    <- object$types
  X        <- object$X

  ##Xn <- if(length(model.frame.default$Des))   3Aug02
  ##        model.frame(object$formula, newdata, na.action=na.keep, Des=FALSE) else
  Xn <- model.frame(object$formula, newdata, na.action=na.keep)
  names(Xn) <- nam

  worst.margfreq <- rep(1e8, m)
  pct <- matrix(NA, m, p, dimnames=list(row.names(Xn),nam))
  for(j in 1:p) {
    xj <- Xn[[j]]
    freq <- margfreq[[nam[j]]][as.character(xj)]
    freq[is.na(freq)] <- 0
    pct[,j] <- if(types[j]=='exact categorical')
                 100*freq/n
               else
                 approx(pctl[[nam[j]]], seq(0,100,by=1),
                        xout=newdata[[nam[j]]], rule=2)$y
    
    worst.margfreq <- pmin(worst.margfreq, freq)
  }

  cnt <- integer(m)
  for(i in 1:m) {
    matches <- rep(TRUE,nrow(X))
    for(j in 1:p) {
      matches <- matches & (as.character(X[[j]]) == as.character(Xn[[j]][i]))
    }
    
    s <- sum(matches)
    if(s > 1) 
      warning('more than one match to original data combinations')
    
    cnt[i] <- if(s)
                count[matches]
              else
                0
  }
  
  if(any(cnt > worst.margfreq))
    warning('program logic error')

  structure(list(count=cnt, percentiles=pct, worst.margfreq=worst.margfreq, 
                 newdata=newdata),	class='predict.dataRep')
}

print.predict.dataRep <- function(x, prdata=TRUE, prpct=TRUE, ...)
{
  if(prdata) {
    dat <- x$newdata
    dat$Frequency     <- x$count
    dat$Marginal.Freq <- x$worst.margfreq
    cat('\nDescriptor Variable Values, Estimated Frequency in Original Dataset,\nand Minimum Marginal Frequency for any Variable\n\n')
    print.data.frame(dat)
  } else {
    cat('\nFrequency in Original Dataset\n\n')
    print(x$count)
    cat('\nMinimum Marginal Frequency for any Variable\n\n')
    print(x$worst.margfreq)
  }
  
  if(prpct) {
    cat('\n\nPercentiles for Continuous Descriptor Variables,\nPercentage in Category for Categorical Variables\n\n')
    print(round(x$percentiles))
  }
  
  invisible()
}
