find.matches <- function(x, y, tol=rep(0,ncol(y)), scale=tol,
                         maxmatch=10)
{
  ##if(length(dim(x))==0) x <- matrix(x, nrow=1)  10may02
  if(!is.matrix(x))
    x <- as.matrix(x)
  
  n <- nrow(x)
  p <- ncol(x)
  if(!is.matrix(y))
    y <- as.matrix(y)  ## 10may02
  
  if(p != ncol(y))
    stop("number of columns of x and y must match")
  
  ny <- nrow(y)
  rown <- dimnames(x)[[1]]
  ry <- dimnames(y)[[1]]
  matches <- matrix(if(length(ry))
                      ""
                    else
                      0,
                    n, maxmatch,
                    dimnames=list(rown,
                                  paste("Match #",1:maxmatch,sep="")))
  
  distance <- matrix(NA, n, maxmatch,
                     dimnames=list(rown,
                                   paste("Distance #",1:maxmatch,sep="")))
  
  if(length(ry)==0)
    ry <- 1:ny

  scale <- ifelse(scale==0,1,tol)
  ones <- rep(1,p)
  mx <- 0
  for(i in 1:n) {
    dif <- abs(y - rep(x[i,], rep.int(ny,p)))
    toll <- rep(tol, rep.int(nrow(dif),p))
    which <- (1:ny)[((dif > toll) %*% ones)==0]
    lw <- length(which)
    if(lw) {
      scaled <- dif[which,,drop=FALSE]/rep(scale, rep.int(lw,p))
      dist <- (scaled^2) %*% ones
      lw <- min(lw,maxmatch)
      mx <- max(mx,lw)
      d <- order(dist)[1:lw]
      matches[i,1:lw] <- ry[which[d]]
      distance[i,1:lw] <- dist[d]
    }
  }
  
  structure(list(matches=matches[,1:mx], distance=distance[,1:mx]), 
            class="find.matches")
}


print.find.matches <- function(x, digits=.Options$digits, ...)
{
  cat("\nMatches:\n\n")
  print(x$matches, quote=FALSE)
  cat("\nDistances:\n\n")
  print(x$distance, digits=digits)
  invisible()
}


summary.find.matches <- function(object, ...)
{
  mat <- object$matches
  dist <- object$distance
  cat("Frequency table of number of matches found per observation\n\n")
  m <- (!is.na(dist)) %*% rep(1,ncol(mat))
  print(table(m))
  cat("\nMedian minimum distance by number of matches\n\n")
  print(tapply(dist[m>0,1], m[m>0], median))
  ta <- table(mat[m>0,1])
  ta <- ta[ta>1]
  if(length(ta)) {
    cat("\nObservations selected first more than once (with frequencies)\n\n")
    print(ta)
  } else cat("\nNo observations selected first more than once\n\n")
  
  invisible()
}


matchCases <- function(xcase,    ycase,    idcase=names(ycase),
                       xcontrol, ycontrol, idcontrol=names(ycontrol),
                       tol=NULL,
                       maxobs=max(length(ycase),length(ycontrol))*10,
                       maxmatch=20, which=c('closest','random'))
{
  if(!length(tol))
    stop('must specify tol')

  if((length(xcase)!=length(ycase)) || (length(xcontrol)!=length(ycontrol)))
    stop('lengths of xcase, ycase and of xcontrol, ycontrol must be same')

  which <- match.arg(which)
  
  ycase    <- as.matrix(ycase)
  ycontrol <- as.matrix(ycontrol)
  if(!length(idcase))
    idcase <- 1:length(ycase)
  
  if(!length(idcontrol))
    idcontrol <- 1:length(ycontrol)
  
  idcase    <- as.character(idcase)
  idcontrol <- as.character(idcontrol)
  
  j <- is.na(ycase %*% rep(1,ncol(ycase))) | is.na(xcase)
  if(any(j)) {
    warning(paste(sum(j),'cases removed due to NAs'))
    ycase <- ycase[!j,,drop=FALSE]
    xcase <- xcase[!j]
    idcase <- idcase[!j]
  }
  
  j <- is.na(ycontrol %*% rep(1,ncol(ycontrol))) | is.na(xcontrol)
  if(any(j)) {
    warning(paste(sum(j),'controls removed due to NAs'))
    ycontrol <- ycontrol[!j,,drop=FALSE]
    xcontrol <- xcontrol[!j]
    idcontrol <- idcontrol[!j]
  }

  idCase <- id <- character(maxobs)
  type   <- factor(rep(NA,maxobs), c('case','control'))
  x      <- numeric(maxobs)
  y      <- matrix(NA, ncol=ncol(ycase), nrow=maxobs)

  last <- 0
  ncase <- length(ycase)
  ncontrol <- length(ycontrol)
  matches  <- integer(ncase)
  for(i in 1:ncase) {
    s <- abs(xcontrol-xcase[i]) <= tol
    nmatch <- sum(s)
    if(nmatch > maxmatch) {
      s <- (1:ncontrol)[s]  ## next line was sample(j,...) 4jun02
      if(which=="random")
        s <- sample(s, maxmatch, replace=FALSE)
      else {
        errors <- abs(xcontrol[s]-xcase[i])
        serrors <- order(errors)
        s <- (s[serrors])[1:maxmatch]
      }
      
      nmatch <- maxmatch
    }
    
    matches[i] <- nmatch
    if(!nmatch)
      next
    
    end <- last + nmatch + 1
    if(end > maxobs)
      stop(paste('needed maxobs >',maxobs))

    start <- last+1
    last <- end
    idCase[start:end] <- rep(idcase[i], nmatch+1)
    type[start:end]   <- c('case',rep('control',nmatch))
    id[start:end]     <- c(idcase[i], idcontrol[s])
    x[start:end]      <- c(xcase[i], xcontrol[s])
    y[start:end,]     <- rbind(ycase[i,,drop=FALSE], ycontrol[s,,drop=FALSE])
  }

  cat('\nFrequencies of Number of Matched Controls per Case:\n\n')
  print(table(matches))
  cat('\n')
  structure(list(idcase=idCase[1:end], type=type[1:end],
                 id=id[1:end], x=x[1:end], y=drop(y[1:end,])),
            row.names=as.character(1:end),
            class='data.frame')
}
