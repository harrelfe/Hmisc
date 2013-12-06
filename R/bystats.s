bystats <- function(y, ..., fun, nmiss, subset)
{
  ## Fri, 16 Sep 2005 - Shawn@ori.org removed left argument to
  ## interaction
  x <- interaction(..., drop=TRUE, sep=" ")
  l <- levels(x)
  if(any(is.na(x))) {
    l <- c(l, "NA")
    attr(x,"class") <- NULL
    x[is.na(x)] <- length(l)
    levels(x) <- l
    attr(x,'class') <- "factor"
  }
  
  y <- as.matrix(y)
  if(!missing(subset)) { 
    x <- x[subset]
    y <- y[subset,,drop=FALSE]
  }

  if(missing(fun)) {
    fun <- function(y) apply(y, 2, mean)
    
    r <- range(y, na.rm=TRUE)
    uy <- unique(y[!is.na(y)])  #fixed 1Jun95, 16Mar96
    funlab <- if(length(uy)==2 && r[1]==0 & r[2]==1)
                "Fraction"
              else
                "Mean"
  } else {
    funlab <- as.character(substitute(fun))
    funlab <- funlab[length(funlab)] #handles fun=function(x)mean(x)
  }
  lab <- as.character(sys.call())[-1]
  m <- (!missing(fun)) + (!missing(nmiss)) + (!missing(subset))
  lab <- lab[1:(length(lab)-m)]
  if(length(lab)>2)
    lab2 <- paste(lab[-1],collapse=", ")
  else
    lab2 <- lab[-1]
  heading <- if(funlab=="")
               paste(lab[1],"by",lab2)
             else
               paste(funlab,"of",lab[1],"by",lab2)

  nna <- !is.na(y %*% rep(1,ncol(y)))
  N <- sum(nna)
  stats <- fun(y[nna,,drop=FALSE])
  nstats <- length(stats)
  name.stats <- if(length(dn <- dimnames(stats))) 
                  as.vector(outer(dn[[1]],dn[[2]],
                                  FUN=function(a,b)paste(b, a)))
                else
                  names(stats)
  
  if(length(name.stats))
    funlab <- name.stats
  if(nstats>1 && length(name.stats)==0)
    funlab <- rep(" ", nstats)
  s <- matrix(NA, nrow=length(l) + 1, ncol=2 + nstats,
              dimnames=list(c(l, "ALL"),c("N", "Missing", funlab)))
  j <- 0
  for(i in l) {
    j <- j+1
    w <- y[x==i,,drop=FALSE]
    nna <- !is.na(w %*% rep(1,ncol(w)))
    n <- sum(nna)
    s[j,] <- c(n, nrow(w)-n, 
               if(n) fun(w[nna,,drop=FALSE])
               else rep(NA,nstats))
  }
  
  s[j+1,] <- c(N, nrow(y)-N, stats)
  if((!missing(nmiss) && !nmiss) || (missing(nmiss) && all(s[,"Missing"]==0)))
    s <- s[,-2]
  
  attr(s, "heading")    <- heading
  attr(s, "byvarnames") <- lab2
  attr(s,'class')       <- "bystats"
  s
}

print.bystats <- function(x, ...)
{
  cat("\n",attr(x,"heading"),"\n\n")
  attr(x,"heading") <- NULL
  attr(x,"byvarnames") <- NULL
  attr(x,'class') <- NULL
  invisible(print(x, ...))
}

latex.bystats <- function(object,
                          title=first.word(expr=substitute(object)),
                          caption=attr(object,"heading"),
                          rowlabel=attr(object,"byvarnames"), ...)
{
  dm <- dimnames(object)
  ##inn <- c("%","<=","<",">=",">","\\[")
  ##out <- c("\\\\%","$\\\\leq$","$<$","$\\\\geq$","$>$","\\\\verb|[|")
  ##dm[[1]] <- translate(dm[[1]],inn,out)
  ##dm[[2]] <- translate(dm[[2]],inn,out)
  inn <- c("%","<=","<",">=",">","[")
  out <- c("\\%","$\\leq$","$<$","$\\geq$","$>$","\\verb|[|")
  dimnames(object) <- dm
  caption <- sedit(caption, "cbind", "")
  latex(unclass(object), title=title, caption=caption, rowlabel=rowlabel, 
        n.rgroup=c(nrow(object)-1,1), ...)
}

bystats2 <- function(y, v, h, fun, nmiss, subset)
{
  y <- as.matrix(y)
  if(!missing(subset)) {
    y <- y[subset,,drop=FALSE];
    v <- v[subset];
    h <- h[subset]
  }
  
  v <- factor(v, exclude=NULL)
  h <- factor(h, exclude=NULL)

  lv <- levels(v)
  lh <- levels(h)
  nv <- length(lv)
  nh <- length(lh)

  if(missing(fun)) {
    fun <- function(y) apply(y, 2, mean)
    r <- range(y, na.rm=TRUE)
    funlab <- if(length(r)==2 && r[1]==0 & r[2]==1) "Fraction"
              else "Mean"
  } else {
    funlab <- as.character(substitute(fun))
    funlab <- funlab[length(funlab)] #handles fun=function(x)mean(x)
  }
  lab <- as.character(sys.call())[-1]
  m <- (!missing(fun)) + (!missing(nmiss)) + (!missing(subset))
  lab <- lab[1:(length(lab)-m)]
  if(length(lab)>2)
    lab2 <- paste(lab[-1],collapse=", ")
  else
    lab2 <- lab[-1]
  
  heading <- if(funlab=="")
               paste(lab[1],"by",lab2)
             else
               paste(funlab,"of",lab[1],"by",lab2)

  nna <- !is.na(y %*% rep(1,ncol(y)))
  N <- sum(nna)
  stats <- fun(y[nna,,drop=FALSE])
  nstats <- length(stats)
  name.stats <- if(length(dn <- dimnames(stats))) 
                  as.vector(outer(dn[[1]],dn[[2]],FUN=function(a,b)paste(b,a)))
                else 
                  names(stats)
  
  if(length(name.stats))
    funlab <- name.stats
  
  if(nstats>1 && length(name.stats)==0)
    funlab <- rep(" ", nstats)
   
  s <- array(NA,dim=c(nv+1,nh+1,2+nstats),
             dimnames=list(c(lv,"ALL"), c(lh,"ALL"), c("N","Missing",funlab)))

  for(xv in c(lv,"ALL")) {
    for(xh in c(lh,"ALL")) {
      if(xv=="ALL" && xh=="ALL")
        st <- c(N, nrow(y)-N, stats)
      else {
        if(xv=="ALL")
          u <- h==xh
        else if(xh=="ALL")
          u <- v==xv
        else
          u <- h==xh & v==xv
        
        if(any(u)) {
          w <- y[u,,drop=FALSE]
          nna <- !is.na(w %*% rep(1,ncol(w)))
          n <- sum(nna)
          st <- c(n, nrow(w)-n, fun(w[nna,,drop=FALSE]))
        } else st <- c(0, n, rep(NA, length(stats)))
      }
      s[xv,xh,] <- st
    }
  }     

  if((!missing(nmiss) && !nmiss) ||
     (missing(nmiss) && all(s[,,"Missing"]==0)))
    s <- s[,,-2,drop=FALSE]
  
  attr(s, "heading")    <- heading
  attr(s, "byvarnames") <- lab[-1]
  attr(s,'class')       <- "bystats2"
  s
}

print.bystats2 <- function(x, abbreviate.dimnames=FALSE, 
                           prefix.width=max(nchar(dimnames(x)[[1]])),...)
{
  cat("\n",attr(x,"heading"),"\n\n")
  if(!exists("print.char.matrix")) {   # Vanilla S
    attr(x, "heading") <- attr(x, "byvarnames") <- attr(x, "class") <-
      NULL
    return(invisible(print(x)))
  }
  
  d <- dim(x)
  cstats <- array("", dim=d[1:3])

  header <- matrix(paste(dimnames(x)[[3]],collapse="\n"),1,1)
  print.char.matrix(header)

  for(k in 1:d[3])
    cstats[,,k] <- format(x[,,k])
  
  dimn <- dimnames(x)[1:2]
  names(dimn) <- attr(x,"byvarnames")
  cstats2 <- matrix("", nrow=d[1], ncol=d[2], dimnames=dimn)
  for(i in 1:d[1]) {
    for(j in 1:d[2]) {
      cstats2[i,j] <- paste(cstats[i,j,],collapse="\n")
    }
  }
  invisible(print.char.matrix(cstats2,...))
}

latex.bystats2 <- function(object,
                           title=first.word(expr=substitute(object)),
                           caption=attr(object, "heading"),
                           rowlabel="", ...)
{
  dm <- dimnames(object)
  inn <- c("%", "<=", "<", ">=", ">", "[")
  out <- c("\\%", "$\\leq$","$<$", "$\\geq$","$>$", "\\verb|[|")
  dm[[1]] <- sedit(dm[[1]], inn, out)
  dm[[2]] <- sedit(dm[[2]],inn,out)
  dm[[3]] <- sedit(dm[[3]],inn,out)
  dimnames(object) <- dm
  caption <- sedit(caption, "cbind", "")
  d <- dim(object)
  dn <- rep(dimnames(object)[[3]], d[2])
  st <- matrix(NA, nrow=d[1], ncol=d[2]*d[3], 
               dimnames=list(dimnames(object)[[1]], dn))

  for(i in 1:d[1]) {
    l <- 0
    for(j in 1:d[2]) {
      for(k in 1:d[3]) {
        l <- l+1
        st[i,l] <- object[i,j,k]
      }
    }
  }

  latex(st, title=title, caption=caption, rowlabel=rowlabel,
        n.rgroup=c(nrow(st)-1,1), 
        cgroup=dimnames(object)[[2]], n.cgroup=rep(d[3],d[2]),...)
}
