##Substitute y when element of x is missing
##also return an attribute "substi.source"=vector of var names and NAs
substi <- function(x,y,pr=TRUE)
{
  if(length(x)!=length(y))
    stop("lengths of x and y are different")

  nf <- is.factor(x) + is.factor(y)
  if(nf==1)
    stop("both x and y must be factor variables if either is")

  isna <- is.na(x)
  vnames <- sys.call()[c(2,3)]
  if(pr) {
    cat("Variables:",vnames,"\n")
    cat("Used first  variable:",sum(!is.na(x)),"\n")
    cat("Used second variable:",sum(is.na(x) & !is.na(y)),"\n")
  }

  if(nf) {
    levs <- unique(c(levels(x),levels(y)))
    x <- as.character(x)
    y <- as.character(y)
    x[isna] <- y[isna]
    x <- factor(x,levs)
    y <- factor(y,levs)
  } else
    x[isna] <- y[isna]

  ss <- ifelse(isna & is.na(y),NA,ifelse(isna,2,1))
  attr(ss,"names") <- NULL
  ss <- factor(ss,labels=vnames)
  if(pr)
    cat("Obs:",sum(!is.na(x))," Obs missing:",sum(is.na(x)),"\n")

  attr(x,"substi.source") <- ss
  attr(x,'class') <- c("substi",attr(x,'class'))
  x
}


substi.source <- function(x) attr(x,"substi.source")


"[.substi" <- function(x, ...)
{
  ss <- attr(x,"substi.source")
  ats <- attributes(x)
  ats$dimnames <- ats$dim <- ats$names <- ats$substi.source <-
    attr(x,'class') <- NULL
  x <- (x)[...]
  attributes(x) <- ats
  attr(x,"substi.source") <- ss[...]
  x
}


print.substi <- function(x, ...)
{
  i <- unclass(attr(x, "substi.source"))
  if(!length(i)) {
    print.default(x)
    return(invisible())
  }

  if(is.factor(x))
    w <- as.character(x)
  else w <- format(x)

  names(w) <- names(x)
  w[i==2] <- paste(w[i==2], "*", sep = "")
  attr(w, "label") <- attr(w, "substi.source") <- attr(w, "class") <- NULL
  print.default(w, quote = FALSE)
  invisible()
}


as.data.frame.substi <- function(x, row.names = NULL, optional = FALSE, ...)
{
  nrows <- length(x)
  if(!length(row.names)) {
    ## the next line is not needed for the 1993 version of data.class and is
    ## included for compatibility with 1992 version
    if(length(row.names <- names(x)) == nrows &&
       !any(duplicated(row.names))) {
    }
    else if(optional)
      row.names <- character(nrows)
    else row.names <- as.character(1:nrows)
  }

  value <- list(x)
  if(!optional)
    names(value) <- deparse(substitute(x))[[1]]

  structure(value, row.names=row.names, class='data.frame')
}
