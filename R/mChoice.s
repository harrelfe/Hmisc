# $Id$
mChoice <- function(..., label='', sort.=TRUE,
                    sort.levels=c('original','alphabetic'),
                    add.none=FALSE, drop=TRUE)
{
  sort.levels <- match.arg(sort.levels)
  dotlist <- list(...)
  X <- matrix(as.character(unlist(dotlist)), ncol=length(dotlist))
  lev <- if(drop) unique(as.vector(X)) else
   unique(unlist(lapply(dotlist, function(x)levels(as.factor(x)))))
  if(sort.levels=='alphabetic') lev <- sort(lev)
  lev <- setdiff(lev,'')

  vcall <- as.character(sys.call())[-1]
  Y <- character(nrow(X))

  x <- matrix(match(X,lev), nrow=nrow(X))
  g <- function(w, sort.) {
    w <- w[!is.na(w)]
    if(!length(w)) return('')
    paste(if(sort.)sort(unique(w)) else unique(w), collapse=';')
  }
  Y <- apply(x, 1, g, sort.=sort.)

  if(add.none && any(Y=='') && 'none' %nin% lev) {
    lev <- c(lev, 'none')
    Y[Y==''] <- as.character(length(lev))
  }
  
  if(label == '')
    label <- attr(dotlist[[1]],'label')
  
  if(!length(label)) {
    label <- vcall[1]
    if(length(nn <- names(dotlist)[1]))
      label <- nn
  }
  
  structure(Y, label=label, levels=lev, class=c('mChoice','labelled'))
}

print.mChoice <- function(x, long=FALSE, ...) {
  if(long) print(format(x)) else {
    print(as.vector(x), quote=FALSE)
    cat('\nLevels:\n')
    print(attr(x,'levels'), quote=FALSE)
  }
  invisible()
}

format.mChoice <- function(x, minlength=NULL, sep=";", ...)
{
  lev <- attr(x, 'levels')
  if(length(minlength)) lev <- abbreviate(lev, minlength)
  w <- strsplit(x, ';')
  sapply(w, function(x, lev, sep)
         paste(lev[as.numeric(x)], collapse=sep), lev=lev, sep=sep)
}

'[.mChoice' <- function(x, ..., drop=FALSE) {
  if(drop) stop('drop=TRUE not implemented')
  atr <- attributes(x)
  atr$names <- NULL
  x <- NextMethod('[')
  combine(attributes(x)) <- atr
  x
}

as.double.mChoice <- function(x, drop=FALSE, ...) {
  lev <- attr(x,'levels')
  X <- matrix(0, nrow=length(x), ncol=length(lev),
              dimnames=list(names(x), lev))
  unused <- numeric(0)
  for(i in 1:length(lev)) {
    xi <- 1*inmChoice(x, i)
    if(sum(xi)==0) unused <- c(unused, i)
    X[,i] <- xi
  }
  if(drop && length(unused)) X <- X[,-unused,drop=FALSE]
  X
}

summary.mChoice <- function(object, ncombos=5, minlength=NULL, drop=TRUE, ...) {
  nunique <- length(unique(object))
  y <- gsub('[^;]', '', object)
  nchoices <- nchar(y)+1
  nchoices[object == ''] <- 0
  nchoices <- table(nchoices)
  
  X <- as.numeric(object, drop=drop)
  if(length(minlength)) dimnames(X)[[2]] <- abbreviate(dimnames(X)[[2]],minlength)
  crosstab <- crossprod(X)

  combos <- table(format(object, minlength))
  i <- order(-combos)
  combos <- combos[i[1:min(ncombos,length(combos))]]
  
  structure(list(nunique=nunique, nchoices=nchoices,
                 crosstab=crosstab, combos=combos,
                 label=label(object)),
            class='summary.mChoice')
}

print.summary.mChoice <- function(x, prlabel=TRUE, ...) {
  if(prlabel) cat(x$label, '   ', x$nunique, ' unique combinations\n', sep='')
  cat('Frequencies of Numbers of Choices Per Observation\n\n')
  print(x$nchoices)
  crosstab <-format(x$crosstab)
  crosstab[lower.tri(crosstab)] <- ''
  cat('\nPairwise Frequencies (Diagonal Contains Marginal Frequencies)\n')
  print(crosstab, quote=FALSE)
  s <- if(length(x$combos)==x$nunique) 'Frequencies of All Combinations' else
   paste('Frequencies of Top', length(x$combos), 'Combinations')
  cat('\n', s, '\n')
  print(x$combos)
  invisible()
}

inmChoice <- function(x, values) {
  lev <- attr(x, 'levels')
  if(is.character(values)) {
    v <- match(values, lev)
    if(any(is.na(v))) stop(paste('values not in levels:',
                                 paste(values[is.na(v)],collapse=';')))
    values <- v
  }
  x <- paste(';', unclass(x), ';', sep='')
  values <- paste(';', values, ';', sep='')
  res <- rep(FALSE, length(x))
  for(j in 1:length(values)) {
    i <- grep(values[j], x)
    if(length(i)) res[i] <- TRUE
  }
  res
}

is.mChoice <- function(x) inherits(x, 'mChoice')
