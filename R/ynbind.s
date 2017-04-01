ynbind <- function(..., label=deparse(substitute(...)), asna=c('unknown', 'unspecified'), sort=TRUE) {
w <- list(...)
k <- length(w)
if(! k) stop('no variables to process')
nam <- as.character(sys.call())[-1]
nam <- nam[1 : k]
lab <- nam
W <- matrix(NA, nrow=length(w[[1]]), ncol=k, dimnames=list(NULL, nam))
for(j in 1 : k) {
  x <- w[[j]]
  na <- is.na(x)
  la <- label(x)
  if(la != '') lab[j] <- la
  if(is.numeric(x) && all(x %in% 0 : 1)) x <- x == 1
  if(! is.logical(x)) {
    x <- tolower(as.character(x))
    if(length(asna)) {
      i <- x %in% asna
      if(any(i)) na[i] <- TRUE
    }
    x <- x %in% c('y', 'yes', 'present')
    if(any(na)) x[na] <- NA
  }
  W[, j] <- x
}
## Sort columns in ascending order of overall proportion
prop <- apply(W, 2, mean, na.rm=TRUE)
if(sort) {
  i <- order(prop)
  W <- W[, i, drop=FALSE]
  lab <- lab[i]
}
structure(W, label=label, labels=lab, class=c('ynbind', 'matrix'))
}

'[.ynbind' <- function(x, rows=1:d[1], cols=1:d[2], ...) {
  d <- dim(x)
  at <- attributes(x)[c('label', 'labels')]
  x <- NextMethod('[')
  at$labels <- at$labels[cols]
  attributes(x) <- c(attributes(x), at)
  if(is.matrix(x)) class(x) <- 'ynbind'
  x
  }
 
pBlock <- function(..., subset=NULL, label=deparse(substitute(...))) {
w <- list(...)
k <- length(w)
if(! k) stop('no variables to process')
nam <- as.character(sys.call())[-1]
nam <- nam[1 : k]
lab <- nam
W <- matrix(NA, nrow=length(w[[1]]), ncol=k, dimnames=list(NULL, nam))
for(j in 1 : k) {
  x <- w[[j]]
  na <- is.na(x)
  la <- label(x)
  if(la != '') lab[j] <- la
  W[, j] <- if(is.factor(x)) as.character(x) else x
}
if(length(subset)) {
  if(is.logical(subset) && (length(subset) != nrow(W)))
    stop('length of subset does not match length of analysis variables')
  subset <- if(is.logical(subset)) ! subset else - subset
  W[subset, ] <- NA
}
structure(W, label=label, labels=lab, class=c('pBlock', 'matrix'))
}

'[.pBlock' <- function(x, rows=1:d[1], cols=1:d[2], ...) {
  d <- dim(x)
  at <- attributes(x)[c('label', 'labels')]
  x <- NextMethod('[')
  if (is.matrix(x)) {
    at$labels <- at$labels[cols]
    attributes(x) <- c(attributes(x), at)
    class(x) <- 'pBlock'
  }
  x
  }
