##' Find Sequential Exclusions Due to NAs
##'
##' Finds the variable with the highest number of `NA`s.  From the non-`NA`s on that variable find the next variable from those remaining with the highest number of `NA`s.  Proceed in like fashion.  The resulting variable summarizes sequential exclusions in a hierarchical fashion.  See [this](https://hbiostat.org/rflow/doverview.html#sec-doverview-filter) for more information.
##' @title seqFreq
##' @param ... any number of variables
##' @param labels if specified variable labels will be used in place of variable names
##' @param noneNA set to `TRUE` to not include 'none' as a level in the result
##' @return `factor` variable with `obs.per.numcond` attribute
##' @author Frank Harrell
##' @md
##' @export
seqFreq <- function(..., labels=NULL, noneNA=FALSE) {
  d <- list(...)
  k <- length(d)
  if(length(labels)) nam <- labels
  else {
    s <- as.character(sys.call()[-1])[1 : k]
    nam <- names(d)
    if(! length(nam)) nam <- rep('', k)
    nam <- ifelse(nam == '', s, nam)
    }

  ispos <- function(x) {
    w <- if(is.logical(x)) x
    else if(is.numeric(x)) x > 0
    else tolower(as.character(x)) %in%
           c('present', 'yes', 'y', 'true', 'positive', 'pos', '+')
    w[is.na(x)] <- FALSE
    1L * w
  }

  names(d) <- nam
  x <- sapply(d, ispos)  # creates a matrix
  # Count number of positives per observation
  po   <- apply(x, 1, sum)
  cond <- c(sum(po == 0), tabulate(po, nbins=k))

  j <- integer(k)
  w <- rep(0, nrow(x))
  for(i in 1 : k) {
    freqs <- apply(x, 2, sum)
    if(all(freqs == 0)) break
    imax  <- which.max(freqs)
    j[i]  <- imax
    w <- ifelse(w == 0 & x[, imax], imax, w)
    x[x[, imax] == 1, ] <- 0
  }
j <- j[j != 0]
x <- if(noneNA) factor(w, j, nam[j]) else
                factor(w, c(0, j), c('none', nam[j]))
attr(x, 'obs.per.numcond') <- cond
x
}

