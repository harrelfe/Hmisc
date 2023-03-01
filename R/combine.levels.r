##' Combine Infrequent Levels of a Categorical Variable
##'
##' After turning `x` into a `factor` if it is not one already, combines
##' levels of `x` whose frequency falls below a specified relative frequency `minlev` or absolute count `m`.  When `x` is not treated as ordered, all of the
##' small frequency levels are combined into `"OTHER"`, unless `plevels=TRUE`.
##' When `ord=TRUE` or `x` is an ordered factor, only consecutive levels
##' are combined.  New levels are constructed by concatenating the levels with
##' `sep` as a separator.  This is useful when comparing ordinal regression
##' with polytomous (multinomial) regression and there are too many
##' categories for polytomous regression.  `combine.levels` is also useful
##' when assumptions of ordinal models are being checked empirically by
##' computing exceedance probabilities for various cutoffs of the
##' dependent variable.
##' @title combine.levels
##' @param x a factor, `ordered` factor, or numeric or character variable that will be turned into a `factor`
##' @param minlev the minimum proportion of observations in a cell before that cell is combined with one or more cells.  If more than one cell has fewer than minlev*n observations, all such cells are combined into a new cell labeled `"OTHER"`.  Otherwise, the lowest frequency cell is combined with the next lowest frequency cell, and the level name is the combination of the two old level levels. When `ord=TRUE` combinations happen only for consecutive levels.
##' @param m alternative to `minlev`, is the minimum number of observations in a cell before it will be combined with others
##' @param ord set to `TRUE` to treat `x` as if it were an ordered factor, which allows only consecutive levels to be combined
##' @param plevels by default `combine.levels` pools low-frequency levels into a category named `OTHER` when `x` is not ordered and `ord=FALSE`.  To instead name this category the concatenation of all the pooled level names, separated by a comma, set `plevels=TRUE`.
##' @param sep the separator for concatenating levels when `plevels=TRUE`
##' @return a factor variable, or if `ord=TRUE` an ordered factor variable
##' @author Frank Harrell
##' @examples
##' x <- c(rep('A', 1), rep('B', 3), rep('C', 4), rep('D',1), rep('E',1))
##' combine.levels(x, m=3)
##' combine.levels(x, m=3, plevels=TRUE)
##' combine.levels(x, ord=TRUE, m=3)
##' x <- c(rep('A', 1), rep('B', 3), rep('C', 4), rep('D',1), rep('E',1),
##'        rep('F',1))
##' combine.levels(x, ord=TRUE, m=3)

## Modified 27Feb23 - added ord
combine.levels <- function(x, minlev=.05, m, ord=is.ordered(x),
                           plevels=FALSE, sep=',') {

  x  <- as.factor(x)
  i  <- ! is.na(x)
  nu <- length(unique(x[i]))
  if(nu < 2) return(x)
  notna <- sum(i)
  if(notna == 0) return(x)
  lev <- levels(x)
  n   <- table(x)   # excludes NAs in tabulation
  f   <- n / notna
  if(! missing(m)) minlev <- m / notna
  i   <- f < minlev
  si  <- sum(i)
  if(si == 0) return(x)

  if(ord) {
    if(nu > 250) warning('combine.levels with ord=TRUE is slow with more than 250 distinct values.\nConsider using cut2().')
    if(missing(m)) m <- round(minlev * notna)
    if(sum(n) < 2 * m)
      stop(paste('with m=', m, 'must have >=', 2 * m,
                 'non-missing observations'))
    levs <- newlevs <- names(n)
    names(n) <- NULL
    l <- length(levs)
    while(TRUE) {  
      ## Find first value of x having < m observations when counting
      ## by the current combined levels
      ## If it's the first level, combine it with the 2nd level
      ## If it's the last level, combine it with the next-to-last level
      ## Otherwise, combine it with the smaller of the previous level
      ## and the next level
      ## factor() below makes tapply keep levels in order
      freq <- tapply(n, factor(newlevs, unique(newlevs)), sum)
      i    <- which.min(freq)
      if(freq[i] >= m) break
      l <- length(freq)
      j <- if(i == 1) 1:2
           else if(i == l) (l-1) : l
           else if(freq[i-1] < freq[i+1]) (i-1) : i else i : (i+1)
      ## freq,i,j are aligned with unique(newlevs)
      ## Need to make changes index into the longer newlevs
      unewlevs <- names(freq)
      newlevs[newlevs %in% unewlevs[j]] <- paste(unewlevs[j], collapse=sep)
    }
    levels(x) <- newlevs
    return(ordered(x))
  }

  comb <- if(si == 1) names(sort(f))[1 : 2]
  else names(f)[i]
  
  keepsep <- setdiff(names(f), comb)
  names(keepsep) <- keepsep
  w <- c(list(OTHER=comb), keepsep)
  if(plevels) names(w)[1] <- paste(comb, collapse=sep)
  levels(x) <- w
  x
}
