##' Pair-up and Compute Differences
##'
##' This function sets up for plotting half-width confidence intervals for differences, sorting by descending order of differences within major categories, especially for dot charts as produced by [dotchartpl()]. Given a numeric vector `x` and a grouping (superpositioning) vector `group` with exactly two levels, computes differences in possibly transformed `x` between levels of `group` for the two observations that are equal on `major` and `minor`.  If `lower` and `upper` are specified, using `conf.int` and approximate normality on the transformed scale to backsolve for the standard errors of estimates, and uses approximate normality to get confidence intervals on differences by taking the square root of the sum of squares of the two standard errors.  Coordinates for plotting half-width confidence intervals are also computed.  These intervals may be plotted on the same scale as `x`, having the property that they overlap the two `x` values if and only if there is no "significant" difference at the `conf.int` level.
##' @title pairUpDiff
##' @param x a numeric vector
##' @param major an optional factor or character vector
##' @param minor an optional factor or character vector
##' @param group a required factor or character vector with two levels
##' @param refgroup a character string specifying which level of `group` is to be subtracted
##' @param lower an optional numeric vector giving the lower `conf.int` confidence limit for `x`
##' @param upper similar to `lower` but for the upper limit
##' @param minkeep the minimum value of `x` required to keep the observation.  An observation is kept if either `group` has `x` exceeding or equalling `minkeep`.  Default is to keep all observations.
##' @param sortdiff set to `FALSE` to avoid sorting observations by descending between-`group` differences
##' @param conf.int confidence level; must have been the value used to compute `lower` and `upper` if they are provided 
##' @return a list of two objects both sorted by descending values of differences in `x`.  The `X` object is a data frame that contains the original variables sorted by descending differences across `group` and in addition a variable `subscripts` denoting the subscripts of original observations with possible re-sorting and dropping depending on `sortdiff` and `minkeep`.  The `D` data frame contains sorted differences (`diff`), `major`, `minor`, `sd` of difference, `lower` and `upper` confidence limits for the difference, `mid`, the midpoint of the two `x` values involved in the difference, `lowermid`, the midpoint minus 1/2 the width of the confidence interval, and `uppermid`, the midpoint plus 1/2 the width of the confidence interval.  Another element returned is `dropped` which is a vector of `major` / `minor` combinations dropped due to `minkeep`.
##' @author Frank Harrell
##' @export
##' @md
##' @examples
##' x <- c(1, 4, 7, 2, 5, 3, 6)
##' pairUpDiff(x, c(rep('A', 4), rep('B', 3)),
##'   c('u','u','v','v','z','z','q'),
##'   c('a','b','a','b','a','b','a'), 'a', x-.1, x+.1)
    
pairUpDiff <- function(x, major=NULL, minor=NULL, group, refgroup,
                       lower=NULL, upper=NULL, minkeep=NULL,
                       sortdiff=TRUE, conf.int=0.95) {

  n <- length(x)
  major <- if(! length(major)) rep(' ', n) else as.character(major)
  minor <- if(! length(minor)) rep(' ', n) else as.character(minor)
  ## Note: R will not let you use z[cbind(...)] if one of the dimensions
  ## is of length 1 and equal to ''; needed to use ' '
  group <- as.character(group)
  glev  <- unique(group)
  if(length(glev) != 2) stop('group must have two distinct values')
  if(refgroup %nin% glev) stop('refgroup must be one of the group values')
  altgroup <- setdiff(glev, refgroup)

  mm <- c(major, minor)
  sep <- if(! any(grepl(':', mm))) ':'
         else
           if(! any(grepl('|', mm))) '|'
         else
           if(! any(grepl(';', mm))) ';'
         else
           if(! any(grepl('!', mm))) '!'
         else
           stop('major or minor contain all delimiters :|;!')
  
  m  <- paste0(major, sep, minor)
  u  <- unique(m)
  lu <- length(u)

  lowup <- length(lower) * length(upper) > 0
  zcrit <- qnorm((1 + conf.int) / 2)

  ## See if any observations should be dropped
  dropped <- NULL
  if(length(minkeep)) {
    xa       <- xb <- structure(rep(NA, lu), names=u)
    j        <- group == refgroup
    xa[m[j]] <- x[j]
    j        <- group == altgroup
    xb[m[j]] <- x[j]
    j        <- ! is.na(xa + xb) & (xa < minkeep & xb < minkeep)
    if(any(j)) {
      dropped <- names(xa)[j]
      u  <- setdiff(u, dropped)
      lu <- length(u)
      }
    }
  
  xa <- xb <- sda <- sdb <- diff <- mid <- dsd <- dlower <-
    dupper <- dlowermid <- duppermid <-  structure(rep(NA, lu), names=u)
  j     <- (group == refgroup) & (m %nin% dropped)
  w     <- m[j]
  xa[w] <- x[j]
  if(lowup) sda[w] <- 0.5 * (upper[j] - lower[j]) / zcrit
    
  j     <- (group == altgroup) & (m %nin% dropped)
  w     <- m[j]
  xb[w] <- x[j]
  if(lowup) sdb[w] <- 0.5 * (upper[j] - lower[j]) / zcrit

  diff[u]   <- xb[u] - xa[u]
  if(lowup) {
    dsd[u]    <- sqrt(sda[u] ^ 2 + sdb[u] ^ 2)
    dlower[u] <- diff[u] - zcrit * dsd[u]
    dupper[u] <- diff[u] + zcrit * dsd[u]
    }
  mid[u]    <- (xa[u] + xb[u]) / 2.
  if(lowup) {
    dlowermid[u] <- mid[u] - 0.5 * zcrit * dsd[u]
    duppermid[u] <- mid[u] + 0.5 * zcrit * dsd[u]
    }

  k  <- strsplit(u, sep)
  ma <- sapply(k, function(x) x[[1]])
  mi <- sapply(k, function(x) x[[2]])

ww <- list(x, major, minor, group, lower, upper, subscripts=1:length(x))

  Z <- if(lowup) 
         data.frame(x, major, minor, group, lower, upper,
                    subscripts=1 : length(x))
       else
         data.frame(x, major, minor, group, subscripts=1 : length(x))

  if(length(dropped)) Z <- Z[m %nin% dropped, ]
  if(sortdiff) {
    m <- paste0(Z$major, sep, Z$minor)
    ## diff[m] is a table lookup; difference will be same for both groups
    j <- order(Z$major, ifelse(is.na(diff[m]), -Inf, - diff[m]))
    Z <- Z[j, ]
    }

  ## Variables referenced below have already had observations dropped
  ## due to minkeep
  D <- data.frame(diff=diff[u], major=ma, minor=mi, sd=dsd[u],
                  lower=dlower[u], upper=dupper[u],
                  mid=mid[u], lowermid=dlowermid[u], uppermid=duppermid[u])
  if(sortdiff) {
    j <- order(D$major, ifelse(is.na(D$diff), -Inf, -D$diff))
    D <- D[j, ]
    }
  list(X=Z, D=D, dropped=dropped)
  }
