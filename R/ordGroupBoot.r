#' Minimally Group an Ordinal Variable So Bootstrap Samples Will Contain All Distinct Values
#'
#' When bootstrapping models for ordinal Y when Y is fairly continuous, it is frequently the case that one or more bootstrap samples will not include one or more of the distinct original Y values.  When fitting an ordinal model (including a Cox PH model), this means that an intercept cannot be estimated, and the parameter vectors will not align over bootstrap samples.  To prevent this from happening, some grouping of Y may be necessary.  The `ordGroupBoot` function uses [Hmisc::cutGn()] to group Y so that the minimum number in any group is guaranteed to not exceed a certain integer `m`.  `ordGroupBoot` tries a range of `m` and stops at the lowest `m` such that either all `B` tested bootstrap samples contain all the original distinct values of Y (if `B`>0), or that the probability that a given sample of size `n` with replacement will contain all the distinct original values exceeds `aprob` (`B`=0).  This probability is computed approximately using an approximation to the probability of complete sample coverage from the _coupon collector's problem_ and is quite accurate for our purposes.
#'
#' @param y a numeric vector
#' @param B number of bootstrap samples to test, or zero to use a coverage probability approximation
#' @param m range of minimum group sizes to test; the default range is usually adequate
#' @param what specifies that either the mean `y` in each group should be returned, a `factor` version of this with interval endpoints in the levels, or the computed value of `m` should be returned
#' @param aprob minimum coverage probability sought
#' @param pr set to `FALSE` to not print the computed value of the minimum `m` satisfying the needed condition
#'
#' @returns a numeric vector corresponding to `y` but grouped, containing eithr the mean of `y` in each group or a factor variable representing grouped `y`, either with the minimum `m` that satisfied the required sample covrage
#' @seealso [cutGn()]
#' @export
#' @author Frank Harrell
#' @md
#'
#' @examples
#' set.seed(1)
#' x <- c(1:6, NA, 7:22)
#' ordGroupBoot(x, m=5:10)
#' ordGroupBoot(x, m=5:10, B=5000, what='factor')
ordGroupBoot <- function(y, B=0, m=7:min(15, floor(n / 3)),
                         what=c('mean', 'factor', 'm'),
                         aprob=0.9999, pr=TRUE) {
  what <- match.arg(what)
  n    <- sum(! is.na(y))

  for(mm in m) {
    yr  <- cutGn(y, m=mm, what=if(what == 'm') 'mean' else what)
    if(B == 0) {
      tab <- table(yr)
      p <- 1 - sum(exp(- tab))
      if(p > aprob) {
        if(pr) cat('Minimum m:', mm, '\n')
        return(if(what == 'm') mm else yr)
      }
      next
    }
    yru <- unique(yr)
    for(i in 1 : B) {
      yb <- sample(yr, replace=TRUE)
      if(! all(yru %in% yb)) break
    }
    if(i == B) {
      if(pr) cat('Minimum m:', mm, '\n')
      return(if(what == 'm') mm else yr)
    }
  }
  stop('no value of m tested will sufficiently guarantee coverage of original values in samples with replacement')
}
