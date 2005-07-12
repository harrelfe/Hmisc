na.keep <- function(mf)
{
  w <- na.detail.response(mf)
  if(length(w))
    oldClass(w) <- 'keep'  ## 9Apr02
  
  attr(mf, "na.action") <- w
  mf
}


naprint.keep <- function(x, ...)
{
  if(length(x)) {
    cat("\nStatistics on Response by Missing/Non-Missing Status of Predictors\n\n")
    print(oldUnclass(x))
    cat("\n")
  }
  
  invisible()
}


naresid.keep <- function(omit, x, ...) x
