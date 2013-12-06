na.keep <- function(mf)
{
  w <- na.detail.response(mf)
  if(length(w))
    class(w) <- 'keep'
  
  attr(mf, "na.action") <- w
  mf
}


naprint.keep <- function(x, ...)
{
  if(length(x)) {
    cat("\nStatistics on Response by Missing/Non-Missing Status of Predictors\n\n")
    print(unclass(x))
    cat("\n")
  }
  
  invisible()
}


naresid.keep <- function(omit, x, ...) x
