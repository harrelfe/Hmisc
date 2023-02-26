##' Melt a Dataset To Examine All Xs vs Y
##'
##' Uses a formula with a single left hand side variable (Y) and one or more numeric right hand side variables (X).  Uses [data.table::melt()] to melt `data` so that each X is played against the same Y.  The resulting data table has variables Y with its original name, `variable`, and `value`.  By default `variable` is taken as `label()`s of X variables.
##' @title meltData
##' @param formula a formula
##' @param data data frame or table
##' @param vnames set to `names` to always use variable names instead of labels for X
##' @return data table
##' @author Frank Harrell
##' @md
##' @seealso [label()]
##' @examples
##' d <- data.frame(y=(1:10)/10, x1=1:10, x2=101:110)
##' label(d$x1) <- 'X1'
##' m=meltData(y ~ x1 + x2, data=d)
##' print(m)
meltData <- function(formula, data, vnames=c('labels', 'names')) {
  vnames <- match.arg(vnames)
  s <- data.table::copy(data)
  if(! is.data.table(s)) data.table::setDT(s)

  v <- all.vars(formula)
  y <- v[1]
  x <- v[-1]
  s <- s[, ..v]
  labs <- sapply(s, label)
  labs <- ifelse(labs == '', names(labs), labs)
  ## data.table wants all variables to be melted to have the same type
  s <- s[, lapply(.SD, as.double)]
  m <- data.table::melt(s, id.var=y)
  if(vnames == 'labels')
    m[, variable := labs[as.character(variable)]]
  m
}

utils::globalVariables(c('..v','.SD','variable'))
