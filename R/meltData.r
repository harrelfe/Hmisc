##' Melt a Dataset To Examine All Xs vs Y
##'
##' Uses a formula with one or more left hand side variables (Y) and one or more right hand side variables (X).  Uses [data.table::melt()] to melt `data` so that each X is played against the same Y if `tall='right'` (the default) or each Y is played against the same X combination if `tall='left'`.  The resulting data table has variables Y with their original names (if `tall='right'`) or variables X with their original names (if `tall='left'`), `variable`, and `value`.  By default `variable` is taken as `label()`s of the `tall` variables.
##' @title meltData
##' @param formula a formula
##' @param data data frame or table
##' @param tall see above
##' @param vnames set to `names` to always use variable names instead of labels for X
##' @param sepunits set to `TRUE` to create a separate variable `Units` to hold units of measurement.  The variable is not created if no original variables have a non-blank `units` attribute.
##' @param ... passed to `label()`
##' @return data table
##' @author Frank Harrell
##' @md
##' @seealso [label()]
##' @examples
##' d <- data.frame(y1=(1:10)/10, y2=(1:10)/100, x1=1:10, x2=101:110)
##' label(d$x1) <- 'X1'
##' units(d$x1) <- 'mmHg'
##' m=meltData(y1 + y2 ~ x1 + x2, data=d, units=TRUE) # consider also html=TRUE
##' print(m)
##' m=meltData(y1 + y2 ~ x1 + x2, data=d, tall='left')
##' print(m)
meltData <- function(formula, data, tall=c('right', 'left'),
                     vnames=c('labels', 'names'), sepunits=FALSE, ...) {
  tall   <- match.arg(tall)
  vnames <- match.arg(vnames)
  s <- data.table::copy(data)
  if(! is.data.table(s)) data.table::setDT(s)

  v <- all.vars(formula)
  k <- as.character(formula)
  y <- all.vars(as.formula(paste('~', k[2])))
  x <- all.vars(as.formula(paste('~', k[3])))
  s <- s[, ..v]
  
  labs  <- sapply(s, label, ...)
  labs  <- ifelse(labs == '', names(labs), labs)
  Units <- sapply(s, Hmisc::units)
  id    <- switch(tall,
                  right = y,
                  left  = x)
  ## data.table wants all variables to be melted to have the same type
  for(n in setdiff(v, id)) {
    z <- s[[n]]
    if(! is.factor(z)) set(s, j=n, value=as.double(z))
    }
  m <- data.table::melt(s, id.var=id)
  if(sepunits && any(Units != ''))
    m[, Units := Units[as.character(variable)]]
  if(vnames == 'labels')
    m[, variable := factor(labs[as.character(variable)],
                           levels=labs[levels(variable)]) ]
  m
}

utils::globalVariables(c('..v','.SD','variable'))
