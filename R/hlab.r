##' Easy Extraction of Labels/Units Expressions for Plotting
##'
##' Given a single unquoted variable, first looks to see if a non-`NULL` `LabelUnits` object exists (produced by `extractlabs()`).  When `LabelUnits` does not exist or is `NULL`, looks up the attributes in the current dataset, which defaults to `d` or may be specified by `options(current_ds='name of the data frame/table')`.  When a variable is not found in either source or has a blank `label` and `units`, an `expression()` with the variable name alone is returned.  If `html=TRUE`, HTML strings are constructed instead, suitable for `plotly` graphics.
##'
##' The result is useful for `xlab` and `ylab` in base plotting functions or in `ggplot2`, along with being useful for `labs` in `ggplot2`.  See example.
##' @title hlab
##' @param x a single variable name, unquoted
##' @param name a single character string providing an alternate way to name `x` that is useful when `hlab` is called from another function such as `hlabs`
##' @param html set to `TRUE` to return HTML strings instead of `plotmath` expressions
##' @return an expression created by `labelPlotmath` with `plotmath=TRUE`
##' @author Frank Harrell
##' @seealso [label()], [units()], [contents()], [hlabs()], [extractlabs()], [plotmath]
##' @md
##' @examples
##' d <- data.frame(x=1:10, y=(1:10)/10)
##' d <- upData(d, labels=c(x='X', y='Y'), units=c(x='mmHg'), print=FALSE)
##' hlab(x)
##' hlab(x, html=TRUE)
##' hlab(z)
##' require(ggplot2)
##' ggplot(d, aes(x, y)) + geom_point() + labs(x=hlab(x), y=hlab(y))
##' # Can use xlab(hlab(x)) + ylab(hlab(y)) also
##' # Store names, labels, units for all variables in d in object
##' LabelsUnits <- extractlabs(d)
##' # Remove d; labels/units still found
##' rm(d)
##' hlab(x)
##' # Remove LabelsUnits and use a current dataset named
##' # d2 instead of the default d
##' rm(LabelsUnits)
##' options(current_ds='d2')
hlab <- function(x, name=NULL, html=FALSE) {
  xname <- if(length(name)) name else as.character(substitute(x))
  ldef  <- labelPlotmath(xname, html=html)
  lu    <- if(exists('LabelsUnits')) LabelsUnits
  if(length(lu)) {
    if(xname %nin% lu$name) return(ldef)
    lu <- lu[xname][1]
    return(labelPlotmath(lu$label, lu$units, html=html))
  }
  
  currds <- getOption('current_ds', 'd')
  if(! exists(currds)) return(ldef)
  d <- get(currds)
  if(xname %nin% names(d)) return(ldef)
  label(d[[xname]], plot=TRUE, default=xname, html=html)
}

##' Front-end to ggplot2 labs Function
##'
##' Runs `x`, `y`, or both through [hlab()] and passes the constructed labels to the [ggplot2::labs] function to specify x- and y-axis labels specially formatted for units of measurement
##' @title hlabs
##' @param x a single variable name, unquoted
##' @param y a single variable name, unquoted
##' @param html set to `TRUE` to render in html (for `plotly`), otherwise the result is `plotmath` expressions
##' @return result of [ggplot2::labs()]
##' @author Frank Harrell
##' @md
##' @examples
##' # Name the current dataset d, or specify a name with
##' # options(curr_ds='...') or run `extractlabs`, then
##' # ggplot(d, aes(x,y)) + geom_point() + hlabs(x,y)
##' # to specify only the x-axis label use hlabs(x), or to
##' # specify only the y-axis label use hlabs(y=...)
hlabs <- function(x, y, html=FALSE) {
  x <- as.character(substitute(x))   # results in '' if no x
  y <- as.character(substitute(y))
  if(y == '')      labs(x=hlab(name=x, html=html))
  else if(x == '') labs(y=hlab(name=y, html=html))
  else             labs(x=hlab(name=x, html=html),
                        y=hlab(name=y, html=html))
}

##' Easily Retrieve Text Form of Labels/Units
##'
##' Uses the same search method as `hlab` returns label and units in a character string with units, if present, in brackets
##' @title vlab
##' @param x a single variable name, unquoted
##' @param name optional character string to use as variable name 
##' @return character string
##' @author Frank Harrell
##' @md
##' @seealso [hlab()]
vlab <- function(x, name=NULL) {
  xname <- if(length(name)) name else as.character(substitute(x))
  ldef  <- label(x, units=TRUE, default=xname)
  lu    <- if(exists('LabelsUnits')) LabelsUnits
  if(length(lu)) {
    if(xname %nin% lu$name) return(ldef)
    lu <- lu[xname][1]
    return(labelPlotmath(lu$label, lu$units, plotmath=FALSE))
  }
  
  currds <- getOption('current_ds', 'd')
  if(! exists(currds)) return(ldef)
  d <- get(currds)
  if(xname %nin% names(d)) return(ldef)
  label(d[[xname]], units=TRUE, default=xname)
}


##' Extract Labels and Units From Multiple Datasets
##'
##' For one or more data frames/tables extracts all labels and units and comb ines them over dataset, dropping any variables not having either labels or units defined.  The resulting data table is returned and is used by the `hlab` function if the user stores the result in an objectnamed `LabelsUnits`.  The result is `NULL` if no variable in any dataset has a non-blank `label` or `units`.  Variables found in more than one dataset with duplicate `label` and `units` are consolidated.  A warning message is issued when duplicate variables have conflicting labels or units, and by default, details are printed.  No attempt is made to resolve these conflicts.
##' @title extractlabs 
##' @param ... one ore more data frames or data tables
##' @param print set to `FALSE` to not print details about variables with conflicting attributes
##' @return a data table
##' @author Frank Harrell
##' @md
##' @seealso [label()], [contents()], [units()], [hlab()]
##' @examples
##' d <- data.frame(x=1:10, y=(1:10)/10)
##' d <- upData(d, labels=c(x='X', y='Y'), units=c(x='mmHg'), print=FALSE)
##' d2 <- d
##' units(d2$x) <- 'cm'
##' LabelsUnits <- extractlabs(d, d2)
##' LabelsUnits
extractlabs <- function(..., print=TRUE) {
  dotlist <- list(...)
  labu <- function(d)
    list(name=names(d), label=sapply(d, label), units=sapply(d, units))
  u <- data.table::rbindlist(lapply(dotlist, labu))
  data.table::setkeyv(u, 'name')
  lu <- if(any(u$label != '') || any(u$units != ''))
          u[label != '' | units != '']
  w <- lu[, .(nc=length(unique(paste(label, units, sep='|')))), by=name]
  nconflict <- sum(w$nc > 1)
  if(nconflict) {
    warning(paste(nconflict,
        'variables have conflicting labels/units from different datasets'))
    if(print) {
      cname <- w$name[w$nc > 1]
      cat('Variable names with inconsistent attributes:\n')
      print(lu[name %in% cname])
      cat('\n')
      }
    }
  unique(lu)
  }

utils::globalVariables(c('name', 'LabelsUnits'))

