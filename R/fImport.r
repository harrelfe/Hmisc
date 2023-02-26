##' General File Import Using `rio`
##'
##' This is a front-end for the `rio` package's `import` function.  `fImport` includes options for setting variable names to lower case and to change underscores in names to periods.  Variables on the imported data frame that have `label`s are converted to Hmisc package `labelled` class so that subsetting the data frame will preserve the labels.
##' @title fImport
##' @param file name of file to import, or full URL.  `rio` determines the file type from the file suffix unless you override this with `format`
##' @param format format of file to import, usually not needed.  See `rio::import()` for details
##' @param lowernames defaults to changing variable names to all lower case unless the name as mixed upper and lower case, which results in keeping the original characters in the name.  Set `lowernames='no'` to leave variable names as they were created in the original file export, or set `lowernames='yes'` to set all names to lower case whether they have mixed case or not.  For all options, a check is made to see if the name conversions would result in any duplicate names.  If so, the original names are retained and a warning message issued.
##' @param und. set to `TRUE` to change all underscores in names to periods
##' @param ... more arguments to pass to `rio::import()`
##' @return a data frame created by `rio`, unless a `rio` option is given to use another format
##' @md
##' @seealso `upData`, especially the `moveUnits` option
##' @author Frank Harrell
##' @examples
##' \dontrun{
##' # Get a Stata dataset
##' d <- fImport('http://www.principlesofeconometrics.com/stata/alcohol.dta')
##' contents(d)
##' }
fImport <- function(file, format,
                    lowernames=c('not mixed', 'no', 'yes'),
                    und.=FALSE, ...) {
if(! requireNamespace('rio', quietly=TRUE))
    stop('requires rio package to be installed')

lowernames <- match.arg(lowernames)

d <- rio::import(file, format, ...)
n <- names(d)

labs <- sapply(d, label)
# Make labelled variables have labelled class so subsetting will preserve labels]
for(x in n[labs != '']) label(d[[x]]) <- labs[x]

chkdup <- function(new, old, txt) {
    j <- duplicated(new)
    if(any(j)) {
      warning(paste('variables would have duplicate names if', txt, ':\n',
                    paste(old[j], collapse=', ')))
      return(old)
    }
    new
}

if(und.) {
    m <- gsub('_', '\\.', n)
    n <- chkdup(m, n, '_ changed to . so und. set to FALSE')
}

if(lowernames == 'yes') {
    m <- tolower(n)
    n <- chkdup(m, n, 'changed to all lower case so lowernames set to no')
}

if(lowernames == 'not mixed') {
    mixed <- grepl('[A-Z]', n) & grepl('[a-z]', n)
    m <- ifelse(mixed, n, tolower(n))
    n <- chkdup(m, n, 'changed upper case-only variable names to all lower case so lowernames set to no')
}

names(d) <- n
d
}
