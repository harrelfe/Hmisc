## $Id$
under.unix <- !(version$os=='Microsoft Windows' ||
                version$os=='Win32' || version$os=='mingw32')

.R.   <- TRUE
.SV4. <- FALSE

.noGenenerics <- TRUE  # faster loading as new methods not used

if(!exists('existsFunction')) {
  existsFunction <- function(...) exists(..., mode='function')
}

.onAttach <- function(libname, pkgname, ...) {
  verbose <- .Options$Hverbose
  if(!length(verbose) || verbose)
    packageStartupMessage("Hmisc library by Frank E Harrell Jr\n\n",
        "Type library(help='Hmisc'), ?Overview, or ?Hmisc.Overview')\n",
        "to see overall documentation.\n")
  invisible()
}
