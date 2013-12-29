## $Id$

.noGenenerics <- TRUE  # faster loading as new methods not used

.onAttach <- function(libname, pkgname, ...) {
  verbose <- .Options$Hverbose
  if(length(verbose) && verbose)
    packageStartupMessage("Hmisc library by Frank E Harrell Jr\n\n",
        "Type library(help='Hmisc'), ?Overview, or ?Hmisc.Overview')\n",
        "to see overall documentation.\n")
  invisible()
}
