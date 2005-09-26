## $Id$
under.unix <- !(version$os=='Microsoft Windows' ||
                version$os=='Win32' || version$os=='mingw32')

.R.   <- TRUE
.SV4. <- FALSE

.noGenenerics <- TRUE  # faster loading as new methods not used

if(!exists('existsFunction')) {
  existsFunction <- function(...) exists(..., mode='function')
}

.First.lib <- function(lib, pkg, ...)
{
  verbose <- .Options$Hverbose
  if(!length(verbose) || verbose)
    cat("Hmisc library by Frank E Harrell Jr\n\n",
        "Type library(help='Hmisc'), ?Overview, or ?Hmisc.Overview')\n",
        "to see overall documentation.\n\n",
        "NOTE:Hmisc no longer redefines [.factor to drop unused levels when\n",
        "subsetting.  To get the old behavior of Hmisc type dropUnusedLevels().\n",
        sep='')
  library.dynam("Hmisc", pkg, lib)
  invisible()
}
