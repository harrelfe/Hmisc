##' Find File With Latest Modification Time
##'
##' Subject to matching on `pattern` finds the last modified file, and if `verbose` is `TRUE` reports on how many total files matched `pattern`.
##' @title latestFile
##' @param pattern a regular expression; see [base::list.files()]
##' @param path full path, defaulting to current working directory
##' @param verbose set to `FALSE` to not report on total number of matching files
##' @return the name of the last modified file
##' @author Frank Harrell
##' @seealso [base::list.files()]
##' @md
latestFile <- function(pattern, path='.', verbose=TRUE) {
  f <- list.files(path=path, pattern=pattern)
  if(length(f) == 1) return(f)
  if(length(f) == 0) {
    warning(paste('no files matching', pattern, 'were found'))
    return(character(0))
  }

  i <- file.info(f, extra_cols=FALSE)
  mtime <- i$mtime
  j <- order(mtime, decreasing=TRUE)[1]
  if(verbose) cat('\nLast modified file: ', f[j],
                  '  (of ', length(f), ' files)\n\n', sep='')
  f[j]
}
