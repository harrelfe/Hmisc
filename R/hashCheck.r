##' Check for Changes in List of Objects
##'
##' Given an RDS file name and a list of objects, does the following:
##' * makes a vector of hashes, one for each object.  Function objects are run through `deparse` so that the environment of the function will not be considered.
##' * see if the file exists; if not, return a list with result=NULL, `hash` = new vector of hashes, `changed='All'`
##' * if the file exists, read the file and its hash attribute as `prevhash`
##' * if `prevhash` is not identical to hash:
##'     if `.print.=TRUE` (default), print to console a summary of what's changed
##'     return a list with result=NULL, `hash` = new hash vector, changed
##' * if `prevhash = hash`, return a list with result=file object, `hash`=new hash,  changed=''
##'
##' Set `options(debughash=TRUE)` to trace results in `/tmp/debughash.txt`
##' @title hashCheck
##' @param ... a list of objects including data frames, vectors, functions, and all other types of R objects that represent dependencies of a certain calculation
##' @param file name of file in which results are stored
##' @param .print. set to `FALSE` to suppress printing information messages about what has changed
##' @param .names. vector of names of original arguments if not calling `hashCheck` directly
##' @return a `list` with elements `result` (the computations), `hash` (the new hash), and `changed` which details what changed to make computations need to be run
##' @author Frank Harrell
##' @md
hashCheck <- function(..., file, .print.=TRUE, .names.=NULL) {
  .d.      <- list(...)
  .nam.    <- if(length(.names.)) .names. else as.character(sys.call())[-1]
  .nam.    <- .nam.[1 : length(.d.)]
  names(.d.) <- .nam.
  
  .debug. <- length(.Options$debughash) && .Options$debughash
  ct <- if(.debug.)
          function(...) cat(..., '\n', file='/tmp/debughash.txt', append=TRUE)
        else
          function(...) {}

  if(! requireNamespace('digest', quietly=TRUE))
    stop('must install digest package to use hashCheck or runifChanged')

  ct(.nam.)
  .g. <- function(x) digest::digest(if(is.function(x)) deparse(x) else x)
  .hash. <- sapply(.d., .g.)
  if(.debug.) prn(.hash., file='/tmp/debughash.txt')
  
  .prevhash. <- NULL
  if(! file.exists(file)) {
    ct('no file', file)
    return(list(result=NULL, hash=.hash., changed='All'))
    }

  R        <- readRDS(file)
  .prevhash. <- attr(R, 'hash')
if(! length(.prevhash.)) {
    if(.print.) cat('\nRe-run because of no previous hash\n\n')
    ct('no previous hash')
    return(list(result=NULL, hash=.hash., changed='No previous hash'))
    }

  samelen <- length(.hash.) == length(.prevhash.)
  if(samelen && all(.hash. == .prevhash.)) {
    ct('no change')
    return(list(result=R, hash=.hash., changed=''))
    }
  .s. <- character(0)

  if(! samelen) {
    .a. <- names(.prevhash.)
    .b. <- names(.hash.)
    .w. <- setdiff(.a., .b.)
    if(length(.w.))
      .s. <- c(.s., paste('objects removed:',
                      paste(.w., collapse=' ')))
    .w. <- setdiff(.b., .a.)
    if(length(.w.))
      .s. <- c(.s., paste('objects added:',
                      paste(.w., collapse=' ')))
  } else 
    .s. <- c(.s., paste('changes in the following objects:',
                    paste(.nam.[.hash. != .prevhash.], collapse=' ')))

  .s. <- paste(.s., collapse=';')
  ct(.s.)
      
  if(.print.) cat('\nRe-run because of', .s., '\n\n')

  list(result=NULL, hash=.hash., changed=.s.)
}


##' Re-run Code if an Input Changed
##'
##' Uses `hashCheck` to run a function and save the results if specified inputs have changed, otherwise to retrieve results from a file.  This makes it easy to see if any objects changed that require re-running a long simulation, and reports on any changes.  The file name is taken as the chunk name appended with `.rds` unless it is given as `file=`.  `fun` has no arguments.  Set `.inclfun.=FALSE` to not include `fun` in the hash check (for legacy uses).  The typical workflow is as follows.
##' ```
##' f <- function(       ) {
##' # . . . do the real work with multiple function calls ...
##' }
##' seed <- 3
##' set.seed(seed)
##' w <- runifChanged(f, seed, obj1, obj2, ....)
##' ```
##' `seed, obj1, obj2`, ... are all the objects that `f()` uses that if changed
##' would give a different result of `f()`.  This can include functions such as
##' those in a package, and `f` will be re-run if any of the function's code
##' changes.  `f` is also re-run if the code inside `f` changes.
##' The result of `f` is stored with `saveRDS` by default in file named `xxx.rds`
##' where `xxx` is the label for the current chunk.  To control this use instead
##' `file=xxx.rds` add the file argument to `runifChanged(...)`.  If nothing has
##' changed and the file already exists, the file is read to create the result
##' object (e.g., `w` above).  If `f()` needs to be run, the hashed input objects
##' are stored as attributes for the result then the enhanced result is written to the file.
##'
##' See [here](https://hbiostat.org/rflow/caching.html) for examples.
##' 
##' @title runifChanged
##' @param fun the (usually slow) function to run
##' @param ... input objects the result of running the function is dependent on
##' @param file file in which to store the result of `fun` augmented by attributes containing hash digests
##' @param .print. set to `TRUE` to list which objects changed that neessitated re-running `f`
##' @param .inclfun. set to `FALSE` to not include `fun` in the hash digest, i.e., to not require re-running `fun` if only `fun` itself has changed 
##' @return the result of running `fun`
##' @author Frank Harrell
##' @md
runifChanged <- function(fun, ..., file=NULL, .print.=TRUE, .inclfun.=TRUE) {
  if(! length(file)) {
    file <- knitr::opts_current$get('label')
    if(! length(file))
      stop('attempt to run runifChanged without file= outside a knitr chunk')
    file <- paste0(file, '.rds')
  }
  w <- list(...)
  .names. <- (as.character(sys.call())[-1])[1 : (length(w) + 1)]

  hashobj <- if(! .inclfun.) hashCheck(..., file=file,
                                       .print.=.print., .names.=.names.[-1])
             else {
               w <- c(list(fun), w)
               w$file    <- file
               w$.print. <- .print.
               w$.names. <- .names.
               do.call(hashCheck, w)
               }

  hash    <- hashobj$hash
  result  <- hashobj$result
  if(! length(result)) {
    result <- fun()
    attr(result, 'hash') <- hash
    saveRDS(result, file, compress='xz')
  }
  result
}
