##' parallel Package Easy Front-End
##'
##' Given a function `onecore` that runs the needed set of simulations on
##' one CPU core, and given a total number of repetitions `reps`, determines
##' the number of available cores and by default uses one less than that.
##' By default the number of cores is one less than the number available
##' on your machine.
##' reps is divided as evenly as possible over these cores, and batches
##' are run on the cores using the `parallel` package `mclapply` function.
##' The current per-core repetition number is continually updated in
##' your system's temporary directory (/tmp for Linux and Mac, TEMP for Windows)
##' in a file name progressX.log where X is the core number.
##' The random number seed is set for each core and is equal to
##' the scalar `seed` - core number + 1.  The default seed is a random
##' number between 0 and 10000 but it's best if the user provides the
##' seed so the simulation is reproducible.
##' The total run time is computed and printed
##' onefile must create a named list of all the results created during
##' that one simulation batch.  Elements of this list must be data frames,
##' vectors, matrices, or arrays.   Upon completion of all batches,
##' all the results are rbind'd and saved in a single list.
##'
##' onecore must have an argument `reps` that will tell the function
##' how many simulations to run for one batch, another argument `showprogress`
##' which is a function to be called inside onecore to write to the
##' progress file for the current core and repetition, and an argument `core`
##' which informs `onecore` which sequential core number (batch number) it is
##' processing.
##' When calling `showprogress` inside `onecore`, the arguments, in order,
##' must be the integer value of the repetition to be noted, the number of reps,
##' `core`, an optional 4th argument `other` that can contain a single
##' character string to add to the output, and an optional 5th argument `pr`.
##' You can set `pr=FALSE` to suppress printing and have `showprogress`
##' return the file name for holding progress information if you want to
##' customize printing.
##'
##' If any of the objects appearing as list elements produced by onecore
##' are multi-dimensional arrays, you must specify an integer value for
##' `along`.  This specifies to the `abind` package `abind` function
##' the dimension along which to bind the arrays.  For example, if the
##' first dimension of the array corresponding to repetitions, you would
##' specify along=1.   All arrays present must use the same `along` unless
##' `along` is a named vector and the names match elements of the
##' simulation result object.
##' Set `simplify=FALSE` if you don't want the result simplified if
##' onecore produces only one list element.  The default returns the
##' first (and only) list element rather than the list if there is only one
##' element.
##'
##' When `onecore` returns a `data.table`, `runParallel` simplifies all this and merely
##' rbinds all the per-core data tables into one large data table.  In that case when you
##' have `onecore` include a column containing a simulation number, it is wise to prepend
##' that number with the core number so that you will have unique simulation IDs when
##' all the cores' results are combined.
##'
##' See [here](https://hbiostat.org/rflow/parallel.html) for examples.
##'
##' @title runParallel
##' @param onecore function to run the analysis on one core
##' @param reps total number of repetitions
##' @param seed species the base random number seed.  The seed used for core i will be `seed` + `i`.
##' @param cores number of cores to use, defaulting to one less than the number available
##' @param simplify set to FALSE to not create an outer list if a `onecore` result has only one element
##' @param along see Details
##' @return result from combining all the parallel runs, formatting as similar to the result produced from one run as possible
##' @author Frank Harrell
##' @md
runParallel <- function(onecore, reps, seed=round(runif(1, 0, 10000)),
                        cores=max(1, parallel::detectCores() - 1),
                        simplify=TRUE, along) {

  if(! requireNamespace('parallel', quietly=TRUE))
    stop('requires parallel package')
  
  progressDir <- if(Sys.info()['sysname'] == 'Darwin') '/tmp' else paste0(dirname(tempdir()))
  stime <- Sys.time()

  ## Function to divide n things as evenly as possible into m groups
  ## See https://math.stackexchange.com/questions/199690
  evenly <- function(n, m) {
    a <- floor(n / m)
    r <- n %% m
    w <- c(rep(a + 1, r), rep(a, m - r))
    if(sum(w) != n) stop('program logic error')
    w
  }
  repsc <- evenly(reps, cores)
  showprogress <- function(i=0, reps=0, core, other='', pr=TRUE) {
    file <- paste0(progressDir, '/progress', core, '.log')
    if(other != '') other <- paste0(other, '   ')
    if(pr) cat(other, i, ' of ', reps, '\n', sep='', file=file)
    invisible(file)
    }
  ff <- function(i) {
    set.seed(seed + i - 1)
    onecore(reps=repsc[i], showprogress=showprogress, core=i)
  }
  v <- parallel::mclapply(1 : cores, ff, mc.cores=cores, mc.set.seed=FALSE)
  v1 <- v[[1]]
  ite <- sapply(v, function(z) inherits(z, 'try-error'))
  if(any(ite)) {
    z <- sapply(v, function(x) {
      x <- as.character(attr(x, 'condition'))
      if(length(x)) x else '' })
    stop(paste(z, collapse=';'))
    }
  etime <- Sys.time()
  cat('\nRun time:', format(etime - stime), 'using', cores, 'cores\n')
  ## Separately for each element of each list in w, stack the results so
  ## the use can treat them as if from a single run
  ## If the result of onecore is a data table, just rbind all the data tables
  ## The user may want to concatenate the core number to the simulation number in that case
  
  if(data.table::is.data.table(v1)) return(data.table::rbindlist(v))

  m <- length(v1)   # number of elements in a per-core list
  R <- vector('list', m)
  names(R) <- names(v1)

  u <- function(j) {
    x <- lapply(v, function(x) x[[j]])
    z <- x[[1]]
    if(is.matrix(z)) x <- do.call('rbind', x)
    else
      if(is.list(z)) x <- data.table::rbindlist(x)
    else if(is.array(z)) {
      if(! requireNamespace('abind', quietly=TRUE))
        stop('must install the abind package to handle arrays')
      al <- if(length(along) == 1) along else along[names(v1)[j]]
      x <- do.call(abind::abind, list(x, along=al))
    }
    else if(! is.atomic(z))
      stop(paste('list element', j,
       ' of result returned by onecore is not data.frame, matrix, array, or vector'))
    else x <- unlist(x)   # vectors
    x
    }
    
  for(j in 1: m)         R[[j]] <- u(j)
  if(simplify && m == 1) R[[1]] else R
  }
