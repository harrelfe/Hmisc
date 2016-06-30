#' Merge Multiple Data Frames or Data Tables
#'
#' Merges an arbitrarily large series of data frames or data tables containing common \code{id} variables (keys for data tables).  Information about number of observations and number of unique \code{id}s in individual and final merged datasets is printed.  The first data frame has special meaning in that all of its observations are kept whether they match \code{id}s in other data frames or not.  For all other data frames, by default non-matching observations are dropped.  The first data frame is also the one against which counts of unique \code{id}s are compared.  Sometimes \code{merge} drops variable attributes such as \code{labels} and \code{units}.  These are restored by \code{Merge}.  If all objects are of class \code{data.table}, faster merging will be done using the \code{data.table} package's join operation.  This assumes that all objects have identical key variables and those of the variables on which to merge.
#'
#' @param \dots two or more dataframes or data tables
#' @param id a formula containing all the identification variables such that the combination of these variables uniquely identifies subjects or records of interest.  May be omitted for data tables; in that case the \code{key} function retrieves the id variables.
#' @param all set to \code{FALSE} to drop observations not found in second and later data frames (only applies if not using \code{data.table})
#' @param verbose set to \code{FALSE} to not print information about observations
#' @export
#' @examples
#' a <- data.frame(sid=1:3, age=c(20,30,40))
#' b <- data.frame(sid=c(1,2,2), bp=c(120,130,140))
#' d <- data.frame(sid=c(1,3,4), wt=c(170,180,190))
#' all <- Merge(a, b, d, id = ~ sid)
#' # For data.table, first file must be the master file and must
#' # contain all ids that ever occur.  ids not in the master will
#' # not be merged from other datasets.
#' a <- data.table(a); setkey(a, sid)
#' # data.table also does not allow duplicates without allow.cartesian=TRUE
#' b <- data.table(sid=1:2, bp=c(120,130)); setkey(b, sid)
#' d <- data.table(d); setkey(d, sid)
#' all <- Merge(a, b, d)

Merge <- function(..., id, all=TRUE, verbose=TRUE) {
  
  w <- list(...)
  nams <- (as.character(sys.call())[-1])[1 : length(w)]
  m <- length(nams)
  ## If argument is a function call, e.g., subset(mydata, age > 20)
  ## find name of first argument and omit any dollar sign prefix and []
  for(i in 1 : m) {
    x <-       nams[i]
    x <-       gsub('subset\\(',   '', x)
    x <-       gsub(',.*',         '', x)
    x <-       gsub('\\[.*'  ,     '', x)
    nams[i] <- gsub('(.*)\\$(.*)', '\\2', x)
  }
  d1   <- w[[1]]
  idt  <- is.data.table(d1)
  id   <- if(idt) key(d1) else all.vars(id)
  m <- length(w)
  va <- n <- nu <- integer(m)
  nin1 <- nnin1 <- rep(NA, m)
  ## did <- if(idt) subdt(d1, , id, with=FALSE) else d1[id]
  did <- if(idt) d1[, id, with=FALSE] else d1[id]
  idc1 <- unique(as.character(interaction(did)))
  id.union <- id.intersection <- idc1
  ## Unique variables, and their labels and units
  uvar <- lab <- un <- character(0)
  for(i in 1 : m) {
    d <- w[[i]]
    nd <- names(d)
    if(any(id %nin% nd))
      stop(paste('data frame', nams[i], 'does not contain id variables',
                 paste(id, collapse=', ')))
    j <- nd %nin% uvar
    uvar <- c(uvar, nd[j])
    lab  <- c(lab,  sapply(d, label)[j])
    un   <- c(un,   sapply(d, units)[j])
    idt  <- is.data.table(d)
    M <- if(i == 1) d
    else
      if(idt) d[M]
    else
      merge(M, d, by=id, all.x=TRUE, all.y=all)
    ## did <- if(idt) subdt(d, , id, with=FALSE) else d[id]
    did   <- if(idt) d[, id, with=FALSE] else d[id]
    idc   <- unique(as.character(interaction(did)))
    di    <- dim(d)
    va[i] <- di[2]
    n [i] <- di[1]
    nu[i] <- length(unique(idc))
    if(i > 1) {
      nin1 [i] <- sum(idc %in%  idc1)
      nnin1[i] <- sum(idc %nin% idc1)
      id.union <- union(id.union, idc)
      id.intersection <- intersect(id.intersection, idc)
    }
  }
  ## Restore labels and units if needed
  nm <- names(M)
  names(lab) <- uvar
  names(un ) <- uvar
  anych <- FALSE
  if(any(c(lab, un) != ''))
    for(i in 1 : ncol(M)) {
      x <- M[[i]]
      ni <- nm[i]
      changed <- FALSE
      if(lab[ni] != '' && ! length(attr(x, 'label'))) {
        label(x) <- lab[ni]
        changed <- TRUE
      }
      if(un[ni] != '' && ! length(attr(x, 'units'))) {
        units(x) <- un[ni]
        changed <- TRUE
      }
      if(changed) M[[i]] <- x
      anych <- anych | changed
    }
  
  nams  <- c(nams, 'Merged')
  va    <- c(va, ncol(M))
  n     <- c(n, nrow(M))
  ## did   <- if(is.data.table(M)) subdt(M, , id, with=FALSE) else M[id]
  did   <- if(is.data.table(M)) M[, id, with=FALSE] else M[id]
  idc   <- unique(as.character(interaction(did)))
  nu    <- c(nu, length(unique(idc)))
  nin1  <- c(nin1,  sum(idc %in%  idc1))
  nnin1 <- c(nnin1, sum(idc %nin% idc1))
  info  <- cbind(Vars=va, Obs=n, 'Unique IDs'=nu, 'IDs in #1'=nin1,
                 'IDs not in #1'=nnin1)
  rownames(info) <- nams
  if(verbose) {
    print(info)
    cat('\nNumber of unique IDs in any data frame :', length(id.union), '\n')
    cat(  'Number of unique IDs in all data frames:', length(id.intersection),
        '\n')
    if(anych) cat('\nLabels or units restored\n')
  }
  attr(M, 'info') <- info
  M
}
