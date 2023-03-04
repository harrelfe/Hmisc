## $Id$

responseSummary <- function(formula, data, na.action=na.pass,
                            FUN=function(y) sapply(y, mean), fun,
                            overall=TRUE, continuous=10, na.rm=TRUE,
                            na.include=TRUE, g, quantile.groups=4,
                            groups=quantile.groups, nmin=0, ...) {
  func.call <- match.call()
  
  ## Print warnings for obsolete function arguments
  if(!missing(g)) {
    warning("argument g is depricated; use quantile.groups instead",
            immediate. = TRUE)
    quantile.groups <- g
  }

  if(!missing(fun)) {
    warning("argument fun is depreicated; use FUN instead", immediate. = TRUE)
    FUN <- fun
  }
  
  ## create model.frame call to create data.frame needed to use formula.
  m <- GetModelFrame(formula=formula,specials="stratify", default.na.action=na.action)
  Terms <- attr(m, "terms")

  ## Extract response and remove from model
  Y <- model.extract(m, "response")
  
  if(is.null(Y))
    stop("must have a variable on the left hand side of the formula")

  yname <- names(m)[1]
  m <- m[-1]

  ylabel <- valueLabel(Y)
  yunit <- valueUnit(Y)

  ## extract stratified variables from m or create a blank
  ## strat if non exists.
  if(!is.null(attr(Terms, 'specials')$stratify)) {
    sRequire('survival')
    temp <- survival::untangle.specials(Terms, 'stratify')

    if(length(temp$vars) == 1)
      stratified <- m[[temp$vars]]
    else {
      stratified <- stratify(m[,temp$vars])
    }

    ## Get labels and names of stratified variables
    stratified.Tags <- valueTags(stratified)

    newTerms <- drop.terms(Terms, dropx=temp$terms)
  }
  else {
    stratified <- factor(rep('',nrow(m)))
    stratified.Tags <- NULL

    newTerms <- delete.response(Terms)
  }


  ## Number of stratified terms
  nstratified <- length(levels(stratified))

  ## Create X from m using newTerms.
  X <- GetModelFrame(formula=newTerms, default.na.action=na.action)

  ## Throw warning if name overall exists in X
  if("Overall" %in% names(X) && overall)
    stop("Data Frame contains a column named 'Overall'; Name confilcts with 'overall=TRUE' argument in function")

  funlab <- NULL
  ## Check to see if fun = "%"
  if(!is.function(FUN)) {
    if (FUN == '%') {
      FUN <- function(y) {
        stats <- 100 * apply(y, 2, mean)
        names(stats) <- paste(dimnames(y)[[2]], "%")
        stats
      }

      funlab <- paste("% of", yname)
    } else 
      FUN <- match.fun(FUN)
  }

  ## Compute number of descriptive statistics per cell

  ## find vector of rows that are NA
  s <- is.na(Y)

  if(is.matrix(s))
    s <- as.vector(s %*% rep(1, ncol(s)), mode="logical")


  ## Run fun on non NA elements of Y
  if(is.matrix(Y))
    stats <- FUN(Y[!s,, drop=FALSE])
  else
    stats <- FUN(Y[!s, drop=FALSE])

  nstats <- length(stats)

  ## Create the names of the columns of summary output
  dn <- dimnames(stats)
  if(length(dn) == 2)
    name.stats <- as.vector(outer(dn[[1]], dn[[2]], FUN=function(a,b) paste(b,a)))
  else
    name.stats <- names(stats)

  if(is.null(name.stats)) {
    if(nstats == 1)
      name.stats <- yname
    else
      name.stats <- paste(yname, 1:nstats, sep="")
  }

  ## Figure out the funlab name
  if(is.null(funlab))
    funlab <- yname

  ## find number of missing observations
  numberMissing <- sum(s)

  if(numberMissing) {
    if(is.matrix(Y))
      Y <- Y[!s,, drop=FALSE]
    else
      Y <- Y[!s, drop=FALSE]

    X <- X[!s,, drop=FALSE]
    stratified <- stratified[!s]
  }


  ## Compute total number of columns
  ncolumns <- nstratified * (1 + nstats)

  colNames <- rep(c('N', name.stats), nstratified)

  ## Initialize default values
  n <- NROW(X)

  subsetX <- function(x, ...) {    
    tags <- valueTags(x)

    if(length(x) == 0) {
      return(x)
    }
      
    if(!is.matrix(x)) {
      ## Find all na's in x
      s <- is.na(x)

      ## If x is not a category make it into one
      if(! is.factor(x)) {
        ## Find the all the unique non-null values of x
        xUnique <- unique(x[!is.na(x)])

        ## If the number of unique values is less then then number deemed
        ## to be continuous treat as a factor
        if(length(xUnique) < continuous)
          x <- factor(x)
        else
          x <- cut2(x, g=quantile.groups, ...)
      }

      if(is.function(na.include) && any(s))
        x <- na.include(x)

      if(nmin > 0) {
        nn <- table(x)
        levels(x) <- ifelse(nn >= nmin, names(nn), NA)
      }
    }
    else {
      cnames <- colnames(x)
      if(!length(cnames))
        stop("matrix variable must have column dimnames")
      
      if(! is.logical(x)){
        ## Coerce x to logical
        if(is.numeric(x))
          x <- x==1
        else {
          x <- structure(casefold(x), dim=dim(x))
          x <- x=='present' | x=='yes' | x=='true'
        }
      }

      colnames(x) <- cnames
      attr(x, "levels") <- cnames

      ## see if there are any stragulars
      if(nmin > 0) {
        nn <- apply(x, 2, sum, na.rm=TRUE)
        x <- x[,nn >= nmin]
      }

      ## Convert the true falses to column name or NA
      x <- ifelse(x, rep(cnames, each=n), NA)
    }

    valueTags(x) <- tags
    return(x)
  }

  ## Subset X
  X <- lapply(X, FUN=subsetX, ...)
  
##  if(is.matrix(Y)) {
##    Y <- split(Y, row(Y))
##    
##     procY <- function(y) do.call(rbind, y)
##   } else {
##     procY <- function(y) y
##   }
  

  comp.stats <- function(grouped.y) {
    ans <- c(length(grouped.y), FUN(grouped.y))
    names(ans) <- c('N', name.stats)
    ans
  }

  ## create stats for each element of X
  processX <- function(x) {
    if(is.mChoice(x)) {
    } else {
      xstats <- tapply(Y, list(x, stratified), FUN=comp.stats)
    }
    
    valueTags(xstats) <- valueTags(x)
    xstats
  }

  Xstats <- lapply(X, FUN=processX)

  ## if overall selected add Overall column
  if(overall) {
    overall <- tapply(Y, stratified, FUN=comp.stats)
    overall <- matrix(overall, ncol=dim(overall), dimnames=list(NULL, dimnames(overall)[[1]]))
    Xstats$Overall <- overall
  }
  
  
#  str(Xstats)
  newAttr <- list(terms=Terms, call=match.call(), n=n, nmissing=numberMissing, yname=yname,
                  ylabel=ylabel, ycolnames=colnames(Y), funlab=funlab,
                  stratified.Tags=stratified.Tags, stratified.levels=levels(stratified))
  attributes(Xstats) <- c(attributes(Xstats), newAttr)
  class(Xstats)<- 'responseSummary'
  return(Xstats)
}

print.responseSummary <- function(x,
                                  valueNames = c('labels','names'),
                                  vnames, printUnits = TRUE, prUnits,
                                  abbreviate.dimnames=FALSE,
                                  prefix.width, min.colwidth, formatArgs=NULL,
                                  ...) {

  if(missing(valueNames) && !missing(vnames)){
    warning("argument vnames is depricated; use valueNames instead",
            immediate. = TRUE)
    valueNames <- vnames
  }
  
  if(missing(printUnits) && !missing(prUnits)){
    warning("argument prUnits is depricated; use printUnits instead",
            immediate. = TRUE)
    printUnits <- prUnits
  }

  x.orig <- x
  
  ## fuzy match value of varNames to default options
  valueNames <- match.arg(valueNames)

  ## Get attributes of x for further use
  xattribs <- attributes(x)
  attributes(x) <- NULL
  
  ## Set useLabels flag to TRUE if user wants to use labels
  ## instead of names
  useLabel <- valueNames == 'labels'

  if(useLabel && !is.null(xattribs$ylabel)) {
    yname <- xattribs$ylabel
  } else {
    yname <- xattribs$yname
  }
  cat(yname)
  
  ## If more then one stratifed levels make by line
  if(length(xattribs$stratified.levels) > 1) {
    if(useLabel && !is.null(xattribs$stratified.Tags$label)) {
      strat.name <- xattribs$stratified.Tags$label
    } else {
      strat.name <- xattribs$stratifed.Tags$label
    }

    cat(' by', strat.name)
  }

  cat('    N=', xattribs$n, sep='')
  
  if(xattribs$nmissing) {
    cat(' ,', xattribs$nmissing, 'Missing')
  }

  cat('\n\n')

  if(useLabel) {
    labels <- unlist(lapply(x, function(x) if(is.null(lab <- valueLabel(x))) NA else lab))
    
    names(x) <- ifelse(is.na(labels), xattribs$names, labels)
  }

  print.char.list(x, abbreviate.dimnames=abbreviate.dimnames, print.it=TRUE, ...)
  invisible(x.orig)
}


latex.responseSummary <- function(object,
                                  title=first.word(deparse(substitute(object))),
                                  caption,
                                  trios,
                                  vnames=c('labels', 'names'),
                                  prn=TRUE,
                                  prUnits=TRUE,
                                  rowlabel='',
                                  cdec=2,
                                  ncaption=TRUE,
                                  ...) {
  ## Fix lazy evaluation
  title <- title
  
  
}
