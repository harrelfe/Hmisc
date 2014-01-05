nobsY <- function(formula, data=NULL, subset=NULL, na.action=na.retain) {
  forig <- formula
  formula <- Formula(formula)
  environment(formula) <- new.env(parent = environment(formula))
  en <- environment(formula)
  assign(envir = en, 'id', function(x) x)
  marg <- length(data) && '.marginal.' %in% names(data)
  if(marg) formula <- update(formula, .~. + .marginal.)
  mf <- if(length(subset))
    model.frame(formula, data=data, subset=subset, na.action=na.action)
  else
    model.frame(formula, data=data, na.action=na.action)
  
  Y <- model.part(formula, data=mf, lhs=1)
  X <- model.part(formula, data=mf, rhs=1)
  ## Get id variable if present so can count unique subjects
  rhs <- terms(formula, rhs=1, specials='id')
  sr  <- attr(rhs, 'specials')
  ## specials counts from lhs variables
  wid <- sr$id
  if(length(wid)) {
    xid <- X[[wid - ncol(Y)]]
    ## Remove id() from formula
    forig <- sub(' \\+ id(.*)', '', as.character(forig))
    forig <- as.formula(paste(forig[2], forig[3], sep=' ~ '))
  }
  else xid <- 1 : nrow(Y)
  xid  <- if(! length(wid)) 1 : nrow(Y) else X[[wid - ncol(Y)]]
  if(marg) {
    Y   <- Y  [! X$.marginal.,, drop=FALSE]
    xid <- xid[! X$.marginal.]
  }
  nobs <- 0
  for(i in 1:ncol(Y)) {
    y <- Y[[i]]
    ## is.na.Surv reduces to vector but need to keep as matrix
    notna <- if(is.matrix(y)) rowSums(is.na(unclass(y))) == 0 else ! is.na(y)
    nobs <- max(nobs, length(unique(xid[notna])))
  }
  structure(nobs, formula=forig)
}

addMarginal <- function(data, ..., label='All') {
  vars <- as.character(sys.call())[- (1 : 2)]
  vars <- intersect(vars, names(data))
  data$.marginal. <- FALSE

  labs <- sapply(data, function(x) {
    la <- attr(x, 'label')
    if(! length(la)) la <- ''
    la })
  un <- sapply(data, function(x) {
    u <- attr(x, 'units')
    if(! length(u)) u <- ''
    u })

  for(v in vars) {
    d <- data
    d$.marginal. <- TRUE
    d[[v]] <- label
    data <- rbind(data, d)
  }
  ## Restore any Hmisc attributes
  if(any(labs != '') || any(un != ''))
    for(i in 1 : length(data)) {
      if(labs[i] != '') {
        attr(data[[i]], 'label') <- labs[i]
        class(data[[i]]) <- c('labelled', class(data[[i]]))
      }
      if(un[i] != '') attr(data[[i]], 'units') <- un[i]
    }
  data
}
