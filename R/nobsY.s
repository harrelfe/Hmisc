nobsY <- function(formula, group=NULL,
                  data=NULL, subset=NULL, na.action=na.retain,
                  matrixna=c('all', 'any')) {
  matrixna <- match.arg(matrixna)
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
    forig <- as.character(forig)
    if(ncol(X) == 1)  ## id() is the only right-hand term
      forig <- as.formula(paste(forig[2], ' ~ 1'))
    else {
      forig[3] <- sub(' \\+ id(.*)', '', forig[3])
      forig <- as.formula(paste(forig[2], forig[3], sep=' ~ '))
    }
  }
  xid  <- if(! length(wid)) 1 : nrow(Y) else X[[wid - ncol(Y)]]
  group <- if(length(group) && group %in% names(X)) X[[group]]
  if(marg) {
    xm <- X$.marginal.
    if(length(group)) group <- group[! xm]
    Y   <- Y  [! xm,, drop=FALSE]
    xid <- xid[! xm]
  }
  nY   <- ncol(Y)
  nobs <- rep(NA, nY)
  ylab <- sapply(Y, label)
  ylab <- ifelse(ylab == '', names(Y), ylab)
  names(nobs) <- ylab
  nobsg <- if(length(group)) {
    glev <- if(is.factor(group)) levels(group)
     else sort(unique(group[! is.na(group)]))
    matrix(NA, ncol=nY, nrow=length(glev), dimnames=list(glev, ylab))
  }

  for(i in 1 : nY) {
    y <- Y[[i]]
    ## is.na.Surv reduces to vector but need to keep as matrix
    notna <- if(is.matrix(y)) {
      numna <- rowSums(is.na(unclass(y)))
      if(matrixna == 'any') numna == 0 else numna < ncol(y)
    } else ! is.na(y)
    nobs[i] <- length(unique(xid[notna]))
    if(length(group))
      nobsg[, i] <- tapply(xid[notna], group[notna],
                           function(x) length(unique(x)))
  }
  structure(list(nobs=nobs, nobsg=nobsg, formula=forig))
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
