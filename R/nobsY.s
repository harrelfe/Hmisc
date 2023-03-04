nobsY <- function(formula, group=NULL,
                  data=NULL, subset=NULL, na.action=na.retain,
                  matrixna=c('all', 'any')) {
  matrixna <- match.arg(matrixna)
  forig <- formula
  formula <- Formula::Formula(formula)
  environment(formula) <- new.env(parent = environment(formula))
  en <- environment(formula)
  assign(envir = en, 'id', function(x) x)
  assign(envir = en, 'pending', function(x) x)
  assign(envir = en, 'randomized', function(x) x)
  assign(envir = en, 'cond',
         function(x, label, condition) rep(1, length(condition)))
  marg <- length(data) && '.marginal.' %in% names(data)
  if(marg) formula <- update(formula, .~. + .marginal.)
  mf <- if(length(subset))
    model.frame(formula, data=data, subset=subset, na.action=na.action)
  else
    model.frame(formula, data=data, na.action=na.action)
  
  Y <- Formula::model.part(formula, data=mf, lhs=1)
  X <- Formula::model.part(formula, data=mf, rhs=1)
  ## Get id variable if present so can count unique subjects
  rhs <- terms(formula, rhs=1, specials='id')
  sr  <- attr(rhs, 'specials')
  ## specials counts from lhs variables
  wid <- sr$id
  if(length(wid)) {
    xid <- X[[wid - ncol(Y)]]
    if(length(wid) > 1) {
      xid$sep <- '.'
      xid <- do.call('paste', xid)
    }
    ## Remove id() from formula
    forig <- as.character(forig)
    if(ncol(X) == 1)  ## id() is the only right-hand term
      forig <- as.formula(paste(forig[2], ' ~ 1'))
    else {
      forig[3] <- sub(' \\+ id(.*)', '', forig[3])
      forig <- as.formula(paste(forig[2], forig[3], sep=' ~ '))
    }
  } else xid <- 1 : nrow(Y)
  idv <- xid

  group <- if(length(group) && group %in% names(X)) X[[group]]
  if(marg) {
    xm <- X$.marginal.
    if(length(group)) group <- group[xm == '']
    Y   <- Y  [xm == '',, drop=FALSE]
    xid <- xid[xm == '']
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

  if(nY > 0) for(i in 1 : nY) {
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
  structure(list(nobs=nobs, nobsg=nobsg, id=idv, formula=forig))
}

addMarginal <- function(data, ..., label='All',
                        margloc=c('last', 'first'), nested) {
  nested <- as.character(substitute(nested))
  if(length(nested) && nested == '') nested <- NULL
  vars <- as.character(sys.call())[- (1 : 2)]
  vars <- intersect(vars, names(data))
  data$.marginal. <- ''
  margloc <- match.arg(margloc)
  if(length(nested) && (nested %nin% names(data)))
    stop(paste('Variable', nested, 'is not in data'))

  labs <- sapply(data, function(x) {
    la <- attr(x, 'label')
    if(! length(la)) la <- ''
    la
  } )
  
  un <- sapply(data, function(x) {
    u <- attr(x, 'units')
    if(! length(u)) u <- ''
    u
  } )

  levs <- vector('list', length(vars))
  names(levs) <- vars
  for(v in setdiff(vars, nested)) {
    d <- data
    d$.marginal. <- ifelse(d$.marginal. == '', v,
                           paste(d$.marginal., v, sep=','))
    levs[[v]] <- levels(as.factor(d[[v]]))
    levs[[v]] <- if(margloc == 'last') c(levs[[v]], label)
                 else                  c(label,     levs[[v]])
    d[[v]] <- label
    if(length(nested)) {
      levs[[nested]] <- levels(as.factor(d[[nested]]))
      levs[[nested]] <- if(margloc == 'last') c(levs[[nested]], label)
                        else                  c(label,          levs[[nested]])
      d[[nested]] <- label
      }
    data <- if(margloc == 'last') rbind(data, d) else rbind(d, data)
  }
  for(v in vars) data[[v]] <- factor(data[[v]], levs[[v]])
  
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
