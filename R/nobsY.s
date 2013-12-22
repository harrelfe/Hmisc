nobsY <- function(formula, data=NULL, subset=NULL, na.action=na.retain) {
  marg <- length(data) && '.marginal.' %in% names(data)
  if(marg) formula <- update(formula, .~. + .marginal.)
  formula <- Formula(formula)
  mf <- if(length(subset))
    model.frame(formula, data=data, subset=subset, na.action=na.action)
  else
    model.frame(formula, data=data, na.action=na.action)
  Y <- model.part(formula, data=mf, lhs=1)
  if(marg) {
    X <- model.part(formula, data=mf, rhs=1)
    Y <- Y[! X$.marginal.,, drop=FALSE]
  }
  nobs <- 0
  for(i in 1:ncol(Y)) {
    y <- Y[[i]]
    nobs <- max(nobs,
                if(is.matrix(y)) colSums(! is.na(y)) else sum(! is.na(y)))
  }
  nobs
}

addMarginal <- function(data, ..., label='All') {
  vars <- as.character(sys.call())[- (1 : 2)]
  vars <- intersect(vars, names(data))
  data$.marginal. <- FALSE
  for(v in vars) {
    d <- data
    d$.marginal. <- TRUE
    d[[v]] <- label
    data <- rbind(data, d)
  }
  data
}
