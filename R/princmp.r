##' Enhanced Output for Principal and Sparse Principal Components
##'
##' Expands any categorical predictors into indicator variables, and calls `princomp` (if `method='regular'` (the default)) or `sPCAgrid` in the `pcaPP` package (`method='sparse'`) to compute lasso-penalized sparse principal components.  By default all variables are first scaled by their standard deviation after observations with any `NA`s on any variables in `formula` are removed.  Loadings of standardized variables, and if `orig=TRUE` loadings on the original data scale are printed.  If `pl=TRUE` a scree plot is drawn with text added to indicate cumulative proportions of variance explained.  If `sw=TRUE`, the `leaps` package `regsubsets` function is used to approximate the PCs using forward stepwise regression with the original variables as individual predictors.
##' @title princmp
##' @param formula a formula with no left hand side, or a numeric matrix
##' @param data a data frame or table.  By default variables come from the calling environment.
##' @param method specifies whether to use regular or sparse principal components are computed
##' @param k the number of components to plot, display, and return
##' @param kapprox the number of components to approximate with stepwise regression when `sw=TRUE`
##' @param cor set to `FALSE` to compute PCs on the original data scale, which is useful if all variables have the same units of measurement
##' @param offset controls positioning of text labels for cumulative fraction of variance explained
##' @param col color of plotted text
##' @param adj angle for plotting text
##' @param scoef set to `FALSE` to not print coefficients (loadings) of standardized variables
##' @param orig set to `FALSE` to not show coefficients on the original scale
##' @param pl set to `FALSE` to not make the scree plot
##' @param ylim y-axis plotting limits, a 2-vector
##' @param add set to `TRUE` to add to an existing plot
##' @param sw set to `TRUE` to run stepwise regression PC prediction/approximation
##' @param nvmax maximum number of predictors to allow in stepwise regression PC approximations
##' @return a k-column matrix with principal component scores, with `NA`s when the input data had an `NA`.  If `k=1` the result is a vector.
##' @author Frank Harrell
##' @export
##' @md
princmp <- function(formula, data=environment(formula),
                    method=c('regular', 'sparse'),
                    k=min(5, p), kapprox=min(5, k),
                    cor=TRUE, offset=0.8, col=1,
                    adj=0, scoef=TRUE, orig=TRUE, pl=TRUE, ylim=NULL,
                    add=FALSE, sw=FALSE, nvmax=5) {
  method <- match.arg(method)
  if(method == 'sparse')
    if(! requireNamespace('pcaPP', quietly=TRUE))
      stop('You must install the pcaPP package to use method="sparse"')

  isf <- inherits(formula, 'formula')
  X <- if(isf) model.matrix.lm(formula, data) else as.matrix(formula)
  if(! missing(data) && nrow(X) != nrow(data))
    cat('Used', nrow(X), 'observations with no NAs out of', nrow(data), '\n')
  if(isf) X  <- X[, -1, drop=FALSE]  # remove intercept
  p  <- ncol(X)
  g  <- switch(method,
               regular = stats::princomp(X, cor=cor),
               sparse  = pcaPP::sPCAgrid(X, k=k, method='sd',
                                  center=mean,
                                  scale=if(cor) sd else function(x) 1.,
                                  scores=TRUE, maxiter=10) )
  
  co <- unclass(g$loadings)
  prz <- function(x, m) {
    x <- format(round(x, m), zero.print=FALSE)
    print(x, quote=FALSE)
  }
  type <- switch(method, regular='', sparse='Sparse ')

  if(scoef) {
    cat('\n', type, 'PC Coefficients of Standardized Variables\n', sep='')
    prz(co[, 1 : k], 3)
    }
  p <- ncol(co)
  if(orig) {
    sds <- g$scale
    cat('\n', type, 'PC Coefficients of Original Variables\n', sep='')
    co <- co / matrix(rep(sds, p), nrow=length(sds))
    prz(co[, 1 : k], 5)
    }

  vars <- g$sdev^2

  if(pl) {
    cumv <- cumsum(vars) / sum(vars)
    if(add) lines(1:k, vars[1:k], type='b', col=col)
    else
      plot(1:k, vars[1:k], type='b', ylab='Variance', xlab='',
           axes=FALSE, col=col, ylim=ylim)
    axis(1, at=1:k,
         labels = as.expression(sapply(1 : k, function(x) bquote(PC[.(x)]))))
    axis(2)
    text(1:k, vars[1:k] + offset * par('cxy')[2],
         as.character(round(cumv[1:k], 2)),
         srt=45, adj=adj, cex=.65, xpd=NA, col=col)
  }

  if(sw) {
    if(! requireNamespace('leaps', quietly=TRUE))
      stop('You must install the leaps package when sw=TRUE')
    for(j in 1 : kapprox) {
      cat('\nStepwise Approximations to PC', j, '\n', sep='')
      fchar <- capture.output(
        f <- leaps::regsubsets(X, g$scores[, j], method='forward',
                        nbest=1, nvmax=min(p - 1, nvmax)))
      s <- summary(f)
      w <- s$which[, -1, drop=FALSE]   # omit intercept
      print(t(ifelse(w, '*', ' ')), quote=FALSE)
      cat('R2:', round(s$rsq, 3), '\n')
    }
    }
    
  ## Use predict method with newdata to ensure that PCs NA when
  ## original variables NA
  ## See https://stackoverflow.com/questions/5616210
  if(isf) X  <- model.matrix.lm(formula, data, na.action=na.pass)
  pcs <- predict(g, newdata=X)[, 1 : k]
  attr(pcs, 'vars') <- vars
  invisible(pcs)
}
