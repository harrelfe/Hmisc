##' Enhanced Output for Principal and Sparse Principal Components
##'
##' Expands any categorical predictors into indicator variables, and calls `princomp` (if `method='regular'` (the default)) or `sPCAgrid` in the `pcaPP` package (`method='sparse'`) to compute lasso-penalized sparse principal components.  By default all variables are first scaled by their standard deviation after observations with any `NA`s on any variables in `formula` are removed.  Loadings of standardized variables, and if `orig=TRUE` loadings on the original data scale are printed.  If `pl=TRUE` a scree plot is drawn with text added to indicate cumulative proportions of variance explained.  If `sw=TRUE`, the `leaps` package `regsubsets` function is used to approximate the PCs using forward stepwise regression with the original variables as individual predictors.
##'
##' A `print` method prints the results and a `plot` method plots the scree plot of variance explained.
##' @title princmp
##' @param formula a formula with no left hand side, or a numeric matrix
##' @param data a data frame or table.  By default variables come from the calling environment.
##' @param method specifies whether to use regular or sparse principal components are computed
##' @param k the number of components to plot, display, and return
##' @param kapprox the number of components to approximate with stepwise regression when `sw=TRUE`
##' @param cor set to `FALSE` to compute PCs on the original data scale, which is useful if all variables have the same units of measurement
##' @param sw set to `TRUE` to run stepwise regression PC prediction/approximation
##' @param nvmax maximum number of predictors to allow in stepwise regression PC approximations
##' @return a list of class `princmp` with elements `scores`, a k-column matrix with principal component scores, with `NA`s when the input data had an `NA`, and other components useful for printing and plotting.  If `k=1` `scores` is a vector.  Other components include `vars` (vector of variances explained), `method`, `k`.
##' @author Frank Harrell
##' @export
##' @md
princmp <- function(formula, data=environment(formula),
                    method=c('regular', 'sparse'),
                    k=min(5, p - 1), kapprox=min(5, k),
                    cor=TRUE, sw=FALSE, nvmax=5) {
  method <- match.arg(method)
  if(method == 'sparse')
    if(! requireNamespace('pcaPP', quietly=TRUE))
      stop('You must install the pcaPP package to use method="sparse"')

  isf <- inherits(formula, 'formula')
  X <- if(isf) model.matrix.lm(formula, data) else as.matrix(formula)
  
  if(isf) X  <- X[, -1, drop=FALSE]  # remove intercept
  p  <- ncol(X)
  res <- list(n=nrow(X), ndata=if(! missing(data)) nrow(data),
              method=method, k=k, kapprox=kapprox, cor=cor, nvmax=nvmax)
  g  <- switch(method,
               regular = stats::princomp(X, cor=cor),
               sparse  = pcaPP::sPCAgrid(X, k=ncol(X) - 1, method='sd',
                                  center=mean,
                                  scale=if(cor) sd else function(x) 1.,
                                  scores=TRUE, maxiter=10) )
  
  co        <- unclass(g$loadings)
  type      <- switch(method, regular='', sparse='Sparse ')
  res$scoef <- co
  res$scale <- g$scale
  p         <- ncol(co)
  sds       <- g$scale
  co        <- co / matrix(rep(sds, p), nrow=length(sds))
  res$coef  <- co
  vars      <- g$sdev^2
  res$vars  <- vars

  if(sw) {
    if(! requireNamespace('leaps', quietly=TRUE))
      stop('You must install the leaps package when sw=TRUE')
    swa <- list()

    for(j in 1 : kapprox) {
      fchar <- capture.output(  # don't allow regular output
        f <- leaps::regsubsets(X, g$scores[, j], method='forward',
                               nbest=1, nvmax=min(p - 1, nvmax)))
      debug <- getOption('princmpdebug', FALSE)
      if(debug) cat('Debugging turned on\n')
      if(j == 1 && debug)
        saveRDS(list(X=X, g=g, f=f), file='/tmp/princmp.rds')
      s <- summary(f)
      w <- s$which[, -1, drop=FALSE]   # omit intercept
      xnm <- colnames(w)
      xadded <- character(0)
      for(l in 1 : nrow(w)) {
        varnow <- xnm[w[l,]]
        varnew <- setdiff(varnow, xadded)
        xadded <- c(xadded, varnew)
        }
      rsq <- structure(s$rsq, names=xadded)
      l <- which(rsq >= 0.999)
      if(length(l)) rsq <- rsq[1 : min(l)]
      swa[[j]] <- rsq
    }
    res$sw <- swa
    }
    
  ## Use predict method with newdata to ensure that PCs NA when
  ## original variables NA
  ## See https://stackoverflow.com/questions/5616210
  if(isf) X  <- model.matrix.lm(formula, data, na.action=na.pass)
  pcs        <- predict(g, newdata=X)
  pcs        <- pcs[, 1 : min(k, ncol(pcs)), drop=FALSE]
  res$scores <- pcs
  class(res) <- 'princmp'
  res
}

##' Print Results of princmp
##'
##' Simple print method for [princmp()]
##' @title print.princmp
##' @param x results of `princmp`
##' @param which specifies which loadings to print, the default being `'none'` and other values being `'standardized'`, `'original'`, or `'both'`
##' @param k number of components to show, defaults to `k` specified to `princmp`
##' @param ... unused
##' @return nothing
##' @author Frank Harrell
##' @export
##' @md
print.princmp <- function(x,
                          which=c('none', 'standardized', 'original', 'both'),
                          k=x$k, ...) {
  which <- match.arg(which)
  prz <- function(x, m) {
    x <- format(round(x, m), zero.print=FALSE)
    print(x, quote=FALSE)
  }
  method  <- x$method
  type <- switch(method,
                 regular = '',
                 sparse  = 'Sparse ')
  cat(type, 'Principal Components Analysis\n', sep='')
  if(length(x$ndata) && x$ndata != x$n)
    cat('Used', x$n, 'observations with no NAs out of', x$ndata, '\n')

  co <- x$scoef
  if(which %in% c('standardized', 'both')) {
    cat('\n', type, 'PC Coefficients of Standardized Variables\n', sep='')
    prz(co[, 1 : k], 3)
  }
  p <- ncol(x$coef)
  if(which %in% c('original', 'both')) {
    sds <- x$scale
    cat('\n', type, 'PC Coefficients of Original Variables\n', sep='')
    co <- co / matrix(rep(sds, p), nrow=length(sds))
    prz(co[, 1 : k], 5)
  }
  sw <- x$sw
  lsw <- length(sw)
  if(lsw) {
    cat('\nStepwise Approximations to PCs With Cumulative R^2\n')
    for(j in 1 : lsw) {
      cat('\nPC', j, '\n')
      rsq <- sw[[j]]
      fw <- character(0)
      xadded <- names(rsq)
      for(l in 1 : length(rsq))
        fw <- paste0(fw, if(l > 1) ' + ', xadded[l],
                     ' (', round(rsq[l], 3), ')')
      cat(strwrap(fw), sep='\n')
    }
  }
  invisible()
}

##' Plot Method for princmp
##'
##' Uses base graphics to by default plot the scree plot from a [princmp()] result, showing cumultive proportion of variance explained.  Alternatively the standardized PC loadings are shown in a `ggplot2` bar chart.
##' @title plot.princmp
##' @param x results of `princmp`
##' @param which '`scree`' or '`loadings'`
##' @param k number of components to show, default is `k` specified to `princmp`
##' @param offset controls positioning of text labels for cumulative fraction of variance explained
##' @param col color of plotted text in scree plot
##' @param adj angle for plotting text in scree plot
##' @param ylim y-axis scree plotting limits, a 2-vector
##' @param add set to `TRUE` to add a line to an existing scree plot without drawing axes
##' @param abbrev an integer specifying the variable name length above which names are passed through [abbreviate(..., minlength=abbrev)]
##' @param nrow number of rows to use in plotting loadings.  Defaults to the `ggplot2` `facet_wrap` default.
##' @param ... unused
##' @return `ggplot2` object if `which='loadings'`
##' @export
##' @author Frank Harrell
plot.princmp <- function(x, which=c('scree', 'loadings'),
                         k=x$k, offset=0.8, col=1, adj=0,
                         ylim=NULL, add=FALSE, abbrev=25, nrow=NULL,
                         ...) {
  which <- match.arg(which)
  if(which == 'scree') {
  vars <- x$vars

  cumv <- cumsum(vars) / sum(vars)
  if(add) lines(1:k, vars[1:k], type='b', col=col)
  else {
    plot(1:k, vars[1:k], type='b', ylab='Variance',
         xlab=if(k <= 10) '' else 'Component',
         axes=FALSE, col=col, ylim=ylim)
    axis(1, at=1:k,
         labels = if(k > 10) 1 : k else
                  as.expression(sapply(1 : k, function(x) bquote(PC[.(x)]))))
    axis(2)
    }
  text(1:k, vars[1:k] + offset * par('cxy')[2],
       as.character(round(cumv[1:k], 2)),
       srt=45, adj=adj, cex=.65, xpd=NA, col=col)
  return(invisible())
  }

  co    <- x$scoef[, 1 : k, drop=FALSE]
  
  xname <- abbreviate(rownames(co), minlength=abbrev)
  p     <- length(xname)
  b     <- as.vector(co)
  r     <- range(abs(b))
  sub   <- paste0('Range of |loading|:', round(r[1], 3),
                  ' - ', round(r[2], 3))
  d <- data.frame(b    = abs(b),
                  y    = as.vector(p + 1 - row(co)),
                  comp = factor(as.vector(col(co)),
                                levels=1 : k,
                                labels=paste0('PC[', 1 : k, ']')),
                  x    = 1,
                  sign = ifelse(b >= 0., '+', '-'))
  g <- ggplot(d,  aes(x=.data$x, y=.data$y, color=.data$sign)) +
    geom_segment(aes(x=.data$x, y=.data$y, xend=.data$x + 0.9 * .data$b / r[2], yend=.data$y),
                 size=3) +
    facet_wrap(~ .data$comp, labeller='label_parsed', nrow=nrow) +
    scale_x_continuous(breaks = 1 : k,
                       labels=as.expression(sapply(1 : k,
                                 function(x) bquote(PC[.(x)])))) +
    scale_y_continuous(breaks = 1 : p, labels=rev(xname)) +
    guides(x='none', color = guide_legend(title='')) +
    xlab('Standardized Loading (Absolute)') + ylab('') + labs(caption=sub) +
    theme(legend.position='bottom')
  g
}
