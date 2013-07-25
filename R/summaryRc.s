summaryRc <-
  function(formula, data=NULL, subset=NULL, na.action=NULL, 
           fun=function(x) x, na.rm=TRUE,
           ylab=NULL, ylim = NULL, nloc=NULL, datadensity=NULL,
           quant = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
           trim=NULL, test=FALSE, vnames='labels',
           ...)
{
  call <- match.call()
  X <- match.call(expand.dots=FALSE)
  X$fun <- X$na.rm <- X$ylim <- X$ylab <- X$nloc <- X$datadensity <- X$quant <-
    X$trim <- X$test <- X$vnames <- X$... <- NULL
  if(missing(na.action))
    X$na.action <- na.retain
  
  Terms <- if(missing(data)) terms(formula, 'stratify')
  else terms(formula, 'stratify', data=data)
  
  X$formula <- Terms
  X[[1]] <- as.name("model.frame")
  
  X <- eval(X, sys.parent())
  
  Terms <- attr(X, "terms")
  resp <- attr(Terms, "response")
  
  nact <- attr(X, "na.action")
  nvar <- ncol(X) - 1
  strat <- attr(Terms, 'specials')$stratify

  getlab <- function(x, default) {
    if(vnames == 'name') return(default)
    lab <- attr(x, 'label')
    if(!length(lab) || lab=='') default else lab
  }

  if(length(strat)) {
    temp <- untangle.specials(Terms,'stratify')
    strat.name <- var.inner(Terms)[temp$terms]
    strat <- if(length(temp$vars) == 1) as.factor(X[[temp$vars]])
    else stratify(X[,temp$vars])
    
    strat.label <- getlab(X[, temp$vars[1]], strat.name)
    X[[temp$vars]] <- NULL   # remove strata factors
  }
  Y <- X[[resp]]
  yname <- as.character(attr(Terms,'variables'))[2]
  ylabel <- if(length(ylab)) ylab else getlab(Y, yname)

  X[[resp]] <- NULL   # remove response var
  s <- is.na(Y)
  nmissy <- sum(s)
  if(nmissy) {
    X <- X[!s,, drop=FALSE]
    Y <- Y[!s,, drop=FALSE]
    strat <- strat[!s]
  }

  pl <- function(x, y, strat=NULL, quant, xlab='', ylab='',
                 ylim=NULL, fun=function(x) x, ...) {
    n   <- sum(!is.na(x))
    group <- if(length(strat)) strat else rep(1, length(x))
    if(!length(trim)) trim <- if(n > 200) 10 / n else 0
    xlim <- if(trim == 0) range(x, na.rm=TRUE)
    else quantile(x, c(trim, 1 - trim), na.rm=TRUE)
    a <- list(x=x, y=y, xlab=xlab, ylab=ylab, xlim=xlim, trim=0, group=group,
              datadensity=if(length(datadensity)) datadensity
              else length(strat) > 0, ...)
    if(length(fun))  a$fun <- fun
    if(length(ylim)) a$ylim <- ylim
    z <- do.call('plsmo', a)
    usr  <- par('usr')
    xl <- usr[1:2]
    yl <- usr[3:4]
    if(! (length(nloc) && is.logical(nloc) && !nloc)) {
      if(length(nloc)) {
        xx <- nloc[[1]]
        yy <- nloc[[2]]
        xx <- xl[1] + xx * diff(xl)
        yy <- yl[1] + yy * diff(yl)
        w <- list(x=xx, y=yy)
      }
      else {
        xs <- unlist(lapply(z, function(x)x$x))
        ys <- unlist(lapply(z, function(x)x$y))
        w <- largest.empty(xs, ys, method='area')
      }
      text(w, paste('n=', n, sep=''), cex=.75, font=3, adj=.5)
    }
    scat1d(x)
    h    <- diff(yl) * .02
    qu <- quantile(x, quant, na.rm=TRUE)
    arrows(qu, yl[2] + h, qu, yl[2], col='blue', length=.05, xpd=NA)
    ## text(xl[2], yl[2] + h/4, paste('n=', n, sep=''),
    ##      cex=.75, font=3, adj=c(1,0), xpd=NA)
  }

  i <- 0
  nams <- names(X)
  for(v in nams) {
    i <- i + 1
    x <- X[[v]]
    xlab <- getlab(x, nams[i])
    units  <- if(length(l <- attr(x,'units'))) l else ''
    xlab <- labelPlotmath(xlab, units)
    pl(x, Y, strat=strat, quant=quant, xlab=xlab, ylab=ylabel, ylim=ylim, ...)
  }
}
