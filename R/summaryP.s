summaryP <- function(formula, data=NULL,
                     subset=NULL, na.action=na.retain,
                     exclude1=TRUE, sort=TRUE,
                     asna=c('unknown', 'unspecified'), ...) {
  
  nobs <- nobsY(formula, data=data, subset=subset, na.action=na.action)
  formula <- Formula(formula)

  Y <- if(length(subset))
    model.frame(formula, data=data, subset=subset, na.action=na.action)
  else
    model.frame(formula, data=data, na.action=na.action)
  X <- model.part(formula, data=Y, rhs=1)
  Y <- model.part(formula, data=Y, lhs=1)

  nY <- NCOL(Y)
  nX <- NCOL(X)
  namY <- names(Y)
  if(nX == 0) X <- data.frame(x=rep(1, NROW(Y)))
  ux <- unique(X)
  Z <- NULL
  n <- nrow(X)

  if(sort) {
    ## Compute marginal frequencies of all regular variables so can sort
    mfreq <- list()
    for(ny in namY) {
      y <- Y[[ny]]
      if(!inherits(y, 'ynbind')) {
        if(length(asna) && (is.factor(y) || is.character(y)))
          y[y %in% asna] <- NA
        freq <- table(y)
        counts        <- as.numeric(freq)
        names(counts) <- names(freq)
        mfreq[[ny]]   <- - sort(- counts)
      }
    }
  }
  for(i in 1 : nrow(ux)) {
    j <- rep(TRUE, n)
    if(nX > 0) for(k in 1 : nX) j <- j & (X[[k]] == ux[i, k])
    ## yx <- Y[j,, drop=FALSE]
    for(k in 1 : nY) {
      ## y <- yx[[k]] doesn't work as attributes lost by [.data.frame
      y <- Y[[k]]
      y <- if(is.matrix(y)) y[j,, drop=FALSE] else y[j]
#      y <- (Y[[k]])[j,, drop=FALSE]
      if(inherits(y, 'ynbind')) {
        overlab <- attr(y, 'label')
        labs <- attr(y, 'labels')
        z <- NULL
        for(iy in 1 : ncol(y)) {
          tab <- table(y[, iy])
          no <- as.numeric(sum(tab))
          z <- rbind(z,
                     data.frame(var=overlab, val=labs[iy],
                                freq=as.numeric(tab['TRUE']),
                                denom=no))
        }
      }
      else {  # regular single column
        if(length(asna) && (is.factor(y) || is.character(y)))
          y[y %in% asna] <- NA
        tab <- table(y)
        ny <- namY[k]
        la  <- label(y)
        if(la == '') la <- ny
        lev <- names(tab)
        mf <- mfreq[[ny]]
        no <- as.numeric(sum(tab))
        if(exclude1 && length(mf) == 2) {
          lowest <- names(which.min(mf))
          z <- data.frame(var=la, val=lowest,
                          freq=as.numeric(tab[lowest]),
                          denom=no)
        }
        else {
          if(sort) lev <- reorder(lev, (mfreq[[ny]])[lev])
          z <- data.frame(var=la, val=lev,
                          freq=as.numeric(tab),
                          denom=no)
        }
      }
      ## Add current X subset settings
      if(nX > 0) for(k in 1: nX) z[[names(ux)[k]]] <- ux[i, k]
      Z <- rbind(Z, z)
    }
  }
  structure(Z, class=c('summaryP', 'data.frame'), formula=formula,
            nX=nX, nY=nY, nobs=nobs)
}

plot.summaryP <-
  function(x, formula=NULL, groups=NULL, xlim=c(0, 1), 
           cex.values=0.5, xwidth=.125, ydelta=0.04,
           key=list(columns=length(groupslevels),
             x=.75, y=-.04, cex=.9,
             col=trellis.par.get('superpose.symbol')$col, corner=c(0,1)),
           outerlabels=TRUE, autoarrange=TRUE, ...)
{
  X <- x
  at   <- attributes(x)
  Form <- at$formula
  nX   <- at$nX
  nY   <- at$nY

  groupslevels <- if(length(groups)) levels(x[[groups]])
  condvar <- setdiff(names(X), c('val', 'freq', 'denom', groups))
  ## Reorder condvar in descending order of number of levels
  numu <- function(x) if(is.factor(x)) length(levels(x))
                       else length(unique(x[! is.na(x)]))

  if(autoarrange && length(condvar) > 1) {
    nlev <- sapply(X[condvar], numu)
    condvar <- condvar[order(nlev)]
  }
  form <- if(length(formula)) formula
  else as.formula(
    paste('val ~ freq',
          paste(condvar, collapse=' * '), sep=' | '))
  
  pan <- function(x, y, subscripts, groups=NULL, ...) {
    y <- as.numeric(y)
    denom <- X$denom[subscripts]
    prop <- x / denom
    panel.dotplot(x/denom, y, subscripts=subscripts, groups=groups, ...)
    if(length(cex.values) && cex.values > 0) {
      col <- if(length(groups)) trellis.par.get('superpose.symbol')$col
       else trellis.par.get('dot.symbol')$col
      xl <- current.panel.limits()$xlim
      xdel <- 0.025 * diff(xl)
      yl <- current.panel.limits()$ylim
      ydel <- ydelta * diff(yl)
      txt <- if(length(groups)) {
        groups <- groups[subscripts]
        tx <- ''
        ig <- 0
        xw <- xwidth * diff(xl)
        xpos <- xl[2] - xdel - length(levels(groups)) * xw
        for(g in levels(groups)) {
          ig <- ig + 1
          i <- groups == g
          fr <- paste(x[i], denom[i], sep='/')
          xpos <- xpos + xw
          ltext(xpos, y - ydel, fr, cex=cex.values,
                col=col[ig], adj=1)
        }
      }
      else {
        fr <- paste(x, denom, sep='/')
        ltext(xl[2] - 0.025 * diff(xl), y - ydel, fr,
              cex=cex.values, col=col[1], adj=1)
      }
    }
  }

d <- if(!length(groups))
    dotplot(form, data=X, scales=list(y='free', rot=0), panel=pan,
            xlim=xlim, xlab='Proportion', ...)
  else eval(parse(text=
      sprintf("dotplot(form, groups=%s, data=X, scales=list(y='free', rot=0), panel=pan, xlim=xlim, auto.key=key, xlab='Proportion', ...)", groups) ))

if(outerlabels && (nX - length(groups) + 1) == 2)
    d <- useOuterStrips(d)

  ## Avoid wasting space for vertical variables with few levels
  if(condvar[length(condvar)] == 'var') {
    vars <- levels(X$var)
    nv <- length(vars)
    h <- integer(nv)
    for(i in 1 : nv) h[i] <- length(unique((X$val[X$var == vars[i]])))
    d <- resizePanels(d, h = h + 1)
  }
  d
}
