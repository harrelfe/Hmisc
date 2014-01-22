summaryP <- function(formula, data=NULL,
                     subset=NULL, na.action=na.retain,
                     exclude1=TRUE, sort=TRUE,
                     asna=c('unknown', 'unspecified'), ...) {
  
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
      if(!inherits(y, 'ynbind') && !inherits(y, 'pBlock')) {
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
    for(k in 1 : nY) {
      ## y <- yx[[k]] doesn't work as attributes lost by [.data.frame
      y <- Y[[k]]
      y <- if(is.matrix(y)) y[j,, drop=FALSE] else y[j]
#      y <- (Y[[k]])[j,, drop=FALSE]
      if(inherits(y, 'ynbind') || inherits(y, 'pBlock')) {
        overlab <- attr(y, 'label')
        labs    <- attr(y, 'labels')
        z <- NULL
        for(iy in 1 : ncol(y)) {
          tab <- table(y[, iy])
          no <- as.numeric(sum(tab))
          d <- if(inherits(y, 'ynbind'))
            data.frame(var=overlab,
                       val=labs[iy],
                       freq=as.numeric(tab['TRUE']),
                       denom=no)
          else
            data.frame(var=overlab,
                       val=names(tab),  # paste(labs[iy], names(tab)),
                       freq=as.numeric(tab),
                       denom=no)
          z <- rbind(z, d)
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
            nX=nX, nY=nY)
}

plot.summaryP <-
  function(x, formula=NULL, groups=NULL, xlim=c(-.05, 1.05), text.at=NULL,
           cex.values=0.5,
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
    panel.dotplot(x/denom, y, subscripts=subscripts, groups=groups, ...)
    if(length(cex.values) && cex.values > 0) {
      col <- if(length(groups)) trellis.par.get('superpose.symbol')$col
       else trellis.par.get('dot.symbol')$col

      longest.string <- paste(max(x), max(denom), sep='/  ')
      length.longest <- unit(1, 'strwidth', longest.string)
      xpos <- unit(1, 'npc') - unit(1, 'mm')
      txt <- if(length(groups)) {
        groups <- groups[subscripts]
        tx <- ''
        ig <- 0
        xpos <- xpos - length(levels(groups)) * length.longest
        for(g in levels(groups)) {
          ig <- ig + 1
          i <- groups == g
          fr <- paste(x[i], denom[i], sep='/')
          xpos <- xpos + length.longest
          grid.text(fr, xpos, unit(y, 'native') - unit(1, 'mm'),
                    just=c('right','top'), gp=gpar(cex=cex.values, col=col[ig]))
        }
      }
      else {
        fr <- paste(x, denom, sep='/')
        grid.text(fr, xpos, unit(y, 'native') - unit(1, 'mm'),
                  gp=gpar(cex=cex.values, col=col[1]), just=c('right','top'))
      }
    }
  }

  scal <- list(y='free', rot=0)
  scal$x <- if(length(text.at)) {
    at <- pretty(xlim)
    list(limits=range(c(xlim, text.at)), at=pretty(xlim))
  } else list(limits=xlim)
  d <- if(!length(groups))
    dotplot(form, data=X, scales=scal, panel=pan,
            xlab='Proportion', ...)
  else eval(parse(text=
                  sprintf("dotplot(form, groups=%s, data=X, scales=scal, panel=pan, auto.key=key, xlab='Proportion', ...)", groups) ))

#  if(outerlabels && ((nX - length(groups) + 1 == 2) ||
#                     length(dim(d)) == 2))  d <- useOuterStrips(d)
  if(length(dim(d)) == 2) d <- useOuterStrips(d)
  
  ## Avoid wasting space for vertical variables with few levels
  if(condvar[length(condvar)] == 'var') {
    vars <- levels(X$var)
    nv <- length(vars)
    h <- integer(nv)
    for(i in 1 : nv) h[i] <- length(unique((X$val[X$var == vars[i]])))
    w <- llist(d, h)
    d <- resizePanels(d, h = h + 1)
  }
  d
}

latex.summaryP <- function(object, groups=NULL, file='', round=3,
                           size=NULL, append=TRUE, ...) {
  class(object) <- 'data.frame'
  if(! append) cat('', file=file)

  p <- round(object$freq / object$denom, round)
  object$y <- paste(format(p), ' {\\scriptsize$\\frac{',
                    format(object$freq), '}{', format(object$denom),
                    '}$}', sep='')
  object$freq <- object$denom <- NULL

  stratvar <- setdiff(names(object), c('var', 'val', 'y', groups))
  svar <- if(! length(stratvar)) as.factor(rep('', nrow(object)))
   else {
     if(length(stratvar) == 1) object[[stratvar]]
      else do.call('interaction', list(object[stratvar], sep=' '))
   }

  object$stratvar <- svar
  object <- object[, c('var', 'val', 'y', groups, 'stratvar')]

  nl <- 0

  slev <- levels(svar)
  nslev <- length(slev)
  for(i in 1 : nslev) {
    
    if(nslev > 1) cat('\n\\vspace{1ex}\n\n\\textbf{', slev[i],
                      '}\n\\vspace{1ex}\n\n', sep='', file=file, append=TRUE)
    x <- object[svar == slev[i], colnames(object) != 'stratvar']
    if(length(groups)) {
    r <- reshape(x, timevar=groups, direction='wide',
                 idvar=c('var', 'val'))

    lev <- levels(x[[groups]])
    nl  <- length(lev)
    var <- unique(as.character(r$var))
    w <- latex(r[colnames(r) != 'var'],
               table.env=FALSE, file=file, append=TRUE,
               rowlabel='', rowname=rep('', nrow(r)),
               rgroup=levels(r$var), n.rgroup=as.vector(table(r$var)),
               size=size,
               colheads=c(' ', lev),
               center='none')
  }
    else {
      w <- latex(x[colnames(x) != 'var'],
                 table.env=FALSE, file=file, append=TRUE,
                 rowlabel='', rowname=rep('', nrow(x)),
                 rgroup=levels(x$var), n.rgroup=as.vector(table(x$var)),
                 size=size, colheads=c(' ', ' '), center='none')
    }
  }
  attr(w, 'ngrouplevels') <- nl
  w
}


