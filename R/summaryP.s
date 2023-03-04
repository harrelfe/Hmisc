summaryP <- function(formula, data=NULL,
                     subset=NULL, na.action=na.retain,
                     sort=TRUE,
                     asna=c('unknown', 'unspecified'), ...) {
  
  formula <- Formula::Formula(formula)

  Y <- if(length(subset))
    model.frame(formula, data=data, subset=subset, na.action=na.action)
  else
    model.frame(formula, data=data, na.action=na.action)
  X <- Formula::model.part(formula, data=Y, rhs=1)
  Y <- Formula::model.part(formula, data=Y, lhs=1)
  nY <- NCOL(Y)
  nX <- NCOL(X)
  namY <- names(Y)
  if(nX == 0) X <- data.frame(x=rep(1, NROW(Y)))
  else {
    ## Remove observations with any values of X NA
    i <- apply(is.na(X), 1, any)
    if(any(i)) {
      X <- X[! i,, drop=FALSE]
      Y <- Y[! i,, drop=FALSE]
    }
  }
  ux <- unique(X)
  Z <- NULL
  n <- nrow(X)
  Lev <- character(0)
  
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
  ## Save combinations of var and val to exclude if exclude1 is used in
  ## a function that operates on summaryP result
  ylevels.to.exclude1 <- NULL
  for(ny in namY) {
    y <- Y[[ny]]
    la <- label(y)
    if(la == '') la <- ny
    tab <- table(y)
    tab <- structure(as.numeric(tab), names=names(tab))
    if(length(tab) == 2)
      ylevels.to.exclude1 <-
      rbind(ylevels.to.exclude1,
            data.frame(var=la, val=names(tab)[which.max(tab)]))
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
          
          if(inherits(y, 'ynbind')) {
            d <- data.frame(var=overlab,
                            val=labs[iy],
                            freq=as.numeric(tab['TRUE']),
                            denom=no)
            Lev <- c(Lev, as.character(labs[iy]))
          } else {
            d <- data.frame(var=overlab,
                            val=names(tab),  # paste(labs[iy], names(tab)),
                            freq=as.numeric(tab),
                            denom=no)
            Lev <- c(Lev, names(tab))
          }
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
        ## mf <- mfreq[[ny]]
        no <- as.numeric(sum(tab))
        if(sort) lev <- reorder(lev, (mfreq[[ny]])[lev])
        Lev <- c(Lev, as.character(lev))
        z <- data.frame(var   = unname(la),
                        val   = lev,
                        freq  = as.numeric(tab),
                        denom = no,
                        stringsAsFactors=TRUE)
      }
      ## Add current X subset settings
      if(nX > 0)
        for(k in 1: nX)
         z[[names(ux)[k]]] <- if(is.character(ux[i,k]))
                                factor(ux[i, k]) else ux[i,k]
      Z <- rbind(Z, z)
    }
  }
  Z$val <- factor(Z$val, levels=unique(Lev))

  yl <- ylevels.to.exclude1
  iex <- integer(0)
  if(length(yl)) {
    for(i in 1 : nrow(Z)) {
      exi <- FALSE
      for(j in 1 : nrow(yl))
        if(as.character(Z$var[i]) == as.character(yl$var[j]) &&
           as.character(Z$val[i]) == as.character(yl$val[j])) exi <- TRUE
      if(exi) iex <- c(iex, i)
    }
  }

  
  structure(Z, class=c('summaryP', 'data.frame'), formula=formula,
            nX=nX, nY=nY, rows.to.exclude1=iex)
}

plot.summaryP <-
  function(x, formula=NULL, groups=NULL,
           marginVal=NULL, marginLabel=marginVal, refgroup=NULL,
           exclude1=TRUE, xlim=c(-.05, 1.05), text.at=NULL,
           cex.values=0.5,
           key=list(columns=length(groupslevels),
             x=.75, y=-.04, cex=.9,
             col=lattice::trellis.par.get('superpose.symbol')$col,
             corner=c(0,1)),
           outerlabels=TRUE, autoarrange=TRUE, col=colorspace::rainbow_hcl,
           ...)
{
  sRequire('lattice')
  sRequire('latticeExtra')
  ## marginval: category name indicating addMarginal summaries (usually 'All')
  ## marginLabel: a fuller label for this, e.g. 'All Regions'
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

  if(exclude1 && length(at$rows.to.exclude1))
    X <- X[- at$rows.to.exclude1, , drop=FALSE]

  if(autoarrange && length(condvar) > 1) {
    nlev <- sapply(X[condvar], numu)
    condvar <- condvar[order(nlev)]
  }

  if(grType() == 'plotly') {
    condvar <- setdiff(condvar, 'var')
    if(length(condvar) > 1)
      stop('with options(grType="plotly") does not handle > 1 stratification variable')
    if(length(condvar) == 1 && length(marginVal)) {
      X$Big <- X[[condvar]] == marginVal
      if(marginLabel != marginVal)
        X[[condvar]] <- ifelse(X$Big, marginLabel, as.character(X[[condvar]]))
    }

    X$.gg. <-
      if(length(groups)) {
        if(length(condvar) == 1 && length(marginVal))
          ifelse(X$Big, as.character(X[[groups]]),
                 paste0(X[[groups]], ' stratified<br>by ', condvar[1]))
        else
          X[[groups]]
      }
    p <-
      with(X,
           dotchartpl(freq / denom,
                      major = var,
                      minor = val,
                      group = if(length(groups)) .gg.,
                      mult  = if(length(condvar) > 0) X[[condvar]],
                      big   = if(length(condvar) == 1 && length(marginVal)) Big,
                      num   = freq,
                      denom = denom,
                      refgroup = refgroup,
                      xlim  = xlim,
                      col   = col,
                      nonbigtracename=if(length(condvar))
                                        paste0('Stratified by\n', condvar[1])
                                      else
                                        'Stratified Estimates',
                      ...)
           )
    return(p)
    }
  
  form <- if(length(formula)) formula
  else as.formula(
    paste('val ~ freq',
          paste(condvar, collapse=' * '), sep=' | '))
  
  pan <- function(x, y, subscripts, groups=NULL, ...) {
    y <- as.numeric(y)
    denom <- X$denom[subscripts]
    lattice::panel.dotplot(x/denom, y, subscripts=subscripts,
                           groups=groups, ...)
    if(length(cex.values) && cex.values > 0) {
      col <- if(length(groups)) lattice::trellis.par.get('superpose.symbol')$col
       else lattice::trellis.par.get('dot.symbol')$col

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
    list(limits=range(c(xlim, text.at)), at=at[at >= -0.0001 & at <= 1.0001])
  } else list(limits=xlim)
  d <- if(!length(groups))
    lattice::dotplot(form, data=X, scales=scal, panel=pan,
            xlab='Proportion', ...)
  else eval(parse(text=
                  sprintf("lattice::dotplot(form, groups=%s, data=X, scales=scal, panel=pan, auto.key=key, xlab='Proportion', ...)", groups) ))

#  if(outerlabels && ((nX - length(groups) + 1 == 2) ||
#                     length(dim(d)) == 2))  d <- useOuterStrips(d)
  if(length(dim(d)) == 2) d <- latticeExtra::useOuterStrips(d)
  ## Avoid wasting space for vertical variables with few levels
  if(condvar[length(condvar)] == 'var') {
    vars <- levels(X$var)
    nv <- length(vars)
    h <- integer(nv)
    for(i in 1 : nv) h[i] <- length(unique((X$val[X$var == vars[i]])))
    d <- latticeExtra::resizePanels(d, h = h + 1)
  }
  d
}

ggplot.summaryP <-
  function(data, mapping, groups=NULL, exclude1=TRUE, xlim=c(0, 1),
           col=NULL, shape=NULL, size=function(n) n ^ (1/4),
           sizerange=NULL, abblen=5,
           autoarrange=TRUE, addlayer=NULL, ..., environment)
{
  X <- data
  class(X) <- setdiff(class(X), 'summaryP')
  at   <- attributes(X)
  Form <- at$formula
  nX   <- at$nX
  nY   <- at$nY

  groupslevels <- if(length(groups)) levels(X[[groups]])
  condvar <- setdiff(names(X), c('val', 'freq', 'denom', groups))
  ## Reorder condvar in descending order of number of levels
  numu <- function(x) if(is.factor(x)) length(levels(x))
                       else length(unique(x[! is.na(x)]))

  if(exclude1 && length(at$rows.to.exclude1))
    X <- X[- at$rows.to.exclude1, , drop=FALSE]

  if(autoarrange && length(condvar) > 1) {
    nlev <- sapply(X[condvar], numu)
    condvar <- condvar[order(nlev)]
  }

  ## Find list of variables that contain only one level but have a
  ## variable name longer than abblen characters.
  ## The space devoted
  ## to one-level variables is not tall enough to print the variable name.
  ## Replace the name with (1) (2) ... and put the variable names possibly
  ## in a footnote

  fnvar <- ''
  lvar <- levels(X$var)
  i <- 0
  for(v in lvar) {
    maxlen <- nchar(v)   # max(nchar(strsplit(v, split=' ')[[1]]))
    if(maxlen > abblen) {
      nlev <- length(unique(X$val[X$var == v]))
      if(nlev == 1) {
        i <- i + 1
        w <- paste('(', i, ')', sep='')
        if(i > 1) fnvar <- paste(fnvar, '; ', sep='')
        fnvar <- paste(fnvar, w, ' ', v, sep='')
        levels(X$var)[levels(X$var) == v] <- w
      }
    }
  }
  
  spl <- function(x) {
    u <- levels(x)
    n <- length(u)
    utrans <- character(n); names(utrans) <- u
    for(w in u)
      utrans[w] <- paste(strwrap(w, 10), collapse='\n')
    factor(x, u, utrans)
  }
  X$var <- spl(X$var)
  if(length(condvar) == 2) {
    othvar <- setdiff(condvar, 'var')
    X[[othvar]] <- spl(X[[othvar]])
  }
  N <- X$denom
  rN <- range(N)
  ratioN <- rN[2] / rN[1]
  if(diff(rN) < 10 | (ratioN < 1.2)) size <- NULL

  ## plotly hover label
  X$hov <- paste0(round(X$freq / X$denom, 3), '&emsp;',
                  markupSpecs$html$frac(X$freq, X$denom, size=90))
  if(length(groups)) X$hov <- paste0(X[[groups]], '<br>', X$hov)
  k <- 'ggplot(X, aes(x=freq / denom, y=val, text=hov'
  if(length(groups)) k <- paste(k, sprintf(', color=%s, shape=%s',
                                           groups, groups))
  k <- paste(k, '))')
  
  if(length(size)) {
    k <- paste(k,
               if(length(size)) 'geom_point(aes(size = N))' else
                'geom_point()',
               sep=' + ')
    Ns <- X$denom
    if(! length(sizerange)) {
      fn <- if(is.function(size)) size else sqrt
      sizerange <- c(max(0.7, 2.7 / fn(ratioN)), 3.25)
    }

    if(is.function(size)) {
      X$N <- size(Ns)
      Ns0 <- Ns[Ns > 0]
      uN <- unique(sort(Ns0))
      Nbreaks <- if(length(uN) < 8) uN else
       unique(round(quantile(Ns0, (0 : 6) / 6, type=1)))
      Nbreakst <- size(Nbreaks)
      k <- paste(k,
       'scale_size_continuous(breaks=Nbreakst, labels=format(Nbreaks), range=sizerange)', sep=' + ')
    }
    else {
      k <- paste(k, 'scale_size_discrete(range = sizerange)', sep=' + ')
      X$N <- cut2(Ns, g=size)
    }
  }
  else k <- paste(k, 'geom_point()', sep=' + ')
  p <- eval(parse(text=k))
  
  if(length(addlayer)) p <- p + addlayer
  if('var' %nin% condvar) stop('program logic error')
  if(length(condvar) == 1)
    p <- p + facet_grid(var ~ . , scales='free_y', space='free_y')
  else {
    p <- p + facet_grid(as.formula(sprintf('var ~ %s', othvar)),
                        scales='free_y', space='free_y')
  }
  p <- p + xlim(xlim) + xlab('Proportion') + ylab('')
  if(length(col))   p <- p + scale_color_manual(values=col)
  if(length(shape)) p <- p + scale_shape_manual(values=shape)
  
  if(fnvar != '') attr(p, 'fnvar') <- fnvar
  p
}


latex.summaryP <- function(object, groups=NULL, exclude1=TRUE, file='', round=3,
                           size=NULL, append=TRUE, ...) {
  class(object) <- 'data.frame'
  rte <- attr(object, 'rows.to.exclude1')

  if(exclude1 && length(rte))
    object <- object[- rte, , drop=FALSE]

  if(! append) cat('', file=file)

  p <- ifelse(object$denom == 0, '',
              format(round(object$freq / object$denom, round)))
  object$y <- paste(p, ' {\\scriptsize$\\frac{',
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

  slev  <- levels(svar)
  nslev <- length(slev)

  for(i in 1 : nslev) {
    
    if(nslev > 1) cat('\n\\vspace{1ex}\n\\begin{minipage}{\\linewidth}\n\\textbf{', slev[i],
                      '}\n\\vspace{1ex}\n\n', sep='', file=file, append=TRUE)
     x <- object[svar == slev[i], colnames(object) != 'stratvar']

    if(length(groups)) {
      ord <- function(v) {
        v  <- as.character(v)
        un <- unique(v)
        vn <- 1 : length(un)
        names(vn) <- un
        vn[v]
      }
      varn <- ord(x$var)
      valn <- ord(x$val)
      
      r <- reshape(x, timevar=groups, direction='wide',
                   idvar=c('var', 'val'))
      ## reorder rows to be in original order
      ir <- order(varn[as.character(r$var)],
                  valn[as.character(r$val)])
      r <- r[ir, ]
      
      ## reshape does not respect order of levels of factors; reorder columns
      lev <- levels(x[[groups]])
      r <- r[c('var', 'val', paste('y', lev, sep='.'))]
      
      nl  <- length(lev)

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
    if(nslev > 1) cat('\\end{minipage}\n', file=file, append=TRUE)
  }
  attr(w, 'ngrouplevels') <- nl
  attr(w, 'nstrata') <- nslev
  w
}
