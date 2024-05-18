redun <- function(formula, data=NULL, subset=NULL,
                  r2=.9, type=c('ordinary','adjusted'),
                  nk=3, tlinear=TRUE, rank=qrank, qrank=FALSE,
                  allcat=FALSE, minfreq=0,
                  iterms=FALSE, pc=FALSE,
                  pr=FALSE, ...)
{
  acall   <- match.call()
  type    <- match.arg(type)

  if(inherits(formula, 'formula')) {
    a <- as.character(formula)
    if(length(a) == 2 && a[1] == '~' && a[2] == '.' &&
       length(list(...))) data <- dataframeReduce(data, ...)

    Terms <- terms(formula, specials='I', data=data)
    # model.frame does not remove variables with a minus sign in front
    # of their names, but these are not in term.labels
    allvars  <- setdiff(as.character(attr(Terms, 'variables')), 'list')
    keptvars <- attr(Terms, 'term.labels')
    if(length(allvars) > length(keptvars)) {
      formula <- as.formula(paste('~', paste(keptvars, collapse='+')))
      Terms   <- terms(formula, specials='I', data=data)
    }

    m     <- list(formula=formula, data=data, subset=subset,
                  na.action=na.delete)
    data      <- do.call('model.frame', m)
    data      <- data[attr(Terms, 'term.labels')]
    nam       <- names(data)
    linear    <- nam[attr(Terms,'specials')$I]
    na.action <- attr(data, 'na.action')
    if(pr) naprint(na.action)
  } else {
    if(! is.matrix(formula))
      stop("formula must be a numeric matrix when it's not an actual formula")
    data      <- as.data.frame(formula)
    formula   <- NULL
    na.action <- NULL
    nam       <- names(data)
    p         <- length(data)
    linear    <- rep(FALSE, length(data))
  }
  
  p <- length(data)
  n <- nrow(data)

  cat.levels <- vector('list',p)
  names(cat.levels) <- nam
  vtype <- rep(if(qrank) 'q' else if(rank) 'r' else 's', p)
  names(vtype) <- nam
  enough <- rep(TRUE, p)
  if(rank) nk <- 0

  for(i in 1:p)
    {
      xi  <- data[[i]]
      ni  <- nam[i]

      iscat <- FALSE
      if(is.character(xi))
        {
          xi    <- as.factor(xi)
          lev   <- levels(xi)
          iscat <- TRUE
        }
      else if(is.factor(xi))
        {
          lev   <- levels(xi)
          iscat <- TRUE
        }
      if(iscat)
        {
          data[[i]] <- as.integer(xi)
          cat.levels[[ni]] <- lev
          vtype[ni] <- 'c'
          if(minfreq > 0 && sum(table(xi) >= minfreq) < 2) enough[i] <- FALSE
        }
      else
        {
          u <- unique(xi)
          if(length(u) == 1)
            {
              warning(paste(ni,'is constant'))
              enough[i] <- FALSE
            }
          if(minfreq > 0 && length(u)==2 && sum(table(xi) >= minfreq) < 2)
            enough[i] <- FALSE
          if(nk==0 || length(u) < 3 || ni %in% linear)
            vtype[ni] <- if(vtype[ni] == 'q') 'r' else 'l'
        }
  }
  toofew <- nam[! enough]
  if(length(toofew))
    {
      p <- sum(enough)
      nam <- nam[enough]
      cat.levels <- cat.levels[enough]
      vtype <- vtype[enough]
      data  <- data[enough]
    }

  dfs <- c(l=1, s=nk - 1, r=1, q=2, c=0)
  xdf <- dfs[vtype]
  j   <- vtype=='c'
  if(any(j)) for(i in which(j)) xdf[i] <- length(cat.levels[[i]]) - 1
  names(xdf) <- nam
  orig.df <- sum(xdf)
  X <- matrix(NA, nrow=n, ncol=orig.df)
  st <- en <- integer(p)
  start <- 1
  for(i in 1:p)
    {
      xi <- data[[i]]
      if(vtype[i] %in% c('r', 'q')) {
        xi <- rank(xi) / length(xi)
        x  <- if(vtype[i] == 'q') cbind(xi, xi ^ 2) else as.matrix(xi)
        }
      else x <- aregTran(xi, vtype[i], nk)
      st[i] <- start
      nc    <- ncol(x)
      xdf[i]<- nc
      end   <- start + nc - 1
      en[i] <- end
      if(end > orig.df) stop('program logic error')
      X[, start : end] <- x
      start <- end + 1
    }

  if(end < orig.df) X <- X[, 1:end, drop=FALSE]
  ## if couldn't derive the requested number of knots in splines

  fcan <- function(ix, iy, X, st, en, vtype, tlinear, type,
                   allcat, r2, minfreq)
    {
      ## Get all subscripts for variables in the right hand side
      k <- rep(FALSE, ncol(X))
      for(i in ix) k[st[i] : en[i]] <- TRUE
      ytype <- if(tlinear && vtype[iy] %in% c('s', 'q')) 'l' else vtype[iy]
      Y <- if(ytype=='l') X[, st[iy], drop=FALSE] else
       X[, st[iy] : en[iy], drop=FALSE]
      d <- dim(Y); n <- d[1]; ny <- d[2]
      f <- cancor(X[, k, drop=FALSE], Y)
      R2 <- f$cor[1]^2
      if(type=='adjusted')
        {
          dof <- sum(k) + ny - 1
          R2  <- max(0, 1 - (1 - R2) * (n - 1) / (n - dof - 1))
        }
      ycoef  <- f$ycoef[, 1]
      yscore <- matxv(Y, ycoef) * sqrt(n - 1) - sum(ycoef * f$ycenter)
    ## If variable to possibly remove is categorical with more than 2
    ## categories (more than one dummy variable) make sure ALL frequent
    ## categories are redundant (not just the linear combination of
    ## dummies) if allcat is TRUE.  Do this by substituting for R^2 the
    ## minimum R^2 over predicting each dummy variable.
    if(R2 > r2 && allcat && ytype=='c' && (en[iy] > st[iy]))
      {
        for(j in st[iy] : en[iy])
          {
            y <- X[, j, drop=FALSE]
            if(sum(y) >= minfreq && n - sum(y) >= minfreq)
              {
                f <- cancor(X[, k, drop=FALSE], y)
                R2c <- f$cor[1]^2
                if(type=='adjusted')
                  {
                    dof <- sum(k)
                    R2c <- max(0, 1 - (1 - R2c)*(n-1)/(n-dof-1))
                  }
                R2 <- min(R2, R2c, na.rm=TRUE)
              }
          }
      }
      list(R2=R2, yscore=yscore)
    }

  if(iterms)
    {
      nc <- ncol(X)
      nm <- NULL
      for(i in 1:p)
        {
          m <- nam[i]
          np <- en[i] - st[i] + 1
          if(np > 1) for(j in 1:(np-1))
            m <- c(m, paste(nam[i], paste(rep("'", j), collapse=''), sep=''))
          nm <- c(nm, m)
          if(pc) X[,st[i]:en[i]] <- prcomp(X[,st[i]:en[i]], scale=TRUE)$x
        }
      colnames(X) <- nm
      p <- nc
      nam <- nm
      st <- en <- 1:nc
      vtype <- rep('l', nc)
    }

  In <- 1:p; Out <- integer(0)
  r2r <- numeric(0)
  r2l <- scores <- list()

  for(i in 1:p) {
    if(pr) cat('Step',i,'of a maximum of', p, '\r')
    ## For each variable currently on the right hand side ("In")
    ## find out how well it can be predicted from all the other "In" variables
    if(length(In) < 2) break
    Rsq <- In * 0
    l <- 0
    for(j in In)
      {
        l <- l + 1
        k <- setdiff(In, j)
        fc <- fcan(k, j, X, st, en, vtype, tlinear, type,
                       allcat, r2, minfreq)
        Rsq[l] <- fc$R2
        if(! length(scores[[nam[j]]])) scores[[nam[j]]] <- fc$yscore
      }
    if(i==1) {Rsq1 <- Rsq; names(Rsq1) <- nam[In]}
    if(max(Rsq) < r2) break
    removed   <- In[which.max(Rsq)]
    r2removed <- max(Rsq)
    ## Check that all variables already removed can be predicted
    ## adequately if new variable 'removed' is removed
    k <- setdiff(In, removed)
    r2later <- NULL
    if(length(Out))
      {
        r2later <- Out*0
        names(r2later) <- nam[Out]
        l <- 0
        for(j in Out)
          {
            l  <- l+1
            fc <- fcan(k, j, X, st, en, vtype, tlinear, type, allcat, r2, minfreq)
            r2later[l] <- fc$R2
            if(! length(scores[[nam[j]]])) scores[[nam[j]]] <- fc$yscore
          }
        if(min(r2later) < r2) break
      }
    Out <- c(Out, removed)
    In  <- setdiff(In, Out)
    r2r <- c(r2r, r2removed)
    if(length(r2later)) r2l[[i]] <- r2later
  }
  if(length(r2r)) names(r2r) <- nam[Out]
  if(length(r2l)) names(r2l) <- nam[Out]
  if(pr) cat('\n')
  
  structure(list(call=acall, formula=formula,
                 In=nam[In], Out=nam[Out], toofew=toofew,
                 rsquared=r2r, r2later=r2l, rsq1=Rsq1,
                 n=n, p=p, rank=rank, qrank=qrank,
                 na.action=na.action,
                 vtype=vtype, tlinear=tlinear,
                 allcat=allcat, minfreq=minfreq, nk=nk, df=xdf,
                 cat.levels=cat.levels,
                 r2=r2, type=type, scores=do.call('cbind', scores)),
            class='redun')
}

print.redun <- function(x, digits=3, long=TRUE, ...)
{
  prcvec <- function(w) cat(strwrap(paste(w, collapse=' ')), '', sep='\n')

  cat("\nRedundancy Analysis\n\n")
  if(length(x$formula)) {
    print(x$formula)
    cat("\n")
    }
  cat('n:',x$n,'\tp:',x$p, '\tnk:',x$nk,'\n')
  cat('\nNumber of NAs:\t', length(x$na.action$omit), '\n')
  a <- x$na.action
  if(length(a)) naprint(a)

  ranks <- 'rank' %in% names(x) && x$rank
  if(ranks)
    cat('\nAnalysis used ranks',
        if(x$qrank) 'and square of ranks', '\n')
  
  if(x$tlinear)
    cat('\nTransformation of target variables forced to be',
        if(ranks) 'linear in the ranks\n' else 'linear\n')
  if(x$allcat)
    cat('\nAll levels of a categorical variable had to be redundant before the\nvariable was declared redundant\n')
  if(x$minfreq > 0)
    cat('\nMinimum category frequency required for retention of a binary or\ncategorical variable:', x$minfreq, '\n')
  if(length(x$toofew))
    {
      cat('\nBinary or categorical variables removed because of inadequate frequencies:\n\n')
      cat(x$toofew, '\n')
    }
  cat('\nR-squared cutoff:', x$r2, '\tType:', x$type,'\n')
  if(long)
    {
      cat('\nR^2 with which each variable can be predicted from all other variables:\n\n')
      print(round(x$rsq1, digits))
      if(x$allcat)
        cat('\n(For categorical variables the minimum R^2 for any sufficiently\nfrequent dummy variable is displayed)\n\n')
    }
  if(!length(x$Out))
    {
      cat('\nNo redundant variables\n\n')
      return(invisible())
    }
  cat('\nRendundant variables:\n\n')
  prcvec(x$Out)
  cat('\nPredicted from variables:\n\n')
  prcvec(x$In)
  w <- x$r2later
  vardel <- names(x$rsquared)
  if(! long)
    {
      print(data.frame('Variable Deleted'=vardel,
                       'R^2'=round(x$rsquared,digits),
                       row.names=NULL, check.names=FALSE))
      return(invisible())
    }
  later  <- rep('', length(vardel))
  i <- 0
  for(v in vardel)
    {
      i <- i + 1
      for(z in w)
        {
          if(length(z) && v %in% names(z))
            later[i] <- paste(later[i], round(z[v], digits), sep=' ')
        }
    }
  print(data.frame('Variable Deleted'=vardel,
                   'R^2'=round(x$rsquared,digits),
                   'R^2 after later deletions'=later,
                   row.names=NULL,
                   check.names=FALSE))
  invisible()
}

