aregImpute <- function(formula, data, subset, n.impute=5,
                       group=NULL, method=c('ace','avas'),
                       type=c('pmm','regression'),
                       match=c('weighted','closest'), fweighted=0.2,
                       defaultLinear=FALSE, x=FALSE,
                       pr=TRUE, plotTrans=FALSE) {
  
  acall   <- match.call()
  method  <- match.arg(method)
  type    <- match.arg(type)
  match   <- match.arg(match)
  if(.R.) require('acepack')  # provides ace, avas

## Temporarily fix bug in ace
  if(.R.) {
ace <- function (x, y, wt = rep(1, nrow(x)), cat = NULL, mon = NULL, 
    lin = NULL, circ = NULL, delrsq = 0.01) 
{
    x <- as.matrix(x)
    if (delrsq <= 0) {
        cat("delrsq must be positive")
        return()
    }
    iy <- ncol(x) + 1
    l <- matrix(1, ncol = iy)
    if (!is.null(circ)) {
        for (i in 1:length(circ)) {
            if (circ[i] < 0 || circ[i] > ncol(x)) {  # FEH nrow -> ncol
                cat("bad circ= specification")
                return()
            }
            if (circ[i] == 0) {
                cat("response spec can only be lin or ordered (default)")
                return()
            }
            else {
                nncol <- circ[i]
                if (l[nncol] != 2 & l[nncol] != 1) {
                  cat("conflicting transformation specifications")
                  return()
                }
                l[nncol] <- 2
            }
        }
    }
    if (length(mon)) {
        for (i in 1:length(mon)) {
            if (mon[i] < 0 || mon[i] > ncol(x)) {  # FEH nrow -> ncol
                cat("bad mon= specification")
                return()
            }
            if (mon[i] == 0) {

              ## Next 2 lines commented out FEH
              ##  cat("response spec can only be lin or ordered (default)")
              ##  return()
            }
            else {
                nncol <- mon[i]
                if (l[nncol] != 3 && l[nncol] != 1) {
                  cat("conflicting transformation specifications")
                  return()
                }
                l[nncol] <- 3
            }
        }
    }
    if (length(lin)) {
        for (i in 1:length(lin)) {
            if (lin[i] < 0 || lin[i] > ncol(x)) {  # FEH nrow -> ncol
                cat("bad lin= specification")
                return()
            }
            if (lin[i] == 0) {
                nncol <- iy
            }
            else {
                nncol <- lin[i]
            }
            if (l[nncol] != 4 && l[nncol] != 1) {
                cat("conflicting transformation specifications")
                return()
            }
            l[nncol] <- 4
        }
    }
    if (length(cat)) {
        for (i in 1:length(cat)) {
            if (cat[i] < 0 || cat[i] > ncol(x)) {  # FEH nrow -> ncol
                cat("bad cat= specification")
                return()
            }
            if (cat[i] == 0) {
#  Next 2 lines commented out FEH
#                cat("response spec can only be lin or ordered (default)")
#                return()
            }
            else {
                nncol <- cat[i]
                if (l[nncol] != 4 && l[nncol] != 1) {
                  cat("conflicting transformation specifications")
                  return()
                }
                l[nncol] <- 4
            }
        }
    }
    tx <- x
    ty <- y
    m <- matrix(0, nrow = nrow(x), ncol = iy)
    z <- matrix(0, nrow = nrow(x), ncol = 12)
    z <- as.matrix(z)
    ns <- 1
    mode(x) <- "double"
    mode(y) <- "double"
    mode(tx) <- "double"
    mode(ty) <- "double"
    mode(wt) <- "double"
    mode(delrsq) <- "double"
    mode(z) <- "double"
    junk <- .Fortran("mace", p = as.integer(ncol(x)), n = as.integer(nrow(x)), 
        x = t(x), y = y, w = as.double(wt), l = as.integer(l), 
        delrsq = delrsq, ns = as.integer(ns), tx = tx, ty = ty, 
        rsq = double(1), ierr = integer(1), m = as.integer(m), 
        z = z, PACKAGE = "acepack")
    return(junk)
}
}
  
  if(!inherits(formula,'formula')) stop('formula must be a formula')
  nam <- var.inner(formula)

  m <- match.call(expand = FALSE)
  Terms <- terms(formula, specials=c('I','monotone'))
  m$formula <- formula
  m$match <- m$fweighted <- m$x <- m$n.impute <- m$defaultLinear <-
  m$type <- m$group <- m$method <- m$pr <- m$plotTrans <- m$... <- NULL
  m$na.action <- na.retain

  m[[1]] <- as.name("model.frame")
  z <- eval(m, sys.parent())
  p <- length(z)
  n <- nrow(z)
  rnam <- row.names(z)
  if(length(rnam)==0) rnam <- as.character(1:n)

  lgroup <- length(group)
  if(lgroup) {
    if(lgroup != n)
      stop('group should have length equal to number of observations')
    ngroup <- length(unique(group[!is.na(group)]))
  }

  linear <- nam[attr(Terms,'specials')$I]
  mono <- nam[attr(Terms,'specials')$monotone]

  cat.levels <- vector('list',p)
  names(cat.levels) <- nam
  categorical <- character(0)
  na <- vector('list',p)
  names(na) <- nam
  nna <- integer(p); names(nna) <- nam

  xf <- matrix(as.double(1), nrow=n, ncol=p, dimnames=list(rnam,nam))
  imp <- vector('list',p)
  names(imp) <- nam
  if(lgroup) group.inds <- imp

  for(i in 1:p) {
	  xi <- z[[i]]
      ni <- nam[i]
      nai <- is.na(xi)
      na[[i]] <- (1:n)[nai] 
      nna[i] <- nnai <- sum(nai)
      if(nnai > 0) imp[[ni]] <-  matrix(NA, nrow=nnai, ncol=n.impute,
                                        dimnames=list(rnam[nai],NULL))
      if(lgroup) {
        if(any(is.na(group[!nai]))) stop('NAs not allowed in group')
        if(length(unique(group[!nai])) != ngroup)
          stop(paste('not all',ngroup,
                     'values of group are represented in\n',
                     'observations with non-missing values of',
                     ni))
        group.inds[[i]] <- split((1:n)[!nai], group[!nai])
      }
  
      iscat <- FALSE
	  if(is.character(xi)) {
		xi <- as.factor(xi)
        lev <- levels(xi)
        iscat <- TRUE
      } else if(is.category(xi)) {
        lev <- levels(xi)
        iscat <- TRUE
      }
      if(iscat) {
        cat.levels[[ni]] <- lev
        xi <- as.integer(xi)
        categorical <- c(categorical,ni)
      } else {
        u <- unique(xi[!nai])
        if(length(u) == 1) stop(paste(ni,'is constant')) else
         if((defaultLinear || length(u) == 2) &&
            ni %nin% linear) linear <- c(linear, ni)
      }
      xf[,i] <- xi
      ## Initialize imputed values to random sample of non-missings
      if(nnai > 0) xf[nai,i] <-
        sample(xi[!nai], nnai, replace=nnai > (n-nnai))
    }
  z <- NULL
  wna <- (1:p)[nna > 0]
  ## xf = original data matrix (categorical var -> integer codes)
  ## with current imputations
  rsq <- double(length(wna)); names(rsq) <- nam[wna]
  
  if(pr) cat('Iteration:')
  for(iter in 1:(3+n.impute)) {
    if(pr) cat(iter,'')
    for(i in wna) {
      nai <- na[[i]]      ## subscripts of NAs on xf[i,]
      j <- (1:n)[-nai]    ## subscripts of non-NAs on xf[i,]
      npr <- length(j)
      if(lgroup) {        ## insure orig. no. obs from each level of group
        s <- rep(NA, npr)
        for(ji in 1:ngroup) {
          gi <- (group.inds[[i]])[[ji]]
          s[gi] <- sample(gi, length(gi), replace=TRUE)
        }
      } else s <- sample(j, npr, replace=TRUE)  ## sample of non-NAs
      nami <- nam[i]
      nm <- c(nami, nam[-i])

      X <- xf[,-i,drop=FALSE]
      w <- list(x=X[s,], y=xf[s,i])
      if(length(mono))        w$mon  <- match(mono, nm) - 1
      if(length(categorical)) w$cat  <- match(categorical, nm) - 1
      if(length(linear))      w$lin  <- match(linear, nm) - 1
      f <- do.call(if(method=='ace' || nami %in% categorical)
                   'ace' else 'avas', w)
      if(plotTrans) {
        plot(f$y, f$ty, xlab=nami, ylab=paste('Transformed',nami))
        xx <- if(is.matrix(f$x)) (if(method=='ace') t(f$x) else f$x)
        else
          as.matrix(f$x)
        ## bug in ace returns transpose of x
        tx <- as.matrix(f$tx)
        for(jj in 1:ncol(xx))
          plot(xx[,jj], tx[,jj],
               xlab=nm[jj+1], ylab=paste('Transformed',nm[jj+1]))
      }
      ## avas does not handle categorical response variables
      cof <- lm.fit.qr.bare(f$tx, f$ty)$coef
      rsq[nami] <- f$rsq
      ## fitter does not automatically make coefficients=1
      pti <- cof[1]    ## predicted transformed xf[,i]
      for(k in 1:(p-1)) {
        ## Transform each RHS variable, all original obs.
        if(length(unique(X[s,k]))==1) {
          cat('\n\n')
          print(table(X[,k]))
          stop(paste('Variable', dimnames(X)[[2]][k],
                     '\nhas only one unique value in a bootstrap sample.\n',
                     'See above for overall frequency distribution.'))
        }
        tk <- if(TRUE || .R.) approxExtrap(X[s,k], f$tx[,k], xout=X[,k])$y else
                      approx(X[s,k], f$tx[,k], xout=X[,k], rule=3)$y
        ## Bug in approx with rule=3 resulting in NA for 6.0 Linux
        pti <- pti + cof[k+1]*tk
      }
      if(type=='pmm') {
        whichclose <- if(match=='closest') {
          ## Jitter predicted transformed values for non-NAs to randomly
          ## break ties in matching with predictions for NAs in xf[,i]
          ## Becuase of normalization used by fitter, pti usually ranges from
          ## about -4 to 4
          pti[j] <- pti[j] + runif(npr,-.0001,.0001)
          ## For each orig. missing xf[,i] impute with non-missing xf[,i] that
          ## has closest predicted transformed value
          j[whichClosest(pti[j], pti[nai])]  ## see Misc.s
        } else j[whichClosePW(pti[j], pti[nai], f=fweighted)]
        impi <- xf[whichclose,i]
      } else {
        ## residuals off of transformed predicted values
        res <- f$ty - pti[s]
        ## predicted transformed target var + random sample of res,
        ## for NAs
        ptir <- pti[nai] +
          sample(res, length(nai),
                 replace=length(nai) > length(res))
        ## predicted random draws on untransformed scale
        impi <- approxExtrap(f$ty, f$y, xout=ptir)$y
      }
      xf[nai,i] <- impi
      if(iter > 3) imp[[nam[i]]][,iter-3] <- impi
    }
  }
  if(pr) cat('\n')

if(!x) xf <- NULL
structure(list(call=acall, formula=formula,
               method=method, match=match, fweighted=fweighted,
			   n=n, p=p, na=na, nna=nna,
			   linear=linear, categorical=categorical, monotone=mono,
			   cat.levels=cat.levels,
			   n.impute=n.impute, imputed=imp, x=xf, rsq=rsq),
          class='aregImpute')
}

print.aregImpute <- function(x, ...) {
  cat("\nMultiple Imputation using Bootstrap and PMM\n\n")
  dput(x$call)
  cat("\n")
  cat('\nMethod:',x$method,'\tn=',x$n,'\tp=',x$p,
      '\tImputations:',x$n.impute,'\n')
  cat('\nNumber of NAs:\n'); print(x$nna)
  if(length(x$linear))      cat('\nLinear:\t',x$linear,'\n')
  if(length(x$categorical)) cat('\nCategorical:\t',x$categorical,'\n')
  if(length(x$monotone))    cat('\nMonotonic:\t',  x$monotone,'\n')
  cat('\nR-squares for Predicting Non-Missing Values for Each Variable\nUsing Last Imputations of Predictors\n')
  print(round(x$rsq,3))
  invisible()
}

plot.aregImpute <- function(x, nclass=NULL, type=c('ecdf','hist'),
                            diagnostics=FALSE, maxn=10, ...) {
  type <- match.arg(type)
  i <- x$imputed
  catg <- x$categorical
  lev  <- x$cat.levels
  n.impute <- x$n.impute
  for(n in names(i)) {
    xi <- i[[n]]
    if(!length(xi)) next
    if(diagnostics) {
      r <- range(xi)
      cat(min(maxn,nrow(xi)))
      for(j in 1:min(maxn,nrow(xi))) {
        plot(1:n.impute, xi[j,], ylim=r, xlab='Imputation',
             ylab=paste("Imputations for Obs.",j,"of",n))
      }
    }
    ix <- as.vector(i[[n]])
    lab <- paste('Imputed',n)
    if(n %in% catg) {
      tab <- table(ix)
      mar <- par('mar')
      dotchart2(tab, lev[[n]], auxdata=tab, xlab='Frequency',
                ylab=lab)
      par(mar=mar)
    } else {
      if(type=='ecdf')
        ecdf(ix, xlab=lab, datadensity='hist', subtitles=FALSE) else {
        if(length(nclass)) hist(ix, xlab=n, nclass=nclass, main='') else
        hist(ix, xlab=lab, main='')
        scat1d(ix)
      }
    }
  }
  invisible()
}


