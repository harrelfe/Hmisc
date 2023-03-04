curveRep <- function(x, y, id, kn=5, kxdist=5, k=5, p=5, force1=TRUE,
                     metric=c('euclidean','manhattan'),
                     smooth=FALSE, extrap=FALSE, pr=FALSE) {
  metric <- match.arg(metric)
  
  id <- as.character(id)
  omit <- is.na(x + y)
  missfreq <- NULL; nomit <- sum(omit)
  if(nomit) {
    m <- tapply(omit, id, sum)
    missfreq <- table(m)
    x <- x[!omit]; y <- y[!omit]; id <- id[!omit]
  }
  n <- length(x)
  ns <- table(id)
  nunique <- length(unique(ns))

  if(nunique==1 || nunique <= kn) ncuts <- c(sort(unique(ns)),Inf) else {
    grouped.n <- cut2(ns, g=kn)
    ncuts <- cut2(ns, g=kn, onlycuts=TRUE)
    if(force1 && ncuts[2] > 1 && min(ns)==1)
      ncuts <- sort(unique(c(1:2, ncuts)))
  }
  nlev <- length(ncuts)-1
  res <- vector('list', nlev)
  names(res) <- as.character(ncuts[-length(ncuts)])

  clust <- function(x, k)
    if(diff(range(x))==0 || NROW(x) < k+1) rep(1, NROW(x)) else
    clara(x, k, metric=metric)$clustering

  interp <- if(extrap)
    function(x, y=NULL, xout) approxExtrap(x, y, xout=xout)$y else
    function(x, y=NULL, xout) approx(x, y, xout=xout, rule=2)$y

  ## Cluster by sample size first
  if(pr) cat('Creating',nlev,'sample size groups\n\n')
  for(i in 1:nlev) {
    ## Get list of curve ids in this sample size group
    if(i==nlev) {
      below <- ns <= ncuts[i+1]
      brack <- ']'
    } else {
      below <- ns < ncuts[i+1]
      brack <- ')'
    }
    ids <- names(ns)[ns >= ncuts[i] & below]
    if(pr) cat('Processing sample size [',ncuts[i],',',ncuts[i+1],
               brack,' containing ', length(ids),' curves\n',sep='')
    if(length(ids) < kxdist) res[[i]] <- list(ids) else {
      ## Cluster by distribution of x within sample size group
      ## Summarize these ids by clustering on range of x,
      ## plus the largest gap if minimum sample size > 2
      ## Use only the x position is min sample size is 1
      s <- id %in% ids
      ssize <- min(tapply(x[s], id[s], function(w) length(unique(w))))
      z <- tapply((1:n)[s], id[s],
                  function(j) if(ssize==1) x[j][1] else
                  if(ssize==2) range(x[j]) else
                  c(range(x[j]),max(diff(sort(x[j])))))
      z <- matrix(unlist(z), nrow=length(z), byrow=TRUE)
      if(kxdist > nrow(z) - 1)
        stop('number of curves to cluster must be >= kxdist+1')
      distclusters <- clust(z, kxdist)
      if(pr) {
        cat(' Number of curves in each x-dist cluster:\n')
        print(table(distclusters))
      }
      resi <- list()
      ## Within x distribution and within sample size interval,
      ## cluster on linearly interpolated y at p equally spaced x points
      ## unless <2 unique x-points for some curve
      for(clus in 1:max(distclusters)) {
        idc <- ids[distclusters==clus]
        if(pr) cat(' Processing x-distribution group', clus,
                   'containing', length(idc),'curves\n')
        s <- id %in% idc
        ssize <- min(tapply(x[s], id[s], function(w) length(unique(w))))
        if(ssize > 1) {
          xrange <- range(x[s])
          xseq <- seq(xrange[1], xrange[2], length.out=p)
        }
        g <- if(ssize==1) function(j) c(mean(x[j]), mean(y[j])) else
         if(smooth && ssize > 2)
           function(j) interp(clowess(x[j],y[j]), xout=xseq) else
           function(j) interp(x[j], y[j], xout=xseq)
        
        z <- tapply((1:n)[s], id[s], g)
        z <- matrix(unlist(z), nrow=length(idc), byrow=TRUE)
        yclusters <- clust(z, min(k, max(length(idc)-2,1)))
        names(yclusters) <- idc
        resi[[clus]] <- yclusters
      }
      res[[i]] <- resi
    }
  }
  structure(list(res=res, ns=table(ns), nomit=nomit, missfreq=missfreq,
                 ncuts=ncuts, kn=kn, kxdist=kxdist, k=k, p=p,
                 smooth=smooth, x=x, y=y, id=id),
            class='curveRep')
}

print.curveRep <- function(x, ...) {
  sm <- if(x$smooth) 'smooth' else 'not smoothed'
  ncuts <- x$ncuts
  cat('kn:',x$kn, ' kxdist:',x$kxdist, ' k:',x$k,
      ' p:',x$p, ' ', sm, '\n\n', sep='')
  cat('Frequencies of number of non-missing values per curve:\n')
  print(x$ns)
  if(length(x$missfreq)) {
    cat(x$nomit, 'missing values excluded.\n\n')
    cat('\nFrequency of number of missing values per curve:\n')
    print(x$missfreq)
  }
  cat('\nSample size cuts:', paste(ncuts, collapse=' '),'\n')
  cat('Number of x distribution groups per sample size group:',
      paste(sapply(x$res, length), collapse=' '),'\n\n')
  res <- x$res
  ng <- length(res)
  for(i in 1:ng) {
    ngroup <- res[[i]]
    maxclus <- max(unlist(ngroup))
    w <- matrix(NA, nrow=maxclus, ncol=length(ngroup),
                dimnames=list(paste('Cluster',1:maxclus),
                  paste('x-Dist', 1:length(ngroup))))
    j <- 0
    for(xdistgroup in ngroup) {
      j <- j+1
      w[,j] <- tabulate(xdistgroup, nbins=maxclus)
    }
    brack <- if(i==ng) ']' else ')'
    z <- if(is.infinite(ncuts[i+1])) ncuts[i] else
    paste('[', ncuts[i], ',', ncuts[i+1], brack, sep='')
    cat('\nNumber of Curves for Sample Size ', z, '\n',sep='')
    print(w)
  }
  invisible()
}

plot.curveRep <- function(x, which=1:length(res),
                          method=c('all','lattice','data'),
                          m=NULL, probs=c(.5,.25,.75),
                          nx=NULL, fill=TRUE,
                          idcol=NULL, freq=NULL, plotfreq=FALSE,
                          xlim=range(x), ylim=range(y),
                          xlab='x', ylab='y', colorfreq=FALSE, ...) {
  method <- match.arg(method)
  retdat <- FALSE
  if(method == 'data') {
    retdat <- TRUE
    method <- 'lattice'
    sRequire('lattice')
    }
  ncuts <- x$ncuts
  res <- x$res; id <- x$id; y <- x$y; k <- x$k; x <- x$x
  nng <- length(res)

  samp <- function(ids)
    if(!length(m) || is.character(m) ||
       length(ids) <= m) ids else sample(ids, m)
  if(is.character(m) &&
     (m != 'quantiles' || method != 'lattice'))
    stop('improper value of m')
  
  if(method=='lattice') {
    if(length(which) != 1)
      stop('must specify one n range to plot for method="lattice" or "data"')
    nres <- names(res)
    nname <- if(length(nres)==1) NULL else
      if(nres[which]=='1' & nres[which+1]=='2') 'n=1' else {
        brack <- if(which==length(nres)) ']' else ')'
        z <- if(is.infinite(ncuts[which+1])) ncuts[which] else
        paste('[',ncuts[which],',',ncuts[which+1],brack,sep='')
        paste('n ',z, sep='')
      }
    
    res <- res[[which]]
    n <- length(x)
    X <- Y <- xdist <- cluster <- sizecluster <- numeric(n)
    curve <- character(n)
    if(length(freq)) {
      unique.cats <- unique(freq)
      Freqtab <- matrix(0, nrow=n, length(unique.cats),
                        dimnames=list(NULL, unique.cats))
    }
    st <- 1
    for(jx in 1:length(res)) {
      xgroup  <- res[[jx]]
      ids <- names(xgroup)
      for(jclus in 1:max(xgroup)) {
        all.ids.in.cluster <- ids[xgroup==jclus]
        if(length(freq)) {
          freqtab <- table(freq[all.ids.in.cluster])
          nfreqtab <- names(freqtab)
        }
        plotted.ids.in.cluster <- samp(all.ids.in.cluster)
        for(cur in plotted.ids.in.cluster) {
          s <- id %in% cur
          np <- sum(s)
          i <- order(x[s])
          en <- st+np-1
          if(en > n) stop('program logic error 1')
          X[st:en]       <- x[s][i]
          Y[st:en]       <- y[s][i]
          xdist[st:en]   <- jx
          cluster[st:en] <- jclus
          curve[st:en]   <- cur
          sizecluster[st:en] <- sum(xgroup==jclus)
          if(length(freq)) Freqtab[st:en, nfreqtab] <- rep(freqtab, each=np)
          st <- st+np
        }
      }
    }
    Y <- Y[1:en]; X <- X[1:en]
    distribution <- xdist[1:en]; cluster <- cluster[1:en]
    curve <- curve[1:en]; sizecluster <- sizecluster[1:en]
    if(length(freq)) Freqtab <- Freqtab[1:en,,drop=FALSE]
    textfun <- function(subscripts, groups=NULL) {
      if(!length(subscripts)) return()
      txt <- if(length(freq) && length(groups)) {
        tab <- Freqtab[subscripts[1],]
        if(plotfreq) {
          mx <- max(Freqtab, na.rm=TRUE)
          f <- mx/(.1*plotfreq)
          y <- 1
          fnam <- names(tab)
          long <- fnam[nchar(fnam)==max(nchar(fnam))][1]
          lx <- convertX(unit(1, 'strwidth', long), 'npc', valueOnly=TRUE)
          for(i in 1:length(tab)) {
            y <- y - .075
            grid.text(fnam[i], x=lx-.005, y=y+.025, just=c(1,.5),
                      gp=gpar(fontsize=7, col=gray(.4)))
            if(tab[i] > 0)
              grid.polygon(x=c(lx, lx+tab[i]/f, lx+tab[i]/f, lx, lx),
                           y=c(y, y, y+.05, y+.05, y), 
                           gp=gpar(fill=gray(.7), col=gray(.7)))
            if(tab[i]==mx)
              grid.text(mx, x=lx+mx/f + .01, y=y+.025,
                        just=c(0,.5), gp=gpar(fontsize=7, col=gray(.4)))
          }
          return()
        }
        txt <- paste(names(tab), tab, sep=':')
        txt2 <- txt
        paste(txt, collapse=';')
      } else {
        size <- sizecluster[subscripts[1]]
        paste('N=',size,sep='')
      }
      if (!colorfreq | is.null(idcol) | is.null(freq))  #Do same as original
      {
        grid.text(txt, x = 0.005, y = 0.99, just = c(0, 1), 
                  gp = gpar(fontsize = 9, col = gray(0.25)))
      }
      else  #color freq text using idcol
      {
        mycolors<-data.frame(idcol,freq)
        mycolors<-unique(mycolors)
        curtext<-txt2[1]
        curtrt<-strsplit(curtext,':')[[1]][1]
        curcol<-as.character( mycolors[ curtrt==as.character( mycolors[,2]) ,1] )
        
        grid.text(curtext, 
                  x = 0.005, 
                  y = 0.99, 
                  just = c(0, 1), 
                  gp = gpar(fontsize = 9, col =curcol)
        )
        emspace<-2*strwidth('M', units="figure")
        for (i in 2:length(txt2))
        {
          curtext<-txt2[i]
          curtrt<-strsplit(curtext,':')[[1]][1]
          curcol<-as.character( mycolors[ curtrt==as.character( mycolors[,2]) ,1] )
          
          grid.text(curtext, 
                    x = emspace+strwidth(txt2[i-1], units="figure") ,
                    y = 0.99, just = c(0, 1), 
                    gp = gpar(fontsize = 9, col = curcol)
          )
        }
      }
    }
    pan <- if(length(idcol))
      function(x, y, subscripts, groups, type, ...) {
        groups <- as.factor(groups)[subscripts]
        textfun(subscripts, groups)
        for(g in levels(groups)) {
          idx <- groups == g
          xx <- x[idx]; yy <- y[idx]; ccols <- idcol[g]
          if (any(idx)) { 
            switch(type, 
                   p = lattice::lpoints(xx, yy, col = ccols), 
                   l = lattice::llines(xx, yy, col = ccols), 
                   b = { lattice::lpoints(xx, yy, col = ccols) 
                         lattice::llines(xx, yy, col = ccols) }) 
          } 
        } 
      } else function(x, y, subscripts, groups, ...) {
        lattice::panel.superpose(x, y, subscripts, groups, ...)
        textfun(subscripts, groups)
      }
    if(retdat) return(data.frame(x=X, y=Y, distribution, cluster,
                                 curve=curve, ninterval=nname))
    
    if(is.character(m))
      print(xYplot(Y ~ X | distribution*cluster,
                   method='quantiles', probs=probs, nx=nx,
                   xlab=xlab, ylab=ylab,
                   xlim=xlim, ylim=ylim,
                   main=nname, as.table=TRUE,
                   panel=function(x, y, subscripts, ...) {
                     if(length(subscripts)) {
                       panel.xYplot(x, y, subscripts, ...)
                       textfun(subscripts)
                     }
                     })) else
    print(lattice::xyplot(Y ~ X | distribution*cluster, groups=curve,
                 xlab=xlab, ylab=ylab,
                 xlim=xlim, ylim=ylim,
                 type=if(nres[which]=='1')'b' else 'l',
                 main=nname, panel=pan, as.table=TRUE))
    return(invisible())
  }

  for(jn in which) {
    ngroup <- res[[jn]]
    for(jx in 1:length(ngroup)) {
      xgroup <- ngroup[[jx]]
      ids <- names(xgroup)
      for(jclus in 1:max(xgroup)) {
        rids <- ids[xgroup==jclus]
        nc <- length(rids)
        ids.in.cluster <- samp(rids)
        for(curve in 1:length(ids.in.cluster)) {
          s <- id %in% ids.in.cluster[curve]
          i <- order(x[s])
          type <- if(length(unique(x[s]))==1)'b' else 'l'
          if(curve==1) {
            plot(x[s][i], y[s][i], xlab=xlab, ylab=ylab,
                 type='n', xlim=xlim, ylim=ylim)
            brack <- if(jn==nng) ']' else ')'
            z <- if(is.infinite(ncuts[jn+1])) ncuts[jn] else
            paste('[', ncuts[jn],',',ncuts[jn+1],brack,sep='')
            title(paste('n ', z, ' x=',jx,
                        ' c=',jclus,' ',nc,' curves', sep=''), cex=.5)
          }
          lines(x[s][i], y[s][i], type=type,
                col=if(length(idcol))
                 idcol[ids.in.cluster[curve]] else curve)
        }
      }
      if(fill && max(xgroup) < k)
        for(i in 1:(k - max(xgroup)))
          plot(0, 0, type='n', axes=FALSE, xlab='', ylab='')
    }
  }
}

curveSmooth <- function(x, y, id, p=NULL, pr=TRUE) {
  omit <- is.na(x + y)
  if(any(omit)) {
    x <- x[!omit]; y <- y[!omit]; id <- id[!omit]
  }
  uid <- unique(id)
  m <- length(uid)
  pp <- length(p)
  if(pp) {
    X <- Y <- numeric(p*m)
    Id <- rep(id, length.out=p*m)
  }
  st <- 1
  en <- 0
  ncurve <- 0
  for(j in uid) {
    if(pr) {
      ncurve <- ncurve + 1
      if((ncurve %% 50) == 0) cat(ncurve,'')
    }
    s <- id==j
    xs <- x[s]
    ys <- y[s]
    if(length(unique(xs)) < 3) {
      if(pp) {
        en <- st + length(xs) - 1
        X[st:en] <- xs
        Y[st:en] <- ys
        Id[st:en] <- j
      }
    } else {
      if(pp) {
        uxs <- sort(unique(xs))
        xseq <- if(length(uxs) < p) uxs else
        seq(min(uxs), max(uxs), length.out=p)
        ye <- approx(clowess(xs, ys), xout=xseq)$y
        n <- length(xseq)
        en <- st + n - 1
        X[st:en] <- xseq
        Y[st:en] <- ye
        Id[st:en] <- j
      } else y[s] <- approx(clowess(xs, ys), xout=xs)$y
    }
    st <- en + 1
  }
  if(pr) cat('\n')
  if(pp) {
    X <- X[1:en]
    Y <- Y[1:en]
    Id <- Id[1:en]
    list(x=X, y=Y, id=Id)
  } else list(x=x, y=y, id=id)
}
