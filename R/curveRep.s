## $Id$
curveRep <- function(x, y, id, kn=5, kxdist=5, k=5, p=5, force1=TRUE,
                     metric=c('euclidean','manhattan')) {
  require(cluster)
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

  if(nunique==1 || nunique <= kn) {
    cuts <- sort(unique(ns))
    ncuts <- c(cuts, Inf)
  } else {
    grouped.n <- cut2(ns, g=kn)
    cuts <- cut2(ns, g=kn, onlycuts=TRUE)
    if(force1 && cuts[1] > 1 && min(ns)==1)
      cuts <- sort(unique(c(1:2, cuts)))
    ncuts <- c(unique(c(min(ns), cuts)), Inf)
  }
  nlev <- length(ncuts)-1
  res <- vector('list', nlev); names(res) <- as.character(cuts)

  clust <- function(x, k) clara(x, k, metric=metric)$clustering

  ## Cluster by sample size first
  for(i in 1:nlev) {
    ## Get list of curve ids in this sample size group
    ids <- names(ns)[ns >= ncuts[i] & ns < ncuts[i+1]]
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
      resi <- list()
      ## Within x distribution and within sample size interval,
      ## cluster on linearly interpolated y at p equally spaced x points
      ## unless <2 unique x-points for some curve
      for(clus in 1:max(distclusters)) {
        idc <- ids[distclusters==clus]
        s <- id %in% idc
        ssize <- min(tapply(x[s], id[s], function(w) length(unique(w))))
        xrange <- range(x[s])
        z <- tapply((1:n)[s], id[s],
                    function(j) if(ssize==1) c(mean(x[j]), mean(y[j]))
                    else
                    approx(x[j], y[j],
                           xout=seq(xrange[1],xrange[2],
                             length.out=p), rule=2)$y)
        z <- matrix(unlist(z), nrow=length(idc), byrow=TRUE)
        yclusters <- clust(z, min(k, max(length(idc)-2,1)))
        names(yclusters) <- idc
        resi[[clus]] <- yclusters
      }
      res[[i]] <- resi
    }
  }
  structure(list(res=res, ns=table(ns), nomit=nomit, missfreq=missfreq,
                 ncuts=ncuts[-length(ncuts)], kn=kn, kxdist=kxdist, k=k, p=p,
                 x=x, y=y, id=id),
            class='curveRep')
}

print.curveRep <- function(x, ...) {
  cat('kn:',x$kn, ' kxdist:',x$kxdist, ' k:',x$k, ' p:',x$p, '\n\n', sep='')
  cat('Frequencies of number of non-missing values per curve:\n')
  print(x$ns)
  if(length(x$missfreq)) {
    cat(x$nomit, 'missing values excluded.\n\n')
    cat('\nFrequency of number of missing values per curve:\n')
    print(x$missfreq)
  }
  cat('\nSample size cuts:', paste(x$ncuts, collapse=' '),'\n')
  cat('Number of x distribution groups per sample size group:',
      paste(sapply(x$res, length), collapse=' '),'\n\n')
  invisible()
}

plot.curveRep <- function(x, which=1:length(res),
                          method=c('all','lattice'),
                          m=NULL, probs=c(.5,.25,.75),
                          nx=NULL, fill=TRUE,
                          xlim=range(x), ylim=range(y),
                          xlab='x', ylab='y') {
  method <- match.arg(method)
  res <- x$res; id <- x$id; y <- x$y; k <- x$k; x <- x$x
  nm <- names(res)

  samp <- function(ids)
    if(!length(m) || is.character(m) ||
       length(ids) <= m) ids else sample(ids, m)
  if(is.character(m) &&
     (m != 'quantiles' || method != 'lattice'))
    stop('improper value of m')
  
  if(method=='lattice') {
    if(length(which) != 1)
      stop('must specify one n range to plot for method="lattice"')
    require(lattice)
    nres <- names(res)
    if(length(nres)==1) nname <- NULL else
    nname <- if(which==length(nres))
      paste('n>=',nres[which],sep='') else
      if(nres[which]=='1' & nres[which+1]=='2') 'n=1' else
      paste(nres[which],'<=n<',nres[which+1],sep='')
    
    res <- res[[which]]
    n <- length(x)
    X <- Y <- xdist <- cluster <- curve <- numeric(n)
    st <- 1
    for(jx in 1:length(res)) {
      xgroup  <- res[[jx]]
      ids <- names(xgroup)
      for(jclus in 1:max(xgroup)) {
        ids.in.cluster <- samp(ids[xgroup==jclus])
        for(cur in 1:length(ids.in.cluster)) {
          s <- id %in% ids.in.cluster[cur]
          np <- sum(s)
          i <- order(x[s])
          en <- st+np-1
          if(en > n) stop('program logic error 1')
          X[st:en]       <- x[s][i]
          Y[st:en]       <- y[s][i]
          xdist[st:en]   <- jx
          cluster[st:en] <- jclus
          curve[st:en]   <- cur
          st <- st+np
        }
      }
    }
    Y <- Y[1:en]; X <- X[1:en]
    distribution <- xdist[1:en]; cluster <- cluster[1:en]
    curve <- curve[1:en]
    if(is.character(m))
      print(xYplot(Y ~ X | distribution*cluster,
                   method='quantiles', probs=probs, nx=nx,
                   xlab=xlab, ylab=ylab,
                   xlim=xlim, ylim=ylim,
                   main=nname)) else
    print(xyplot(Y ~ X | distribution*cluster, groups=curve,
                 xlab=xlab, ylab=ylab,
                 xlim=xlim, ylim=ylim,
                 type=if(nres[which]=='1')'b' else 'l',
                 main=nname, panel=panel.superpose))
    return(invisible())
  }

  for(jn in which) {
    ngroup <- res[[jn]]
    for(jx in 1:length(ngroup)) {
      xgroup <- ngroup[[jx]]
      ids <- names(xgroup)
      for(jclus in 1:max(xgroup)) {
        ids.in.cluster <- samp(ids[xgroup==jclus])
        for(curve in 1:length(ids.in.cluster)) {
          s <- id %in% ids.in.cluster[curve]
          i <- order(x[s])
          type <- if(length(unique(x[s]))==1)'b' else 'l'
          if(curve==1) {
            plot(x[s][i], y[s][i], xlab=xlab, ylab=ylab,
                 type='n', xlim=xlim, ylim=ylim)
            title(paste('n=',nm[jn],' x=',jx,
                        ' c=',jclus,sep=''), cex=.5)
          }
          lines(x[s][i], y[s][i], type=type, col=curve)
        }
      }
      if(fill && max(xgroup) < k)
        for(i in 1:(k - max(xgroup)))
          plot(0, 0, type='n', axes=FALSE, xlab='', ylab='')
    }
  }
}
