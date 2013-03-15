panel.bpplot <- function(x, y, box.ratio = 1, means=TRUE, qref=c(.5,.25,.75),
                         probs= c(.05,.125,.25,.375), nout=0,
                         datadensity=FALSE, scat1d.opts=NULL,
                         font = box.dot$font, pch = box.dot$pch, 
                         cex  = box.dot$cex, col = box.dot$col, ...)
{
  require(lattice)

  grid <- TRUE
  if(grid) {
    lines <- llines;
    points <- lpoints;
    segments <- lsegments
  }

  y <- as.numeric(y)
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]
  y.unique <-  sort(unique(y))
  width <- box.ratio/(1 + box.ratio)
  w <- width/2
  probs2 <- sort(c(probs,1-probs))

  box.dot  <- trellis.par.get("box.dot")
  lineopts <- trellis.par.get("box.rectangle")
  box.dot.par <- c(list(pch = pch, cex = cex, col = col, font = font), ...)

  m  <- length(probs)
  m2 <- length(probs2)
  j <- c(1,sort(rep(2:m2,2)),-sort(-rep(1:(m2-1),2)))
  z <- c(sort(rep(probs,2)),-sort(-rep(probs[1:(m-1)],2)))
  z <- c(z, -z, probs[1])
  k <- max(z)
  k <- if(k > .48) .5 else k
  
  if(length(qref)) {
    size.qref <- pmin(qref, 1-qref)
    size.qref[qref==.5] <- k
  }
  
  for(Y in y.unique) {
    X <- x[y == Y]
    if(!length(X))
      next
    
    q <- quantile(X, c(probs2,qref))
    if(length(qref)) 
      do.call('segments',c(list(q[-(1:m2)],      Y-w*size.qref/k,
                                q[-(1:m2)], 	 Y+w*size.qref/k),
                           lineopts))
    
    do.call('lines',c(list(x=q[j], y=Y + w*z/k), lineopts))
    if(means) {
      mean.value <- list(x=mean(X), y=Y)
      do.call('points', c(mean.value, box.dot.par))
    }

    if(datadensity)
      do.call('scat1d',c(list(x=X,y=Y,grid=grid), scat1d.opts))

    if(nout>0) {
      ii <- if(nout < 1) {
        ## Note - bug in quantile - endless loop if probs=c(.5,.5)
        if(nout==.5)
          stop('instead of nout=.5 use datadensity=T')

        cuts <- quantile(X, c(nout,1-nout))
        X < cuts[1] | X > cuts[2]
      } else {
        X <- sort(X)
        nx <- length(X)
        ll <- 1:nx
        (ll <= min(nout,nx/2)) | (ll >= max(nx-nout+1,nx/2))
      }
      
      if(sum(ii))
        do.call('scat1d',c(list(x=X[ii],y=Y,grid=grid), scat1d.opts))
    }
  }
}


# Given a matrix where rows are groups and columns have all the
# quantiles already computed, plus the Mean, draw a panel containing
# horizontal box-percentile plots like the default in panel.bpplot.  This is
# primarily for plot.summary.formula.reverse's continuous variable
# plots
bpplt <- function(stats, xlim, xlab='', box.ratio = 1, means=TRUE,
                  qref=c(.5,.25,.75), qomit=c(.025,.975),
                  pch=16, cex.labels=par('cex'),
                  cex.points=if(prototype) 1 else .5,
                  grid=FALSE)
{
  prototype <- missing(stats)
  if(prototype) {
    x <- c(.025,.05,.125,.25,.375,.5,.625,.75,.875,.95,.975)
    stats <- matrix(x, nrow=1, dimnames=list('',format(x)))
    Means <- .56
  } else {
    Means <- stats[,'Mean']
    stats <- stats[,dimnames(stats)[[2]] %nin% c('Mean','SD','N'),drop=FALSE]
  }

  stats <- stats[,order(as.numeric(dimnames(stats)[[2]])), drop=FALSE]

  groups <- dimnames(stats)[[1]]
  qq <- as.numeric(dimnames(stats)[[2]])
  probs2 <- qq
  if(missing(xlim))
    xlim <- range(stats)
  
  i <- integer(0)
  for(a in c(.5,qomit))
    i <- c(i, seq.int(along.with=probs2)[abs(probs2-a) < .001])
  
  probs2 <- probs2[-i]
  probs  <- probs2[seq.int(length.out=floor(length(probs2)/2))]

  if(grid) {
    lines <- llines;
    points <- lpoints;
    segments <- lsegments
  }

  width <- box.ratio/(1 + box.ratio)
  w <- width/2

  m  <- length(probs)
  m2 <- length(probs2)
  j <- c(1, rep(seq.int(along.with = probs2[c(-1, -m2)]) + 1, each=2), m2)
  j <- c(j, rev(j), NA)


  z <- c(rep(probs[-m], each=2), probs[m])
  z <- c(z, rev(z))
  z <- c(z, -z, NA)
  k <- max(z, na.rm=TRUE)
  k <- if(k > .48) .5 else k
  
  if(length(qref)) {
    size.qref <- pmin(qref, 1-qref)
    size.qref[qref==.5] <- k
  }

  plot.new()

  mai <- par('mai')
  mxlab <- .3+max(strwidth(groups, units='inches',cex=cex.labels))

  mai[2] <- mxlab
  opar <- par(mai=mai)
  on.exit(par(opar))

  plot.window(xlim=xlim, ylim=c(0.5,length(groups)+0.5))

  if(!prototype) {
    box()
    mgp.axis(1, axistitle=xlab)
  }
  
  mtext(paste(groups,''), 2, 0, at=length(groups):1,
        adj=1, las=1, cex=cex.labels)

  y <- seq.int(from=length(groups), to=1, length.out=length(groups))

  qref.x <- as.vector(stats[,match(qref,qq)])
  qref.y <- rep.int(y, times=length(size.qref))
  qref.mod <- rep(w*size.qref/k, each=length(groups))
  segments(x0=qref.x, y0=qref.y-qref.mod,
           x1=qref.x, y1=qref.y+qref.mod)

  polygon(x=as.vector(t(stats[,match(probs2,qq)[j]])),
          y=rep(y, each=length(j)) + w*z/k)

  if(means)
    points(Means, y, pch=pch, cex=cex.points)
  
  if(prototype) {
    mar <- par('mar')
    on.exit(par(mar=mar))
    par(mar=rep(.5,4))
    text(Means, 1.025+.02, 'Mean')
    for(a in c(.5,probs2)) {
      arrows(a, .6, a, .725, length=.1)
      f <- format(a)
      text(a, .575, format(a))
    }
    
    text(.5, .52, 'Quantiles')
    xd <- .004
    text(.485-xd, 1,
         expression(Median==Q[2]),
         srt=90)
    
    text(.235-xd, 1,
         expression(Q[1]),
         srt=90)
    
    text(.735-xd, 1,
         expression(Q[3]),
         srt=90)
    
    lines(c(.375,.625), rep(1.3,2));
    text(.635, 1.3,  '1/4', adj=0, cex=.9)
    
    lines(c(.25, .75 ), rep(1.35,2));
    text(.76,  1.35, '1/2', adj=0, cex=.9)
    
    lines(c(.125,.875), rep(1.4,2));
    text(.885, 1.4,  '3/4', adj=0, cex=.9)
    
    lines(c(.05, .95),  rep(1.45,2));
    text(.96,  1.45, '9/10',adj=0, cex=.9)
    
    text(.68, 1.24, 'Fraction of Sample Covered', adj=0, srt=13, cex=.7)
  }
}
