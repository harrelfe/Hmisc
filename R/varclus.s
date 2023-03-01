varclus <-
  function(x,
           similarity=c("spearman","pearson","hoeffding",
                        "bothpos","ccbothpos"), 
           type=c("data.matrix","similarity.matrix"),
           method="complete",
           data=NULL, subset=NULL, na.action=na.retain,
           trans=c("square", "abs", "none"),
           ...)
{
  call <- match.call()
  type <- match.arg(type)
  if(type != "similarity.matrix") similarity <- match.arg(similarity)
  trans <- match.arg(trans)
  
  nact <- NULL

  if(inherits(x,"formula")) {
    form <- x
    oldops <- options(contrasts=c("contr.treatment","contr.poly"))
    if(length(list(...))) data <- dataframeReduce(data, ...)
    x <- list(formula=form, data=data, na.action=na.action, subset=subset)
    x <- do.call('model.frame', x)
    nam <- names(x)
    nv <- length(x)
    Terms <- attr(x,'terms')
    
    nact <- attr(x,"na.action")
    x <- model.matrix(Terms, x)
    if(dimnames(x)[[2]][1]=='(Intercept)') x <- x[,-1]
    form <- TRUE
    options(oldops)
    type <- "data.matrix"
  }
  else form <- FALSE
  
  n <- NULL
  if(mode(x) != "numeric") stop("x matrix must be numeric")

  if(type == "data.matrix") { ## assume not a correlation matrix
      if(similarity %in% c("bothpos","ccbothpos")) {
        isthere <- 1*(! is.na(x))
        x[is.na(x)] <- 0
        x[x > 0] <- 1
        n <- crossprod(isthere)
        x <- crossprod(x)/n
        if(similarity=='ccbothpos') {
          cc <- diag(x) %*% t(diag(x))
          cc[row(cc)==col(cc)] <- 0
          x <- x - cc
        }
      }
      else if(similarity=="hoeffding") {
        D <- hoeffd(x); x <- D$D; n <- D$n 
      }
      else {
        D <- rcorr(x, type=similarity)
        x <- D$r
        x <- switch(trans,
                    square = x^2,
                    abs    = abs(x),
                    none   = x)
        n <- D$n
      }
    }
  else if(diff(dim(x)) != 0) 
    stop("x must be square to be a similarity matrix")
  
  if(any(is.na(x))) {
    cat("Part of the similarity matrix could not be computed:\n")
    x[x < .01] <- 0
    print(x, digits=2)
    stop()
  }
  
  w <- if(similarity=='ccbothpos') NULL
  else hclust(as.dist(1-x), method=method)
  
  structure(list(call=call, sim=x, n=n, hclust=w, similarity=similarity,
                 trans=trans, method=method, na.action=nact),
            class="varclus")
}


print.varclus <- function(x, abbrev=FALSE, ...)
{
  dput(x$call); cat("\n")
  if(length(x$na.action))
    naprint(x$na.action)
  trans <- x$trans
  s <- c(hoeffding="30 * Hoeffding D",
         spearman=switch(trans,
           square = "Spearman rho^2",
           abs    = "|Spearman rho|",
           none   = "Spearman rho"),
         pearson=switch(trans,
           square = "Pearson r^2",
           abs    = "|Pearson r|",
           none   = "Pearson r"),
         bothpos="Proportion",
         ccbothpos="Chance-Corrected Proportion")[x$similarity]
  cat("\nSimilarity matrix (",s,")\n\n",sep="")
  k <- x$sim
  lab <- dimnames(k)[[2]]
  if(abbrev)
    lab <- abbreviate(lab)

  dimnames(k) <- list(lab,lab)
  print.default(round(k, 2))
  n <- x$n
  if(length(n)) {
    if(length(n) == 1)
      cat("\nNo. of observations used=", n,"\n\n")
    else {
      cat("\nNo. of observations used for each pair:\n\n")
      dimnames(n) <- list(lab,lab)
      print(n)
    }
  }
  
  cat("\nhclust results (method=",x$method,")\n\n",sep="")
  print(x$hclust)
  invisible()
}

plot.varclus <- function(x, ylab, abbrev=FALSE, legend.=FALSE, loc, maxlen=20,
                         labels=NULL, ...)
{
  trans <- x$trans
  if(missing(ylab)) {
    s <- c(hoeffding="30 * Hoeffding D",
           spearman=switch(trans,
             square = expression(paste(Spearman,~rho^2)),
             abs    = expression(paste(Spearman,~abs(rho))),
             none   = expression(paste(Spearman,~rho))),
           pearson=switch(trans,
             square = expression(paste(Pearson,~r^2)),
             abs    = expression(paste(Pearson,~abs(r))),
             none   = expression(paste(Pearson,~r))),
           bothpos="Proportion",
           ccbothpos="Chance-Corrected Proportion")[x$similarity]
    if((is.expression(s) && as.character(s)=='NULL') ||
       (! is.expression(s) && (is.na(s) || s=='')))
      s <- x$similarity
    ylab <- s
  }
  
  if(legend.) abbrev <- TRUE
  
  if(! length(labels)) labels <- dimnames(x$sim)[[2]]
  
  olabels <- labels
  if(abbrev) labels <- abbreviate(labels)

  if(! length(x$hclust))
    stop('clustering was not done on similarity="ccbothpos"')

  plot(x$hclust, labels=labels, ann=FALSE, axes=FALSE, ...)
  ya <- pretty(range(1 - x$hclust$height))
  axis(2, at=1-ya, labels=format(ya))
  title(ylab=ylab)

  s <- labels != olabels
  if(legend. && any(s)) {
    if(missing(loc)) {
      cat("Click mouse at upper left corner of legend\n")
      loc <- locator(1)
    }
    
    olabels <- ifelse(nchar(olabels)>maxlen, substring(olabels,1,maxlen),
                      olabels)
    text(loc, paste(paste(labels[s],":",olabels[s],"\n"),
                    collapse=""), adj=0)
  }
  
  invisible()
}


na.retain <- function(mf) mf


naclus <- function(df, method="complete")
{
  ismiss <- function(x) if(is.character(x)) is.na(x) | x=='' else is.na(x) 

  na <- sapply(df, ismiss) * 1

  n <- nrow(na)
  sim <- crossprod(na) / n
  res <- varclus(sim, type="similarity.matrix", similarity="Fraction Missing",
                 method=method)
  na.per.obs <- apply(na, 1, sum)
  nc <- ncol(na)
  mean.na <- rep(NA, nc)
  names(mean.na) <- dimnames(na)[[2]]
  for(i in 1:nc) {
    y <- na[,i] == 1
    if(any(y)) mean.na[i] <- mean(na.per.obs[y]) - 1
    NULL
  }
  
  res$na.per.obs <- na.per.obs
  res$mean.na    <- mean.na
  res
}


naplot <- function(obj, which=c('all','na per var','na per obs','mean na',
                                'na per var vs mean na'),
                   ...)
{
  which <- match.arg(which)
  tab <- table(obj$na.per.obs)
  na.per.var <- diag(obj$sim)
  names(na.per.var) <- dimnames(obj$sim)[[2]]
  mean.na <- obj$mean.na

  if(which %in% c('all','na per var'))
    dotchart2(sort(na.per.var), xlab='Fraction of NAs', 
              main='Fraction of NAs in each Variable', ...)

  if(which %in% c('all','na per obs'))
    dotchart2(tab, auxdata=tab,
              xlab='Frequency', 
              main='Number of Missing Variables Per Observation', ...)

  if(which %in% c('all','mean na'))
    dotchart2(sort(mean.na), 
              xlab='Mean Number of NAs',
              main='Mean Number of Other Variables Missing for\nObservations where Indicated Variable is NA',
              ...)

  if(which %in% c('all','na per var vs mean na')) {
    xpd <- par('xpd')
    par(xpd=NA)
    on.exit(par(xpd=xpd))
    
    plot(na.per.var, mean.na, xlab='Fraction of NAs for Single Variable',
         ylab='Mean # Other Variables Missing', type='p')
    usr <- par('usr')
    eps <- .015*diff(usr[1:2]);
    epsy <- .015*diff(usr[3:4])
    
    s <- (1:length(na.per.var))[! is.na(mean.na)]
    taken.care.of <- NULL
    for(i in s) {
      if(i %in% taken.care.of)
        next
      
      w <- s[s > i & abs(na.per.var[s]-na.per.var[i]) < eps &
             abs(mean.na[s]-mean.na[i]) < epsy]
      if(any(w)) {
        taken.care.of <- c(taken.care.of, w)
        text(na.per.var[i]+eps, mean.na[i],
             paste(names(na.per.var[c(i,w)]),collapse='\n'),adj=0)
      }
      else text(na.per.var[i]+eps, mean.na[i], names(na.per.var)[i], adj=0)
    }
  }
  
  invisible(tab)
}

plotMultSim <- function(s, x=1:dim(s)[3],
                        slim=range(pretty(c(0,max(s,na.rm=TRUE)))),
                        slimds=FALSE,
                        add=FALSE, lty=par('lty'), col=par('col'),
                        lwd=par('lwd'), vname=NULL, h=.5, w=.75,
                        u=.05, labelx=TRUE, xspace=.35)
{
  if(! length(vname))
    vname <- dimnames(s)[[1]]
  p <- dim(s)[1]
  if(length(vname) != p) stop('wrong length for vname')
  
  if(p != dim(s)[2])
    stop('similarity matrix not square')
  
  if(length(x) != dim(s)[3])
    stop('length of x differs from extent of 3rd dimension of s')

  if(! add) {
    plot(c(-xspace,p+.5),c(.5,p+.25), type='n', axes=FALSE, xlab='',ylab='')
    if(labelx)
      text(1:p, rep(.6,p), vname, adj=.5)
    
    text(rep(.5,p), 1:p, vname, adj=1)
  }
  
  scaleit <- function(x, xlim, lim) lim[1] +
    (x - xlim[1]) / diff(xlim) * diff(lim)

  if(slimds) {
    slim.diag <- -1e10
    for(k in 1:length(x)) {
      sk <- s[,,k]
      r <- max(diag(sk))
      slim.diag <- max(slim.diag, r)
    }
    
    slim.diag <- range(pretty(c(0,slim.diag)))
    slim.offdiag <- slim.diag - diff(slim.diag)/2
  }
  
  rx  <- range(x)
  rxe <- c(rx[1] - u * diff(rx), rx[2] + u * diff(rx))

  for(i in 1 : p) {
      for(j in 1 : p) {
        if((i == j) && all(s[i,j,] == 1))
          next
          
        sl <- if(slimds) if(i==j) slim.diag
        else slim.offdiag
        else slim
        
        sle <- c(sl[1]-u*diff(sl), sl[2]+u*diff(sl))
        
        if(! add) {
          lines(c(i-w/2,i+w/2,i+w/2,
                  i-w/2,i-w/2),
                c(j-h/2,j-h/2,j+h/2,
                  j+h/2,j-h/2), col=gray(.5), lwd=.65)
          xc <- rep(i-w/2-u/3,2)
          yc <- scaleit(sl, sle, c(j-h/2,j+h/2))
          if(i==1 && j<=2)
            {
              text(xc, yc,
                   format(sl,digits=2), adj=1, cex=.7)
              segments(rep(xc+u/8,2),yc,
                       rep(xc+u/3,2),yc)
            }
        }
        lines(scaleit(x, rxe, c(i-w/2,i+w/2)),
              scaleit(s[i,j,], sle, c(j-h/2,j+h/2)),
              lty=lty, lwd=lwd, col=col)
        if(! add && slimds && (i != j))
          lines(c(i-w/2,i+w/2),
                rep(scaleit(0, sle, c(j-h/2,j+h/2)),2),
                col=gray(.5))
      }
    }
  
  invisible(slim)
}
