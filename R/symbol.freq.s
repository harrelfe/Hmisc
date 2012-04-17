## marginals applies only to symbol="therm", orig.scale to symbol="circle"

symbol.freq <- function(x, y, symbol=c("thermometer","circle"), 
                        marginals=FALSE, orig.scale=FALSE,
                        inches=.25, width=.15, subset, srtx=0, ...)
{
  symbol <- match.arg(symbol)
  if(missing(subset))
    subset <- rep(TRUE, length(x))

  if(!is.logical(subset)) {
	s <- rep(FALSE,length(x))
	s[subset] <- FALSE
	subset <- s
  }

  xlab <- attr(x,'label')
  if(!length(xlab))
    xlab <- as.character(substitute(x))

  ylab <- attr(y,'label')
  if(!length(ylab))
    ylab <- as.character(substitute(y))
  
  s <- !(is.na(x) | is.na(y)) & subset
  x <- x[s]
  y <- y[s]
  f <- table(x, y)
  dx <- dimnames(f)[[1]]
  dy <- dimnames(f)[[2]]
  if(orig.scale)
    xp <- as.numeric(dimnames(f)[[1]])
  else
    xp <- 1:length(dimnames(f)[[1]])

  xp1 <- length(xp)+1
  if(orig.scale)
    yp <- as.numeric(dimnames(f)[[2]])
  else
    yp <- 1:length(dimnames(f)[[2]])
  
  yp1 <- length(yp)+1
  m <- nrow(f) * ncol(f)
  xx <- single(m)
  yy <- single(m)
  zz <- single(m)
  k <- 0
  for(i in 1:nrow(f)) {
    for(j in 1:ncol(f)) {
      k <- k + 1
      xx[k] <- xp[i]
      yy[k] <- yp[j]
      if(f[i, j] > 0)
        zz[k] <- f[i, j]
      else zz[k] <- NA
    }
  }

  maxn <- max(f)
  n <- 10^round(log10(maxn))
  if(marginals) {
    xx <- c(xx, rep(xp1, length(yp)))
    yy <- c(yy, yp)
    zz <- c(zz, table(y)/2)
    xx <- c(xx, xp)
    yy <- c(yy, rep(yp1, length(xp)))
    zz <- c(zz, table(x)/2)		
    xx <- c(xx, xp1)
    yy <- c(yy, yp1)
    zz <- c(zz, n)
  }

  if(symbol=="circle") {
    ##		zz <- inches*sqrt(zz/maxn)
    zz <- sqrt(zz)
    if(orig.scale)
      symbols(xx,yy,circles=zz,inches=inches,
              smo=.02,xlab=xlab,ylab=ylab,...)
    else
      symbols(xx,yy,circles=zz,inches=inches,smo=.02,
              xlab=xlab,ylab=ylab,axes=FALSE,...)

    title(sub=paste("n=",sum(s),sep=""),adj=0)
    if(marginals) {
      axis(1, at = 1:xp1, 
           labels = c(dx, "All/2"), srt=srtx,
           adj=if(srtx>0)1
           else .5)
      
      axis(2, at = 1:yp1, 
           labels = c(dy, "All/2"),adj=1)
    } else { #	if(!orig.scale) {
      axis(1, at=xp, labels=dx, srt=srtx,
           adj=if(srtx>0)1
           else .5)
      
      axis(2, at=yp, labels=dy)
    }

    return(invisible())
  }

  zz <- cbind(rep(width,length(zz)), inches*zz/maxn, rep(0,length(zz)))
  symbols(xx,yy,thermometers=zz,inches=FALSE,
          axes=FALSE,xlab=xlab,ylab=ylab,...) 
  title(sub=paste("n=",sum(s),sep=""),adj=0)
  if(marginals)	{
    text(xp1-width, yp1, n, adj=1, cex=.5)
    axis(1, at = 1:xp1, 
         labels = c(dx, "All/2"), srt=srtx,
         adj=if(srtx>0)1
         else .5)
    
    axis(2, at = 1:yp1, 
         labels = c(dy, "All/2"),adj=1)
    abline(h=yp1-.5, lty=2)
    abline(v=xp1-.5, lty=2)
  } else {
    axis(1, at=xp, labels=dx, srt=srtx,
         adj=if(srtx>0)1
         else .5)
    
    axis(2, at=yp, labels=dy)
    cat("click left mouse button to position legend\n")
    xy <- locator(1)
    symbols(xy$x, xy$y, thermometers=cbind(width,inches*n/maxn,0), 
            inches=FALSE,add=TRUE,xlab=xlab,ylab=ylab)
    text(xy$x-width, xy$y, n,adj=1,cex=.5)
  }

  box()
  invisible()
}
