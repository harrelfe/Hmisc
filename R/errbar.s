## From: geyer@galton.uchicago.edu
## Modified 11May91 FEH - added na.rm to range()
## Modified 12Jul91 FEH - added add=T and lty=1 parameters
## Modified 12Aug91 FEH - added explicit ylim parameter
## Modified 26Aug94 FEH - added explicit lwd parameter for segments()
## FEH 2Jul02 added horizontal charts with differences on 2nd axis

errbar <-
  function(x, y, yplus, yminus, cap=.015,
           xlab=as.character(substitute(x)),
           ylab=if(is.factor(x) || is.character(x)) ''
           else
           as.character(substitute(y)),
           add=FALSE, lty=1, ylim, lwd=1,
           Type=rep(1,length(y)), ... )
{
  if(missing(ylim))
    ylim <- range(y[Type==1], yplus[Type==1], yminus[Type==1],
                  na.rm=TRUE)
  
  if(is.factor(x) || is.character(x)) {
    x <- as.character(x)
    n <- length(x)
    t1 <- Type==1
    t2 <- Type==2
    n1 <- sum(t1)
    n2 <- sum(t2)
    
    omai <- par('mai')
    mai <- omai
    mai[2] <- max(strwidth(x, 'inches')) + .25 * .R.
    par(mai=mai)
    on.exit(par(mai=omai))
    plot(0,0,xlab=ylab,ylab='',xlim=ylim,ylim=c(1,n+1),axes=FALSE,...)
    axis(1)
    w <-
      if(any(t2))
        n1+(1:n2)+1
      else
        numeric(0)
    
    axis(2, at=c(1:n1,w), labels=c(x[t1],x[t2]), las=1,adj=1)
    points(y[t1], 1:n1, pch=16, ...)
    segments(yplus[t1], 1:n1, yminus[t1], 1:n1, ...)

    if(any(Type==2)) {
      abline(h=n1+1, lty=2, ...)
      offset <- mean(y[t1]) - mean(y[t2])
      if(min(yminus[t2]) < 0 & max(yplus[t2]) > 0)
        lines(c(0,0)+offset, c(n1+1,par('usr')[4]), lty=2, ...)
      
      points(y[t2] + offset, w, pch=16, ...)
      segments(yminus[t2]+offset, w, yplus[t2]+offset, w, ...)
      at <- pretty(range(y[t2],yplus[t2],yminus[t2]))
      axis(3, at=at+offset, label=format(round(at,6)))
    }
    
    return(invisible())
  }
  
  if(add) points(x, y, ...)
  else
    plot(x, y, ylim=ylim, xlab=xlab, ylab=ylab, ...)
  
  xcoord <- par()$usr[1:2]
  segments(x, yminus, x, yplus , lty=lty, lwd=lwd, ...)
  smidge <- cap * ( xcoord[2] - xcoord[1] ) / 2
  segments( x - smidge, yminus, x + smidge, yminus, lwd=lwd, ...)
  segments( x - smidge, yplus, x + smidge, yplus, lwd=lwd, ...)
  invisible()
}
