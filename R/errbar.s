## From: geyer@galton.uchicago.edu
## Modified 11May91 FEH - added na.rm to range()
## Modified 12Jul91 FEH - added add=T and lty=1 parameters
## Modified 12Aug91 FEH - added explicit ylim parameter
## Modified 26Aug94 FEH - added explicit lwd parameter for segments()
## FEH 2Jul02 added horizontal charts with differences on 2nd axis

errbar <-
  function(x, y, yplus, yminus, cap=.015,
           main=NULL, sub=NULL,
           xlab=as.character(substitute(x)),
           ylab=if(is.factor(x) || is.character(x)) ''
           else
           as.character(substitute(y)),
           add=FALSE, lty=1, type='p', ylim=NULL, lwd=1, pch=16,
           errbar.col=par("fg"),
           Type=rep(1,length(y)), ...)
{
  if(is.null(ylim)) 
    ylim <- range(y[Type==1], yplus[Type==1], yminus[Type==1],
                  na.rm=TRUE)
  
  if(is.factor(x) || is.character(x))
    {
      x <- as.character(x)
      n <- length(x)
      t1 <- Type==1
      t2 <- Type==2
      n1 <- sum(t1)
      n2 <- sum(t2)
      
      omai <- par('mai')
      mai <- omai
      mai[2] <- max(strwidth(x, 'inches')) + .25
    
      par(mai=mai)
      on.exit(par(mai=omai))

      plot(NA, NA, xlab=ylab, ylab='',
           xlim=ylim, ylim=c(1, n+1),
           axes=FALSE, main=main, sub=sub, ...)
      axis(1)
    
      w <-
        if(any(t2)) n1+(1:n2)+1
        else
          numeric(0)
    
      axis(2, at=c(seq.int(length.out=n1), w), labels=c(x[t1], x[t2]),
           las=1, adj=1)
      points(y[t1], seq.int(length.out=n1), pch=pch, type=type, ...)
      segments(yplus[t1], seq.int(length.out=n1), yminus[t1],
               seq.int(length.out=n1), lwd=lwd, lty=lty, col=errbar.col)

      if(any(Type==2))
        {
          abline(h=n1+1, lty=2, ...)
          offset <- mean(y[t1]) - mean(y[t2])
          
          if(min(yminus[t2]) < 0 & max(yplus[t2]) > 0)
            lines(c(0,0)+offset, c(n1+1,par('usr')[4]), lty=2, ...)
          
          
          points(y[t2] + offset, w, pch=pch, type=type, ...)
          segments(yminus[t2] + offset, w, yplus[t2] + offset, w,
                   lwd=lwd, lty=lty, col=errbar.col)
          
          at <- pretty(range(y[t2], yplus[t2], yminus[t2]))      
          axis(side=3, at=at + offset, labels=format(round(at, 6)))      
        }
      
      return(invisible())
    }
  
  if(add)
    points(x, y, pch=pch, type=type, ...)
  else
    plot(x, y, ylim=ylim, xlab=xlab, ylab=ylab, pch=pch, type=type,
         main=main, ...)
  
  xcoord <- par()$usr[1:2]
  smidge <- cap * ( xcoord[2] - xcoord[1] ) / 2
  
  segments(x, yminus, x, yplus , lty=lty, lwd=lwd, col=errbar.col)
  
  if(par()$xlog)
    {
      xstart <- x * 10 ^ (-smidge)
      xend <- x * 10 ^ (smidge)
    }
  else
    {
      xstart <- x - smidge
      xend <- x + smidge
    }
  segments( xstart, yminus, xend, yminus, lwd=lwd, lty=lty, col=errbar.col)
  segments( xstart, yplus, xend, yplus, lwd=lwd, lty=lty, col=errbar.col)
  
  return(invisible())
}
