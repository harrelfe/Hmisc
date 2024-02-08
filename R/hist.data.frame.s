hist.data.frame <- function(x, n.unique=3, nclass="compute", na.big=FALSE,
                            rugs=FALSE, freq=TRUE, mtitl=FALSE, ...)
{
  oldmf  <- par('mfrow')
  oldoma <- par('oma')
  oldmar <- par('mar')  # resetting mfrow causes a new mar
  on.exit(par(mfrow=oldmf, oma=oldoma, mar=oldmar))
  mf <- oldmf
  if(length(mf)==0) mf <- c(1,1)

  automf <- FALSE
  if((la <- length(x))>1 & max(mf)==1) {
    mf <-
      if(la<=4)       c(2,2)
      else if(la<=6)  c(2,3)
      else if(la<=9)  c(3,3)
      else if(la<=12) c(3,4)
      else if(la<=16) c(4,4)
      else            c(4,5)
    
    automf <- TRUE
    par(mfrow=mf)
  }
  
  if(is.character(mtitl))
    par(oma=c(0,0,3,0))

  nam <- names(x)
  i <- 0
  j <- 0
  for(v in x) {
    j <- j+1
    type <-
      if(is.character(v) || is.factor(v))
        'cat'
      else if(inherits(v,'Date'))
        'Date'
      else
        'none'
    
    lab <- attr(v,"label")
    lab <-
      if(length(lab) && nchar(lab) > 35)
        nam[j]
      else
        label(v, units=TRUE, plot=type!='cat', default=nam[j])
    
    if(type=='cat') {
      tab <- -sort(-table(v))
      dotchart3(tab, xlab=paste('Frequencies for', lab))
    } else {
      type <- if(inherits(v,'Date')) 'Date' else 'none'
      
      if(type %nin% c('none','Date'))
        v <- unclass(v)
      
      w <- v[!is.na(v)]
      n <- length(w)
      if(length(unique(w)) >= n.unique) {
        i <- i+1
        if(is.numeric(nclass))
          nc <- nclass else

        if(nclass=="compute")
          nc <- max(2,trunc(min(n/10,25*logb(n,10))/2))

        if(nclass == 'default') {
          if(type == 'Date')
            hist(v, nc, xlab=lab, freq=freq, main='')
          else hist(v, xlab=lab, main='', freq=freq)
        } else {
          if(type == 'Date')
            hist(v, nc, xlab=lab, freq=freq, main='')
          else
            hist(v, nclass=nc, xlab=lab, freq=freq, main='')
        }
       
        m <- sum(is.na(v))
        pm <- paste("n:",n," m:",m,sep="")
        title(sub=pm,adj=0,cex=.5)
        if(na.big && m>0)
          mtext(paste(m,"NAs"),line=-2,cex=1)

        if(rugs)
          scat1d(v, ...)

        if(automf && interactive() &&
           all(names(dev.list()) %nin% c('postscript')) && # add win.printer?
           (i %% prod(mf)==0)) {
          if(is.character(mtitl))
            mtitle(mtitl)
          
          cat("click left mouse button to proceed\n")
          locator(1)
        } else if(is.character(mtitl) && i %% prod(mf)==1)
          mtitle(mtitl)
      }
    }
  }
  
  invisible(ceiling(i / prod(mf)))
}
