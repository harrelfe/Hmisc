pstamp <- if(.R.) function(txt, pwd=FALSE, time.=TRUE)
{
  stamp <- function(string, ...)
  {
    opar <- par(yaxt='s',xaxt='s',xpd=NA)
    on.exit(par(opar))
    plt <- par('plt')
    usr <- par('usr')
    text(usr[2] + diff(usr[1:2])/diff(plt[1:2])*
         (1-plt[2]) - .6*strwidth('m'),
         usr[3] - diff(usr[3:4])/diff(plt[3:4])*plt[3] +
         .6*strheight('m'),
         string, adj=1)
    invisible()
  }

  date.txt <- if(time.) format(Sys.time())
              else format(Sys.time(), '%Y-%m-%d')
  
  if(pwd)
    date.txt <- paste(getwd(), date.txt)

  old <- par(c('mfrow','cex'))
  par(mfrow=c(1,1))
  par(cex=.5)
  if(!missing(txt))
    date.txt <- paste(txt,'   ',date.txt, sep='')
  
  stamp(string=date.txt,print=FALSE,plot=TRUE)
  par(old)
  invisible()

} else function(txt, pwd=FALSE, time.=under.unix)
{

  date.txt <- if(time.) date() else {
    if(.SV4.)
      format(timeDate(date(), in.format='%w %m %d %H:%M:%S %Z %Y',
                      format='%Y-%m-%d'))
    else if(under.unix)
      unix('date +%Y-%m-%d')
    else
      stop('time.=T not supported')
  }
                 
  if(pwd) {
    if(!under.unix)
      stop('pwd not supported except with Linux/UNIX')
    
    pwd <- unix('pwd')
    date.txt <- paste(pwd, date.txt)
  }
  
  old <- par(c('mfrow','cex'))
  par(mfrow=c(1,1))
  par(cex=.5)
  if(!missing(txt))
    date.txt <- paste(txt,'   ',date.txt, sep='')
  
  stamp(string=date.txt,print=FALSE,plot=TRUE)
  par(old)
  invisible()
}  
