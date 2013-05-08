setpdf <- function(filename, w=0, h=4, pointsize=10, sublines=0, toplines=0,
                   type="symbol", lwd=1.5,
                   font='Helvetica',
                   ratio=4/3,
                   leftlines=0, las=1, bty='l', hor=FALSE,
                   trellis=!(missing(setTrellis.) & missing(strip.blank) &
                             missing(lty.dot.line) & missing(lwd.dot.line)), 
                   setTrellis.=TRUE, 
                   strip.blank = TRUE, lty.dot.line = 1, lwd.dot.line =1,
                   region=c(0, 0, h, w), color=FALSE, seqno=NULL, ...)
{
  if(type=="char")
    filename <- paste(filename,seqno,".pdf",sep="")
  else
    filename <- paste(substitute(filename),seqno,".pdf",sep="")
  
  if(length(.Options$setpdfPrefix))
    filename <- paste(.Options$setpdfPrefix, filename, sep='')

  if (w > 0 & h == 0)
    h <- w/ratio
  
  if (w == 0 & h > 0)
    w <- h*ratio
  
  if(trellis)
    trellis.device('pdf', file=filename, width=w, height=h,
                   pointsize=pointsize, family=font,
                   color=color,onefile=FALSE,
                   bg=ifelse(color,NULL,'white'))
  else
    pdf(filename, width=w, height=h, pointsize=pointsize,
        family=font,onefile=FALSE)

  if(!trellis) {
    par(lwd=lwd, mgp=c(2,.475,0), tcl=-0.4,
        mar=c(3.25+sublines,3.5+leftlines,
          .5+toplines,.5), bty=bty)
    par(smo = 0)
  }
  
  if(length(las))
    par(las=las)
  
  if(trellis && setTrellis.)
    setTrellis(strip.blank = strip.blank, 
               lty.dot.line = lty.dot.line, lwd.dot.line = lwd.dot.line)
  invisible()
}
