ps.slide <- function(file,
                     background=if(type!=2)"white"
                                else "navy blue", 
                     foreground=if(type==2)'yellow'
                                else (if(background=="white")"black"
                                      else "white"),
                     
                     font='Helvetica',
                     pointsize=c(24,28,14,14)[type], hor=type!=4, 
                     lwd=c(2,5,2,4)[type],
                     mgp=if(under.unix)
                           list(c(1.8,.4,0),c(1.5,.2,0),c(2,.4,0),c(1.5,.2,0))[[type]]
                         else
                           list(c(1.8,.5,0),c(1.5,.4,0),c(2,.5,0),c(1.5,.4,0))[[type]],
                     
                     mar=list(c(4,3,2,1)+.1,c(5,4,2.25,2)+.1,c(3,3,1,1)+.1,
                              c(5,4,2.25,2)+.1)[[type]],
                     pch=202, view=FALSE, pcx=FALSE, tiff=FALSE, close=view|pcx|tiff, bty="l", type=2,
                     height=switch(type,NULL,NULL,5,8), width=switch(type,NULL,NULL,7,7),
                     tck=if(type==3 || !under.unix) -.013
                         else par('tck'), 
                     las=if(type==3)1
                         else 0, 
                     eps=FALSE, ...)
{
  if(close) {
    graphics.off()
    file <- .Options$ps.slide.file
    if(view)
      sys(paste("ghostview ", file, ".ps &", sep=""), output=FALSE)
    
    if(pcx) {
      sys(paste("(gs -sDEVICE=pbm -sOutputFile=- -r75 -q - quit.ps < ",
                 file, ".ps | pnmflip -cw | ppmtopcx > ", file, ".pcx) &", sep=""),
           output=FALSE)
      cat("\nFile ", file, ".pcx being created \n", sep="")
    }
    
    if(tiff) {
      sys(paste("(gs -sDEVICE=pbmraw -sOutputFile=- -r300 -q - quit.ps < ",
                 file, ".ps | pnmflip -cw | pnmtotiff > ", file, ".tiff) &",sep=""),
           output=FALSE)
      cat("\nFile ", file, ".tiff being created \n", sep="")
    }
    
    return(invisible())
  }

  if(is.logical(background) && background)
    background <- "navy blue"
  
  options(ps.slide.file=file, TEMPORARY=FALSE)
  if(length(height) && length(width)) 
    postscript(paste(file,'.ps',sep=''),
               horizontal=hor, height=height, width=width,
               pointsize=.6*pointsize*max(width/(30*12/72.27),
                 height/(30*12/72.27/((1+sqrt(5))/2))),
               fg=foreground, bg=background, family=font, ...)
  else
    postscript(paste(file,'.ps',sep=''),
               fg=foreground, bg=background, family=font, ...)

  par(lwd=lwd, mgp=mgp, mar=mar, pch=pch, bty=bty, smo=0, tck=tck, las=las)
  ##mgp.axis.labels(c(mgp[2], if(las==1) 1.3 else mgp[2]))
  invisible()
}
