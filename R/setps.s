setps <- function(filename, w=0, h=3, pointsize=10, sublines=0, toplines=0,
                  type="symbol", lwd=2, font='Helvetica',
                  leftlines=0, las=1, 
                  trellis=!(missing(setTrellis.) & missing(strip.blank) &
                            missing(lty.dot.line) & missing(lwd.dot.line)), 
                  setTrellis.=TRUE, 
                  strip.blank = TRUE, lty.dot.line = 1, lwd.dot.line = 1,
                  seqno=NULL, color=FALSE)
{
  filebase <-
    if(type=='char')
      filename
    else as.character(substitute(filename))
  
  if(length(seqno))
    filebase <- paste(filebase,seqno,sep='')
  
  filename <- paste(filebase,'.ps',sep='')
  if(length(.Options$setpsPrefix))
    filename <- paste(.Options$setpsPrefix, filename, sep='')

  ## Changed after submission to s-news: pointsize=NULL
  ## Antonio likes the default
  ## ratio of width/height to be the "golden ratio", which is the default.
  ## I often prefer a smaller ratio of 1.4. If exactly one of (width, height)
  ## is zero, the "ratio" is used to replace it based on the one specified.
  ## For a single figure in the plot I usually use psfig(filename,height=3).
  ## For a single figure in the plot I usually use psfig(filename,height=3).
  ## The logic in psfig assumes that one figure is being drawn, i.e., that
  ## par(mfrow=c(1,1)) is in effect. It will work for multiple plots if you
  ## set pointsize to something like 9.
  ## sublines specifies the number of extra lines to leave at the bottom of
  ## the plot for subtitles.
  ##
  ##    I include an S function that sets the stage for EPS graphics
  ## generation that will be incorporated by TeX (LaTeX, etc.), and that
  ## does a little of what you want, by hand, not in the smart way you
  ## envision.
  ##   Note that this function intentionally disallows main titles, with
  ## the understanding that they will be part of the figure's caption,
  ## which TeX itself generates. You may like to use it as starting point
  ## to get something that suits your needs.
  ##
  ##   - Antonio Possolo
  ##
  ##        Applied Mathematics & Statistics
  ##        The Boeing Company
  ##                           antonio@atc.boeing.com
  ##
  ##
  ## Added else scale <-   FEH 8Sep92, also added arg "ratio",
  ## commented out warning message for omitting main title,
  ## added arg sublines, pointsize
  ## may want to specify pointsize=9 if multiple plots used
  ## added lwd FEH 27Oct92
  ## added toplines FEH 18Oct93
  ## override fonts spec to ps.options because of bug - FEH 21Apr94
  ## added bty="l" FEH 24Aug94
  ## added leftlines FEH 26Aug94
  ## added onefile 27Feb95
  ## maden font default to Helvetica 25Mar00
  ## Doug Bates just does this:
  ## a) use postscript(filename, height=xx, width=yy, pointsize=10)
  ## b) change the figure's region on the page by using
  ##    par (mar=c(3.5, 3.5, 1.5, 0.5))  ## for example and perhaps also
  ##    par (mgp=c(2.5, 0.5, 0))
  ## 
  ##  added color=FALSE 7feb03
  
  psfig <- function(file = "", width = 0, height = 0,
                    ratio= (1 + sqrt(5))/2, font = 'Helvetica', 
                    pointsize=NULL, sublines=0, 
                    toplines=0, leftlines=0, lwd=0.5, bty="l", onefile=FALSE, 
                    las=NULL, trellis=FALSE, color=FALSE)
  {

    ##	POSTSCRIPT FIGURE MAKER
    ##	for incorporation into TeX using PSFIG or BoxedEPSF.
    ##	The strategy is to create a pleasant aspect ratio, 
    ##	while minimizing white space around the figure.
    ##
    ## Aspect ratio is Golden Ratio
    ## Standard width is 30 picas = 30*12/(72.27) inches
    StandardWidth <- (30 * 12)/(72.27)
    StandardHeight <- StandardWidth/ratio
    StandardPointSize <- 9
    if ( width == 0 & height == 0 ) { 
      width <- StandardWidth
      height <- StandardHeight
      scale <- 1
    }
    
    if ( width > 0 & height == 0 ) { 
      height <- width/ratio
      scale <- width/StandardWidth
    }
    
    if ( width == 0 & height > 0 ) { 
      width <- height*ratio
      scale <- width/StandardWidth
    }
    else scale <- max(width/StandardWidth,height/StandardHeight)

    if(!length(pointsize)) pointsize <- round(scale * StandardPointSize)

    ##	FONTS & FONT SELECTION
    ##
    ##  1 Helvetica               19 Bookman-DemiItalic
    ##  2 Courier                 20 Bookman-Light
    ##  3 Times-Roman             21 Bookman-LightItalic
    ##  4 Helvetica-Oblique       22 Helvetica-Narrow
    ##  5 Helvetica-Bold          23 Helvetica-Narrow-Bold
    ##  6 Helvetica-BoldOblique   24 Helvetica-Narrow-BoldOblique
    ##  7 Courier-Oblique         25 Helvetica-Narrow-Oblique
    ##  8 Courier-Bold            26 NewCenturySchlbk-Roman
    ##  9 Courier-BoldOblique     27 NewCenturySchlbk-Bold
    ## 10 Times-Italic            28 NewCenturySchlbk-Italic
    ## 11 Times-Bold              29 NewCenturySchlbk-BoldItalic
    ## 12 Times-BoldItalic        30 Palatino-Roman
    ## 13 Symbol                  31 Palatino-Bold
    ## 14 AvantGarde-Book         32 Palatino-Italic
    ## 15 AvantGarde-BookOblique  33 Palatino-BoldItalic
    ## 16 AvantGarde-Demi         34 ZapfChancery-MediumItalic
    ## 17 AvantGarde-DemiOblique  35 ZapfDingbats
    ## 18 Bookman-Demi           

    if(trellis) do.call('trellis.device',
                        list(device='postscript',
                             file = file, horizontal = FALSE,
                             width = width, height = height,
                             pointsize = pointsize,
                             family=font, color=color, paper='special',
                             bg=if(!color)'white'
                             else NULL))
    else
      postscript(file = file, horizontal = FALSE, width = width, height = height,
                 pointsize = pointsize, family=font,
                 onefile=onefile, print.it=FALSE, paper='special')	
    
    par(lwd=lwd, mgp=c(2,.475,0), tcl=-0.4,
        mar=c(3.25+sublines, 3.5+leftlines,
          .5+toplines,.5), bty=bty)
    ##	SMO is number of rasters that the piecewise linear
    ##	approximation to a curve is allowed to differ from the exact
    ##	position of the curve.
    par(smo = 0)                      #	PLOTTING SYMBOL
    
    ##	PCH  selects plotting characters from Standard Encoding
    ##	(PostScript Language Reference Manual, p.252)
    ##	168 = currency
    ##	180 = centered period
    ##	183 = bullet (with a negative font parameter yields a circle
    par(pch = 1)                      # was 183 11Jan01
    
    ## MAIN TITLE not allowed: plot will be described in figure caption, 
    ##	handled by TeX itself.
    ##	cat(paste("\tPSFIG WARNING:", "Do not use high-level parameter MAIN\n",
    ##		"\t\tFigure caption should be created within LaTeX\n"))	#
    if(length(las))
      par(las=las)
    
    invisible()
  }

  psfig(filename, h=h, w=w, ratio=1.4, 
        pointsize=pointsize,sublines=sublines,toplines=toplines,
        lwd=lwd,font=font,leftlines=leftlines, las=las,
        trellis=trellis, color=color)   # color= 7feb03
          
  if(trellis && setTrellis.)
    setTrellis(strip.blank = strip.blank, 
               lty.dot.line = lty.dot.line, lwd.dot.line = lwd.dot.line)

  topdf <- function(filebase)
  {
    cmd <-
      if(under.unix)'gs'
      else 'gswin32c'
    
    cmd <-
      paste(cmd, ' -q -dNOPAUSE -dBATCH -sDEVICE #pdfwrite -sOutputFile#',
            filebase, '.pdf -c save pop -f ', filebase, '.ps', sep='')
    
    sys(cmd)
    invisible()
  }
  
  formals(topdf) <- list(filebase=filebase)
  .settopdf(topdf)
  invisible()
}
