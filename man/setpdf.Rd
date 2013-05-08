\name{setpdf}
\alias{setpdf}
\title{
  Makes small pdf plots with minimal surrounding white space
}
\description{
  \code{setpdf} is similar to \code{setps} but for making Adobe Acrobat
  PDF graphics files directly.  There are a few problems with the S-Plus
  \code{pdf.graph} function used by \code{setpdf}, though: (1) the
  default for points (open circle) is too large, (2) graphs are not
  centered properly, (3) gray scale does not work, and (4) there is some
  wasted space at the bottom of the graph.  When drawing points, the
  user may want to specify \code{cex=0.7}.  It may be better to use
  \code{setps} followed by \code{topdf()}.
}
\usage{
setpdf(filename, w=0, h=4, pointsize=10, sublines=0, toplines=0,
       type="symbol", lwd=1.5, font="Helvetica",
       ratio= 4/3,
       leftlines=0, las=1, bty='l', hor=FALSE, 
       trellis=!(missing(setTrellis.) & missing(strip.blank) &
                 missing(lty.dot.line) & missing(lwd.dot.line)), 
       setTrellis.=TRUE, 
       strip.blank =TRUE, lty.dot.line = 1, lwd.dot.line =1,
       region=c(0, 0, h, w), color=FALSE, seqno=NULL, \dots)
}
\arguments{
  \item{file,filename}{
    name or character string or character vector specifying file
    prefix. For \code{setps} or \code{setpdf} specify \code{type="char"}
    if this is a character vector or a quote-delimited character string.
  }
  \item{string}{
    a character string to be processed by \code{psfrag} in LaTeX.
  }
  \item{background}{
    default is yellow on navy blue background (black on white for
    \code{type=1,3}. \code{background} may also be set to any legitimate
    background color listed in the S-supplied object
    \code{ps.colors.rgb}.
  }
  \item{foreground}{
    foreground color.  See \code{background} for allowable values.
  }
  \item{font}{
    font for text.  Replaces the first font in the standard list of
    fonts in \code{ps.options("fonts")}.  If \code{font="Times-Roman"},
    the fifth font (normally \code{Helvetica-Bold}) is set to
    \code{Times-Bold}.  For \code{setpdf}, \code{font} is a number, and
    the default is \samp{1} for Helvetica.   All default fonts are
    Helvetica for \code{setps}, \code{psfig}, and \code{ps.slide}.
  }
  \item{pointsize}{
    postscript point size. Set to a larger number if using multiple
    plots via \code{par(mfrow=)}.
  }
  \item{hor}{
    default is \code{TRUE} to make a horizontal graph
  }
  \item{lwd}{
    line width
  }
  \item{mgp}{
    see \code{\link{par}}.  Defaults are chosen according to \code{type}.
  }
  \item{mar}{
    margins (see \code{\link{par}})
  }
  \item{pch}{
    see \code{\link{par}}
  }
  \item{view}{
    set to \code{TRUE} to initiate a \command{ghostview} run to
    view the postscript file. This option will also close out the
    postscript file (this is done before viewing).  If you have an
    active \command{ghostview} window for this file already, you
    can just type \code{graphics.off} or \code{dev.off} to re-create
    the \file{.ps} file. \command{ghostview} will then update theimage
    automatically.
  }
  \item{pcx}{
    set to \code{TRUE} to initiate conversion to \verb{pcx} format.
    Also implies \code{close=TRUE}.
  }
  \item{tiff}{
    set to \code{TRUE} to initiate conversion to \verb{tiff} format.
    Also implies \code{close=TRUE}.
  }
  \item{close}{
    set to \code{TRUE} to finish construction of the postscript file.
  }
  \item{bty}{
    box type surrounding graph.  Default is \code{"l"} for \code{"L"}
    shape.  Use \code{"c"} for completebox.
  }
  \item{type}{
    For \code{ps.slide}, \code{type} is an integer.  In this case, set
    \code{type=1} to use black on white background, smaller pointsize,
    and other settings that are good for making overhead transparencies
    and graphs to include in reports.  Set \code{type=3} for 5 x 7in
    landscape plots, and \code{type=4} for overheads. For \code{setps}
    and \code{setpdf}, \code{type="char"} specifies that  the
    \code{filename} argument is a character string  or vector, and any
    other value indicatesthat it is an unquoted name.
  }
  \item{height}{
    defaults to 5 if \code{type=3}, otherwise no default (except for type=4)
  }
  \item{width}{
    defaults to 7 if \code{type=3}, otherwise no default (except for type=4)
  }
  \item{tck}{
    length of tick marks.  See \code{\link{par}}.
  }
  \item{las}{
    set to \code{0} to have axis labels always parallel to the axis,
    \code{1} for always horizontal, \code{2} for perpendicular toaxis
  }
  \item{eps}{
    set to \code{TRUE} if you are going to be importing the postscript
    file to a system that really cares that it is marked to officially
    be encapsulated postscript.  If you set \code{eps=TRUE}, you may put
    only one figure in the file (see the \code{onefile} argument in
    \code{\link{postscript}}).  This applies to UNIX systems only.
  }
  \item{\dots}{
    other arguments to \code{\link{ps.options}} (or
    \code{\link{postscript}} for Windows or \code{pdf.graph} for
    \code{setpdf})
  }
  \item{w}{
    width of plot.  Default is chosen to scale nicely to \code{h} for a
    landscapeplot
  }
  \item{h}{
    height of plot (default is 3in)
  }
  \item{sublines}{
    number of lines to reserve for subtitles
  }
  \item{toplines}{
    number of lines to reserve for main title
  }
  \item{leftlines}{
    number of lines to reserve for left margin
  }
  \item{trellis}{
    set to \code{TRUE} to set up for postscript output for Trellis
    graphics.  This makes \code{trellis.device("postscript", \dots)} be
    called instead of \code{postscript(\dots)} directly, and leaves
    \code{\link{par}} parametersat defaults.
  }
  \item{setTrellis.}{
    set to \code{FALSE} to prevent \code{setTrellis} from being called
    to set the strip panel background and to set characteristics for dot
    plot reference lines
  }
  \item{strip.blank}{
    set to \code{FALSE} to keep shading in conditioning variable panel
    titles, if \code{setTrellis.=TRUE}
  }
  \item{lty.dot.line}{
    if \code{setTrellis.=TRUE}, the line type for dot plot reference
    lines (default = solid line)
  }
  \item{lwd.dot.line}{
    if \code{setTrellis.=TRUE}, the line width for dot plot reference
    lines (default= 1)
  }
  \item{seqno}{
    if non-\code{NULL}, pastes the value of \code{seqno} at the end of
    the base of the file name, for \code{setps} and \code{setpdf}
  }
  \item{color}{
    set \code{color=TRUE} to use a color Trellis device instead of
    default of black and white, for \code{setps}.  For \code{setpdf} set
    to \code{TRUE} to get color pdf graphics.
  }
  \item{region}{
    see \code{pdf.graph}.  Default is to use an image region that is
    just large enough to contain the graphic.
  }
  \item{ratio}{
    ratio of width to height of the plot when only one of those is
    specified.  Defaults depend on whether S-Plus or \R are being used.
  }
  \item{lref}{
    LaTeX reference point for \code{string}.  See the \code{psfrag}
    documentation referenced below.  Default is \code{"c"} for centered
    (this is also the default for \code{psref}).
  }
  \item{psref}{
    PostScript reference point.
  }
  \item{scale}{
    scall factor, default is 1
  }
  \item{srt}{
    rotation for \code{string} in degrees (default is zero)
  }
}
\author{
  Frank Harrell\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{f.harrell@vanderbilt.edu}
}
\references{
  Grant MC, Carlisle (1998): The PSfrag System, Version 3.  Full
  documentation is obtained by searching www.ctan.org for \file{pfgguide.ps}.
}
\seealso{
  \code{\link{postscript}}, \code{\link{par}}, \code{\link{ps.options}},
  \code{\link{mgp.axis.labels}}, \code{\link{pdf}},
  \code{\link[lattice]{trellis.device}}, \code{\link{setTrellis}}
}
\examples{
\dontrun{
setpdf(myfile)
x <- seq(0,15,length=100)
plot(x, dchisq(x, 5), xlab=tex('$x$'),
        ylab=tex('$f(x)$'), type='l')
title(tex('Density Function of the $\\chi_{5}^{2}$ Distribution'))
dev.off()
# To process this file in LaTeX do something like
#\documentclass{article}
#\usepackage[scanall]{psfrag}
#\begin{document}
#\begin{figure}
#\includegraphics{test.ps}
#\caption{This is an example}
#\end{figure}
#\end{document}
}
}
\keyword{hplot}
\keyword{device}
\concept{trellis}
\concept{lattice}
