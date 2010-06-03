\name{ps.slide}
\alias{ps.slide}
\alias{setps}
\alias{setpdf}
\alias{topdf}
\alias{tex}
\alias{showPsfrag}
\title{Postscript and Adobe PDF Setup for 35mm Slides and Other Formats}
\description{
The \code{ps.slide} function has nice defaults to create postscript
images with larger font, thicker lines, and better axis labeling.  These
images can be used to make nice slides.  There is an option to view the
constructed postscript file using \preformatted{ghostview}, and an option to
initiate a background process to convert the postscript file to a PC
Paintbrush \file{.pcx} file for importing into various PC presentation
graphics packages although with a significant  loss in resolution.  This
option assumes you have installed various public-domain unix image
conversion programs. You can preview \file{.pcx} files using
e.g. \preformatted{xli file.pcx &}. Specify \code{type=1} to make nice fullsize
graphs or \code{type=3} for making 5 x 7in landscape graphs using
14-point type (useful for submitting to journals).  \code{type=2} (the
default) is for color 35mm slides. Use \code{type=4} to make nice black
and white overhead projection transparancies (portrait mode).  This uses
line thickness 4, pointsize 14, height 8, width 7.  For \code{type=3},
numbers on the y-axis are written horizontally (\code{las} defaults to
\code{1} for \code{type=3}).

\code{ps.slide} calls \code{mgp.axis.labels} in Hmisc set up axis-specific
defaults for the 2nd \code{mgp} graphical parameter.  See \code{\link{HmiscOverview}} for
Hmisc for help.  This is only used automatically for select high-level graphics
functions in Hmisc and Design, as S-Plus only supports a single
distance between tick marks and tick mark labels using \code{par}, and
when \code{las=1} a larger distance is needed for the y-axis.

See the body of the function for \code{type}-specific default values for many
of the parameters.  This function has not been tested for color output on
Windows systems.

\code{setps} is a function that makes small postscript plots with minimal
surrounding white space, suitable for inclusion in books and reports.
Internally \code{setps} uses (and defines) the \code{psfig} function by
Antonio Possolo (\email{antonio@atc.boeing.com}).  \code{setps} is especially good
for including plots in LaTeX.  \code{setps} creates a temporary function in the
session database that when invoked will convert a completed postscript
graphics file to a Adobe Acrobat .pdf if you have Ghostscript
installed and in your path (so that the \preformatted{gs} command is available in
UNIX or \preformatted{gswin32c} is available for Windows/NT).  Invoke \code{topdf} by
the command \code{topdf()}, or, if you want to convert a graphic other than
the last one created, run \code{topdf(filename)} to convert \file{filename.ps}
to \file{filename.pdf}.  If \code{trellis=TRUE}, \code{setps} invokes \code{\link[lattice]{trellis.device}}
with a \code{postscript} device argument, and it does not set any of the
\code{par} parameters.  Arguments
3, 4, 5, 7, 9, and 10 to \code{setps} are ignored if \code{trellis=TRUE}.  If
\code{options(setpsPrefix="name")} is defined, the \code{"name"} string 
will be prefixed to the file name used by \code{setps}.  \code{setpdf} uses a
similar option \code{setpdfPrefix}.  \code{setps} and \code{setpdf} set
\code{par(mgp=c(2,0.4,0))} if \code{trellis=FALSE}.

\code{setpdf} is similar to \code{setps} but for making Adobe Acrobat PDF
graphics files directly.  There are a few problems with the S-Plus
\code{pdf.graph} function used by \code{setpdf}, though: (1) the default for
points (open circle) is too large, (2) graphs are not centered
properly, (3) gray scale does not work, and (4) there is some wasted
space at the bottom of the graph.  When drawing points, the user may
want to specify \code{cex=0.7}.  It may be better to use \code{setps} followed
by \code{topdf()}.

\code{tex} is a little function to save typing when including tex
commands in graphs that are used with the psfrag package in LaTeX to
typeset any LaTeX text inside a postscript graphic.  \code{tex} surrounds
the input character string with \preformatted{\tex[options]\{\}}.  This is especially
useful for getting Greek letters and math symbols in postscript
graphs.  By default \code{tex} returns a string with \code{psfrag} commands
specifying that the string be centered, not rotated, and not specially
enlarged or shrunk.

\code{showPsfrag} is used to display (using ghostview) a postscript image
that contained psfrag LaTeX strings, by building a small LaTeX script
and running latex and dvips.
}
\usage{
ps.slide(file, background = if (type != 2) "white" else "navy blue",
         foreground = if (type == 2) "yellow" else
                     (if(background == "white") "black" else "white"),
         font = "Helvetica", pointsize = c(24, 28, 14, 14)[type],
         hor = type != 4, lwd = c(2, 5, 2, 4)[type],
         mgp = if(under.unix) list(c(1.8, 0.4, 0), c(1.5, 0.2, 0),
                 c(2, 0.4, 0), c(1.5, 0.2, 0))[[type]] else
                 list(c(1.8, 0.5, 0), c(1.5, 0.4, 0), c(2, 0.5, 0),
                 c(1.5, 0.4, 0))[[type]],
         mar = list(c(4, 3, 2, 1) + 0.1, c(5, 4, 2.25, 2) + 0.1,
                 c(3, 3, 1, 1) + 0.1, c(5, 4, 2.25, 2) + 0.1)[[type]],
         pch = 202, view = FALSE, pcx = FALSE, tiff = FALSE,
         close = view | pcx | tiff, bty = "l",
         type = 2, height = switch(type, NULL, NULL, 5, 8),
         width = switch(type, NULL, NULL, 7, 7),
         tck = if (type == 3 || !under.unix) -0.013 else par("tck"),
         las = if (type == 3) 1 else 0, eps =  FALSE, ...)

setps(filename, w=0, h=3, pointsize=10, sublines=0, toplines=0,
      type="symbol", lwd=2, font="Helvetica",
      leftlines=0, las=1,
      trellis=!(missing(setTrellis.) & missing(strip.blank) &
                missing(lty.dot.line) & missing(lwd.dot.line)), 
      setTrellis.=TRUE, 
      strip.blank =TRUE, lty.dot.line = 1, lwd.dot.line = 1,
      seqno=NULL, color=FALSE)


setpdf(filename, w=0, h=4, pointsize=10, sublines=0, toplines=0,
       type="symbol", lwd=1.5, font=if(.R.)"Helvetica" else 1,
       ratio= if(.R.) 4/3 else (1 + sqrt(5))/2,
       leftlines=0, las=1, bty='l', hor=FALSE, 
       trellis=!(missing(setTrellis.) & missing(strip.blank) &
                 missing(lty.dot.line) & missing(lwd.dot.line)), 
       setTrellis.=TRUE, 
       strip.blank =TRUE, lty.dot.line = 1, lwd.dot.line =1,
       region=c(0, 0, h, w), color=FALSE, seqno=NULL, \dots)


tex(string, lref='c', psref='c', scale=1, srt=0)


showPsfrag(filename)
}
\arguments{
\item{file}{
}
\item{filename}{
name or character string or character vector specifying file prefix.
For \code{setps} or \code{setpdf} specify \code{type="char"} if this is a
character vector or a quote-delimited character string.
}
\item{string}{
a character string to be processed by \code{psfrag} in LaTeX.
}
\item{background}{
  default is yellow on navy blue background (black on white for \code{type=1,3}.  
  \code{background} may also be
  set to any legitimate background color listed in the S-supplied object
  \code{ps.colors.rgb}.
}
\item{foreground}{
  foreground color.  See \code{background} for allowable values.
}
\item{font}{
  font for text.  Replaces the first font in the standard list of fonts
  in \code{ps.options("fonts")}.  If \code{font="Times-Roman"}, the fifth font
  (normally \code{Helvetica-Bold}) is set to \code{Times-Bold}.  For \code{setpdf},
  \code{font} is a number, and the default is \code{1} for Helvetica.   All
  default fonts are Helvetica for \code{setps}, \code{psfig}, and \code{ps.slide}.
}
\item{pointsize}{
  postscript point size. Set to a larger number if using multiple plots
  via \code{par(mfrow=)}.}
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
  set to \code{TRUE} to initiate a \preformatted{ghostview} run to view the postscript file.
  This option will also close out the postscript file (this is done before
  viewing).  If you have an active \preformatted{ghostview} window for this file already,
  you can just type \code{graphics.off()} or \code{dev.off()} to re-create the \file{.ps} file.
  \preformatted{ghostview} will then update the image automatically.
}
\item{pcx}{
  set to \code{TRUE} to initiate conversion to \preformatted{pcx} format.  Also implies
  \code{close=TRUE}.
}
\item{tiff}{
  set to \code{TRUE} to initiate conversion to \preformatted{tiff} format.  Also implies
  \code{close=TRUE}.
}
\item{close}{
  set to \code{TRUE} to finish construction of the postscript file.
}
\item{bty}{
  box type surrounding graph.  Default is \code{"l"} for \code{"L"} shape.  Use
  \code{"c"} for complete box.
}
\item{type}{
  For \code{ps.slide}, \code{type} is an integer.  In this case,
  set \code{type=1} to use black on white background, smaller pointsize,
  and other settings that are good for making
  overhead transparencies and graphs to include in reports.  Set \code{type=3}
  for 5 x 7in landscape plots, and \code{type=4} for overheads.
  For \code{setps} and \code{setpdf}, \code{type="char"} specifies that 
  the \code{filename} argument is a character string  or vector, and 
  any other value indicates that it is an unquoted name.
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
  set to \code{0} to have axis labels always parallel to the axis, \code{1} for
  always horizontal, \code{2} for perpendicular to axis
}
\item{eps}{
  set to \code{TRUE} if you are going to be importing the postscript file to a
  system that really cares that it is marked to officially be encapsulated
  postscript.  If you set \code{eps=TRUE}, you may put only one figure in the file
  (see the \code{onefile} argument in \code{postscript}).  This applies to UNIX systems
  only.
}
\item{...}{
  other arguments to \code{ps.options} (or \code{postscript} for Windows or
  \code{pdf.graph} for \code{setpdf})
}
\item{w}{
  width of plot.  Default is chosen to scale nicely to \code{h} for a
  landscape plot
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
  set to \code{TRUE} to set up for postscript output for Trellis graphics.  This
  makes \code{trellis.device("postscript", \dots)} be called instead of
  \code{postscript(\dots)} directly, and leaves \code{par} parameters at defaults.
}
\item{setTrellis.}{
  set to \code{FALSE} to prevent \code{setTrellis} from being called to set the
  strip panel background and to set characteristics for dot plot
  reference lines
}
\item{strip.blank}{
  set to \code{FALSE} to keep shading in conditioning variable panel titles, if
  \code{setTrellis.=TRUE}
}
\item{lty.dot.line}{
  if \code{setTrellis.=TRUE}, the line type for dot plot reference lines (default = solid line)
}
\item{lwd.dot.line}{
  if \code{setTrellis.=TRUE}, the line width for dot plot reference lines
  (default = 1)
}
\item{seqno}{
  if non-null, pastes the value of \code{seqno} at the end of the base of the
  file name, for \code{setps} and \code{setpdf}
}
\item{color}{
  set \code{color=TRUE} to use a color Trellis device instead of default of
  black and white, for \code{setps}.  For \code{setpdf} set to \code{TRUE}
  to get color pdf graphics.
}
\item{region}{
  see \code{pdf.graph}.  Default is to use an image region that is just large
  enough to contain the graphic.
}
\item{ratio}{
  ratio of width to height of the plot when only one of those is
  specified.  Defaults depend on whether S-Plus or R are being used.
}
\item{lref}{
  LaTeX reference point for \code{string}.  See the \code{psfrag} documentation
  referenced below.  Default is \code{"c"} for centered (this is also the
  default for \code{psref}).
}
\item{psref}{
  PostScript reference point.
}
\item{scale}{
  scall factor, default is 1
}
\item{srt}{
  rotation for \code{string} in degrees (default is zero)
}}
\value{
  nothing, for most of the functions.  \code{tex} returns a modified
  character string.
}
\section{Side Effects}{
  Starts a postscript file or a process to convert it to pcx format, or
  starts a Trellis postscript device.
  \code{ps.slide} Stores a system option \code{ps.slide.file}.  \code{pdf.graph} opens
  a graphics file using \code{pdf.graph}.  \code{setps} creates a function \code{topdf}
  in frame 0 (the session database).
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
\code{\link{mgp.axis.labels}}, \code{\link{pdf}}, \code{\link[lattice]{trellis.device}}, \code{\link{setTrellis}}
}
\examples{
\dontrun{
ps.slide("myslide")   # myslide is file name prefix
# use ps.slide("myslide",back="green") to use e.g. green background
plot(x, y)
title("My Title")


ps.slide(view=TRUE)   # makes myslide.ps file
                   # use ps.slide(close=TRUE) to close file without viewing with
                   # ghostview.
ps.slide(view=TRUE, pcx=TRUE)
                   # converts myslide.ps into myslide.pcx (PC Paintbrush
                   # format suitable for importing in PC graphics packages)
mgp.axis.labels(c(.4,1.2))  # override 2nd mgp parameters for x- and y axes
mgp.axis.labels(type='x')   # retrieve 3 mgp parameters for x-axis


setps(myfile)      # equiv. to setps('myfile', type='char')
                   # setps(myfile, trellis=TRUE, other args) for Trellis
# plotting commands
dev.off()
topdf()            # topdf created by setps
                   # makes Ghostscript create "myfile.pdf"
setpdf(myfile)
# plotting commands
dev.off()


# Put math and Greek symbols in a graph
setps(test)
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
