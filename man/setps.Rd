\name{setps}
\alias{setps}
\alias{topdf}
\title{
  Makes small postscript plots with minimal surrounding white space
}
\description{
  \code{setps} is a function that makes small postscript plots with
  minimal surrounding white space, suitable for inclusion in books and
  reports. Internally \code{setps} uses (and defines) the \code{psfig}
  function by Antonio Possolo (\email{antonio@atc.boeing.com}).
  \code{setps} is especially good for including plots in LaTeX.
  \code{setps} creates a temporary function in the session database that
  when invoked will convert a completed postscript graphics file to a
  Adobe Acrobat \file{.pdf} if you have Ghostscript installed and in
  your path (so that the \command{gs} command is available in UNIX or
  \command{gswin32c} is available for Windows/NT).  Invoke \code{topdf}
  by the command \code{topdf()}, or, if you want to convert a graphic
  other than the last one created, run \code{topdf(filename)} to convert
  \file{filename.ps} to \file{filename.pdf}.  If \code{trellis=TRUE},
  \code{setps} invokes \code{\link[lattice]{trellis.device}} with a
  \code{postscript} device argument, and it does not set any of the
  \code{par} parameters.  Arguments 3, 4, 5, 7, 9, and 10 to
  \code{setps} are ignored if \code{trellis=TRUE}.  If
  \code{options(setpsPrefix="name")} is defined, the \samp{name} string
  will be prefixed to the file name used by \code{setps}.  \code{setpdf}
  uses a similar option \code{setpdfPrefix}.  \code{setps} and
  \code{setpdf} set \code{par(mgp=c(2,0.4,0))} if \code{trellis=FALSE}.
}
\usage{
setps(filename, w=0, h=3, pointsize=10, sublines=0, toplines=0,
      type="symbol", lwd=2, font="Helvetica",
      leftlines=0, las=1,
      trellis=!(missing(setTrellis.) & missing(strip.blank) &
                missing(lty.dot.line) & missing(lwd.dot.line)), 
      setTrellis.=TRUE, 
      strip.blank =TRUE, lty.dot.line = 1, lwd.dot.line = 1,
      seqno=NULL, color=FALSE)
}
\arguments{
  \item{filename}{
    name or character string or character vector specifying file
    prefix. Specify \code{type="char"} if this is a character vector or
    a quote-delimited character string.
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
  \item{lwd}{
    line width
  }
  \item{type}{
    For \code{setps} \code{type="char"} specifies that  the
    \code{filename} argument is a character string  or vector, and any
    other value indicatesthat it is an unquoted name.
  }
  \item{las}{
    set to \code{0} to have axis labels always parallel to the axis,
    \code{1} for always horizontal, \code{2} for perpendicular toaxis
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
}
\section{Side Effects}{
  \code{setps} creates a function \code{topdf} in frame 0 (the session
  database).
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
setps(myfile)      # equiv. to setps('myfile', type='char')
                   # setps(myfile, trellis=TRUE, other args) for Trellis
# plotting commands
dev.off()
topdf()            # topdf created by setps
                   # makes Ghostscript create "myfile.pdf"
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
