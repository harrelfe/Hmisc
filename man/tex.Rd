\name{tex}
\alias{tex}
\title{
  function for use in graphs that are used with the psfrag package in LaTeX
}
\description{
  \code{tex} is a little function to save typing when including TeX
  commands in graphs that are used with the psfrag package in LaTeX to
  typeset any LaTeX text inside a postscript graphic.  \code{tex}
  surrounds the input character string with \samp{\tex[options]\{\}}.
  This is especially useful for getting Greek letters and math symbols
  in postscript graphs.  By default \code{tex} returns a string with
  \code{psfrag} commands specifying that the string be centered, not
  rotated, and not specially enlarged or shrunk.
}
\usage{
tex(string, lref='c', psref='c', scale=1, srt=0)
}
\arguments{
  \item{string}{
    a character string to be processed by \code{psfrag} in LaTeX.
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
\value{
  \code{tex} returns a modified character string.
}
\author{
  Frank Harrell\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{fh@fharrell.com}
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
pdf('test.pdf')
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
