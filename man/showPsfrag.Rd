\name{showPsfrag}
\alias{showPsfrag}
\title{
  Display image from psfrag LaTeX strings
}
\description{
  \code{showPsfrag} is used to display (using ghostview) a postscript
  image that contained psfrag LaTeX strings, by building a small LaTeX
  script and running \command{latex} and \command{dvips}.
}
\usage{
showPsfrag(filename)
}
\arguments{
  \item{filename}{
    name or character string or character vector specifying file
    prefix.
  }
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
\keyword{hplot}
\keyword{device}
\concept{trellis}
\concept{lattice}
