\name{minor.tick}
\alias{minor.tick}
\title{Minor Tick Marks}
\description{
Adds minor tick marks to an existing plot.  All minor tick marks that
will fit on the axes will be drawn.
}
\usage{
minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
}
\arguments{
\item{nx}{
number of intervals in which to divide the area between major tick marks on
the X-axis.  Set to 1 to suppress minor tick marks.
}
\item{ny}{
same as \code{nx} but for the Y-axis.
}
\item{tick.ratio}{
ratio of lengths of minor tick marks to major tick marks.  The length
of major tick marks is retrieved from \code{par("tck")}.}
\item{x.args}{
additionl arguments (e.g. \code{post}, \code{lwd}) used by \code{axis()} function when rendering the X-axis.}
\item{y.args}{
same as \code{x.args} but for Y-axis.}
}
\section{Side Effects}{
plots
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{f.harrell@vanderbilt.edu}
\cr
Earl Bellinger
\cr
Max Planck Institute
\cr
\email{earlbellinger@gmail.com}
}
\seealso{
\code{\link{axis}}
}
\examples{
plot(runif(20), runif(20))
minor.tick()
}
\keyword{aplot}
\keyword{hplot}
