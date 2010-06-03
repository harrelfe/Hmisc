\name{minor.tick}
\alias{minor.tick}
\title{
Minor Tick Marks
}
\description{
Adds minor tick marks to an existing plot.  All minor tick marks that
will fit on the axes will be drawn.
}
\usage{
minor.tick(nx=2, ny=2, tick.ratio=0.5)
}
\arguments{
\item{nx}{
number of intervals in which to divide the area between major tick marks on
the X-axis.  Set to 1 to suppress minor tick marks.
}
\item{ny}{
same as \code{nx} but for the Y-axis
}
\item{tick.ratio}{
ratio of lengths of minor tick marks to major tick marks.  The length
of major tick marks is retrieved from \code{par("tck")}.
}}
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
}
\seealso{
\code{\link{axis}}
}
\examples{
plot(runif(20),runif(20))
minor.tick()
}
\keyword{aplot}
\keyword{hplot}
% Converted by Sd2Rd version 1.21.
