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
\email{fh@fharrell.com}
\cr
Earl Bellinger
\cr
Max Planck Institute
\cr
\email{earlbellinger@gmail.com}
\cr
Viktor Horvath
\cr
Brandeis University
\cr
\email{vhorvath@brandeis.edu}
}
\seealso{
\code{\link{axis}}
}
\examples{
# Plot with default settings
plot(runif(20), runif(20))
minor.tick()

# Plot with arguments passed to axis()
plot(c(0,1), c(0,1), type = 'n', axes = FALSE, ann = FALSE)
# setting up a plot without axes and annotation
points(runif(20), runif(20))                       # plotting data
axis(1, pos = 0.5, lwd = 2)                        # showing X-axis at Y = 0.5 with formatting
axis(2, col = 2)                                   # formatted Y-axis
minor.tick( nx = 4, ny = 4, tick.ratio = 0.3,
            x.args = list(pos = 0.5, lwd = 2),     # X-minor tick format argumnets
            y.args = list(col = 2))                # Y-minor tick format arguments
}
\keyword{aplot}
\keyword{hplot}
