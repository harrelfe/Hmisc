\name{multLines}
\alias{multLines}
\title{Plot Multiple Lines}
\description{
	Plots multiple lines based on a vector \code{x} and a matrix \code{y},
	draws thin vertical lines connecting limits represented by columns of
	\code{y} beyond the first.  It is assumed that either (1) the second
	and third columns of \code{y} represent lower and upper confidence
	limits, or that (2) there is an even number of columns beyond the
	first and these represent ascending quantiles that are symmetrically
	arranged around 0.5.
}
\usage{
multLines(x, y, pos = c('left', 'right'), col='gray',
          lwd=1, lty=1, lwd.vert = .85, lty.vert = 1,
          alpha = 0.4, grid = FALSE)
}
\arguments{
  \item{x}{a numeric vector}
  \item{y}{a numeric matrix with number of rows equal to the number of
		\code{x} elements}
	\item{pos}{when \code{pos='left'} the vertical lines are drawn, right
		to left, to the left of the point \code{(x, y[,1)}.  Otherwise lines
		are drawn left to right to the right of the point.}
  \item{col}{a color used to connect \code{(x, y[,1])} pairs.  The same
		color but with transparency given by the \code{alpha} argument is
		used to draw the vertical lines}
  \item{lwd}{line width for main lines}
  \item{lty}{line types for main lines}
	\item{lwd.vert}{line width for vertical lines}
	\item{lty.vert}{line type for vertical lines}
  \item{alpha}{transparency}
  \item{grid}{set to \code{TRUE} when using \code{grid}/\code{lattice}}
}
\author{Frank Harrell}
\examples{
x <- 1:4
y <- cbind(x, x-3, x-2, x-1, x+1, x+2, x+3)
plot(NA, NA, xlim=c(1,4), ylim=c(-2, 7))
multLines(x, y, col='blue')
multLines(x, y, col='red', pos='right')
}
\keyword{hplot}
