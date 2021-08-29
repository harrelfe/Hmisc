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
	arranged around 0.5.  If \code{options(grType='plotly')} is in effect,
	uses \code{plotly} graphics instead of \code{grid} or base graphics.
	For \code{plotly} you may want to set the list of possible colors,
	etc. using \code{pobj=plot_ly(colors=...)}.  \code{lwd,lty,lwd.vert}
	are ignored under \code{plotly}.
}
\usage{
multLines(x, y, pos = c('left', 'right'), col='gray',
          lwd=1, lty=1, lwd.vert = .85, lty.vert = 1,
          alpha = 0.4, grid = FALSE,
          pobj=plotly::plot_ly(), xlim, name=colnames(y)[1], legendgroup=name,
          showlegend=TRUE, ...)
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
	\item{pobj}{an already started \code{plotly} object to add to}
	\item{xlim}{global x-axis limits (required if using \code{plotly})}
	\item{name}{trace name if using \code{plotly}}
	\item{legendgroup}{legend group name if using \code{plotly}}
	\item{showlegend}{whether or not to show traces in legend, if using
		\code{plotly}} 
	\item{\dots}{passed to \code{add_lines} or \code{add_segments} if
		using \code{plotly}}
		
}
\author{Frank Harrell}
\examples{
if (requireNamespace("plotly")) {
  x <- 1:4
  y <- cbind(x, x-3, x-2, x-1, x+1, x+2, x+3)
  plot(NA, NA, xlim=c(1,4), ylim=c(-2, 7))
  multLines(x, y, col='blue')
  multLines(x, y, col='red', pos='right')
}
}
\keyword{hplot}
