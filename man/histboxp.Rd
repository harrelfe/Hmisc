\name{histboxp}
\alias{histboxp}
\title{Use plotly to Draw Stratified Spike Histogram and Box Plot Statistics}
\description{
Uses \code{plotly} to draw horizontal spike histograms stratified by
\code{group}, plus the mean (solid dot) and vertical bars for these
quantiles: 0.05 (red, short), 0.25 (blue, medium), 0.50 (black, long),
0.75 (blue, medium), and 0.95 (red, short).  The robust dispersion measure
Gini's mean difference and the SD may optionally be added.  These are
shown as horizontal lines starting at the minimum value of \code{x}
having a length equal to the mean difference or SD.  Even when Gini's
and SD are computed, they are not drawn unless the user clicks on their
legend entry.

Spike histograms have the advantage of effectively showing the raw data for both
small and huge datasets, and unlike box plots allow multi-modality to be
easily seen.
}
\usage{
histboxp(p = plotly::plot_ly(height=height), x, group = NULL,
         xlab=NULL, gmd=TRUE, sd=FALSE, bins = 100, mult=7)
}
\arguments{
  \item{p}{\code{plotly} graphics object if already begun}
  \item{x}{a numeric vector}
  \item{group}{a discrete grouping variable.  If omitted, defaults to a
		vector of ones}
	\item{xlab}{x-axis label, defaults to labelled version include units
		of measurement if any}
	\item{gmd}{set to \code{FALSE} to not compute Gini's mean difference}
	\item{sd}{set to \code{TRUE} to compute the SD}
  \item{bins}{number of equal-width bins to use for spike histogram.  If
		the number of distinct values of \code{x} is less than \code{bins},
		the actual values of \code{x} are used.}
	\item{wmax,mult}{tweaks for margin to allocate}
}
\value{a \code{plotly} object}
\author{Frank Harrell}

\seealso{\code{\link{histSpike}}, \code{\link{plot.describe}},
	\code{\link{scat1d}}} 
\examples{
\dontrun{
dist <- c(rep(1, 500), rep(2, 250), rep(3, 600))
Distribution <- factor(dist, 1 : 3, c('Unimodal', 'Bimodal', 'Trimodal'))
x <- c(rnorm(500, 6, 1),
       rnorm(200, 3, .7), rnorm(50, 7, .4),
       rnorm(200, 2, .7), rnorm(300, 5.5, .4), rnorm(100, 8, .4))
histboxp(x=x, group=Distribution, sd=TRUE)
}
}
\keyword{hplot}
