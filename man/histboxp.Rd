\name{histboxp}
\alias{histboxp}
\alias{histboxpM}
\alias{dhistboxp}
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

\code{histboxpM} plots multiple histograms stacked vertically, for
variables in a data frame having a common \code{group} variable (if any)
and combined using \code{plotly::subplot}.

\code{dhistboxp} is like \code{histboxp} but no \code{plotly} graphics
are actually drawn.  Instead, a data frame suitable for use with
\code{plotlyM} is returned.  For \code{dhistboxp} an additional level of
stratification \code{strata} is implemented.  \code{group} causes a
different result here to produce back-to-back histograms (in the case of
two groups) for each level of \code{strata}.
}
\usage{
histboxp(p = plotly::plot_ly(height=height), x, group = NULL,
         xlab=NULL, gmd=TRUE, sd=FALSE, bins = 100, wmax=190, mult=7,
         connect=TRUE, showlegend=TRUE)

dhistboxp(x, group = NULL, strata=NULL, xlab=NULL, 
          gmd=FALSE, sd=FALSE, bins = 100, nmin=5, ff1=1, ff2=1)

histboxpM(p=plotly::plot_ly(height=height, width=width), x, group=NULL,
          gmd=TRUE, sd=FALSE, width=NULL, nrows=NULL, ncols=NULL, ...)
}
\arguments{
  \item{p}{\code{plotly} graphics object if already begun}
  \item{x}{a numeric vector, or for \code{histboxpM} a numeric vector or
a data frame of numeric vectors, hopefully with \code{label} and
\code{units} attributes}
  \item{group}{a discrete grouping variable.  If omitted, defaults to a
		vector of ones}
	\item{strata}{a discrete numeric stratification variable.  Values are
          also used to space out different spike histograms.  Defaults
          to a vector of ones.}
	\item{xlab}{x-axis label, defaults to labelled version include units
		of measurement if any}
	\item{gmd}{set to \code{FALSE} to not compute Gini's mean difference}
	\item{sd}{set to \code{TRUE} to compute the SD}
	\item{width}{width in pixels}
	\item{nrows}{number of rows for layout of multiple plots}
	\item{ncols}{number of columns for layout of multiple plots.  At most
		one of \code{nrows,ncols} should be specified.}
  \item{bins}{number of equal-width bins to use for spike histogram.  If
		the number of distinct values of \code{x} is less than \code{bins},
		the actual values of \code{x} are used.}
	\item{nmin}{minimum number of non-missing observations for a
          group-stratum combination before the spike histogram and
          quantiles are drawn}
	\item{ff1,ff2}{fudge factors for position and bar length for spike histograms}
	\item{wmax,mult}{tweaks for margin to allocate}
	\item{connect}{set to \code{FALSE} to suppress lines connecting
		quantiles}
	\item{showlegend}{used if producing multiple plots to be combined with
		\code{subplot}; set to \code{FALSE} for all but one plot}
  \item{\dots}{other arguments for \code{histboxpM} that are passed to
\code{histboxp}}
}
\value{a \code{plotly} object.  For \code{dhistboxp} a data frame as
          expected by \code{plotlyM}}
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
X <- data.frame(x, x2=runif(length(x)))
histboxpM(x=X, group=Distribution, ncols=2)  # separate plots
}
}
\keyword{hplot}
