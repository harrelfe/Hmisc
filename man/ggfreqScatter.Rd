\name{ggfreqScatter}
\alias{ggfreqScatter}
\title{Frequency Scatterplot}
\description{
Uses \code{ggplot2} to plot a scatterplot or dot-like chart for the case
where there is a very large number of overlapping values.  This works
for continuous and categorical \code{x} and \code{y}.  For continuous
variables it serves the same purpose as hexagonal binning.  Counts for
overlapping points are grouped into quantile groups and level of
transparency and rainbow colors are used to provide count information.

Instead, you can specify \code{stick=TRUE} not use color but to encode
	cell frequencies 
	with the height of a black line y-centered at the middle of the bins.
	Relative frequencies are not transformed, and the maximum cell
	frequency is shown in a caption.  Every point with at least a
	frequency of one is depicted with a full-height light gray vertical
	line, scaled to the above overall maximum frequency.  In this way to
	relative frequency is to proportion of these light gray lines that are
	black, and one can see points whose frequencies are too low to see the
	black lines.

The result can also be passed to \code{ggplotly}.  Actual cell
frequencies are added to the hover text in that case using the
\code{label} \code{ggplot2} aesthetic.
}
\usage{
ggfreqScatter(x, y, by=NULL, bins=50, g=10, cuts=NULL,
              xtrans = function(x) x,
              ytrans = function(y) y,
              xbreaks = pretty(x, 10),
              ybreaks = pretty(y, 10),
              xminor  = NULL, yminor = NULL,
              xlab = as.character(substitute(x)),
              ylab = as.character(substitute(y)),
              fcolors = viridisLite::viridis(10), nsize=FALSE,
              stick=FALSE, html=FALSE, prfreq=FALSE, \dots)
}
\arguments{
\item{x}{x-variable}
\item{y}{y-variable}
\item{by}{an optional vector used to make separate plots for each
	distinct value using \code{facet_wrap()}}
\item{bins}{for continuous \code{x} or \code{y} is the number of bins to
	create by rounding.  Ignored for categorical variables.  If a
	2-vector, the first element corresponds to \code{x} and the second to
	\code{y}.}
\item{g}{number of quantile groups to make for frequency counts.  Use
	\code{g=0} to use frequencies continuously for color 
	coding.  This is recommended only when using \code{plotly}.}
\item{cuts}{instead of using \code{g}, specify \code{cuts} to provide
	the vector of cuts for categorizing frequencies for assignment to colors}
\item{xtrans,ytrans}{functions specifying transformations to be made
	before binning and plotting}
\item{xbreaks,ybreaks}{vectors of values to label on axis, on original
	scale}
\item{xminor,yminor}{values at which to put minor tick marks, on
	original scale}
\item{xlab,ylab}{axis labels.  If not specified and variable has a
	\code{label}, thatu label will be used.}
\item{fcolors}{\code{colors} argument to pass to
	\code{scale_color_gradientn} to color code frequencies.  Use
	\code{fcolors=gray.colors(10, 0.75, 0)} to show gray
	scale, for example.  Another good choice is
	\code{fcolors=hcl.colors(10, 'Blue-Red')}.} 
\item{nsize}{set to \code{TRUE} to not vary color or transparency but
	instead to size the symbols in relation to the number of points.  Best
	with both \code{x} and \code{y} are discrete.  \code{ggplot2}
	\code{size} is taken as the fourth root of the frequency.  If there
	are 15 or unique frequencies all the unique frequencies are used,
	otherwise \code{g} quantile groups of frequencies are used.}
\item{stick}{set to \code{TRUE} to not use colors but instead use
	varying-height black vertical lines to depict cell frequencies.}
\item{html}{set to \code{TRUE} to use html in axis labels instead of
	plotmath}
\item{prfreq}{set to \code{TRUE} to print the frequency distributions of
	the binned coordinate frequencies}
\item{\dots}{arguments to pass to \code{geom_point} such as \code{shape}
	and \code{size}}
}
\value{a \code{ggplot} object}
\author{Frank Harrell}
\seealso{\code{\link[Hmisc]{cut2}}}
\examples{
require(ggplot2)
set.seed(1)
x <- rnorm(1000)
y <- rnorm(1000)
count <- sample(1:100, 1000, TRUE)
x <- rep(x, count)
y <- rep(y, count)
# color=alpha=NULL below makes loess smooth over all points
g <- ggfreqScatter(x, y) +   # might add g=0 if using plotly
      geom_smooth(aes(color=NULL, alpha=NULL), se=FALSE) +
      ggtitle("Using Deciles of Frequency Counts, 2500 Bins")
g
# plotly::ggplotly(g, tooltip='label')  # use plotly, hover text = freq. only
# Plotly makes it somewhat interactive, with hover text tooltips

# Instead use varying-height sticks to depict frequencies
ggfreqScatter(x, y, stick=TRUE) +
 labs(subtitle='Relative height of black lines to gray lines
is proportional to cell frequency.
Note that points with even tiny frequency are visable
(gray line with no visible black line).')


# Try with x categorical
x1 <- sample(c('cat', 'dog', 'giraffe'), length(x), TRUE)
ggfreqScatter(x1, y)

# Try with y categorical
y1 <- sample(LETTERS[1:10], length(x), TRUE)
ggfreqScatter(x, y1)

# Both categorical, larger point symbols, box instead of circle
ggfreqScatter(x1, y1, shape=15, size=7)
# Vary box size instead
ggfreqScatter(x1, y1, nsize=TRUE, shape=15)
}
\keyword{hplot}
\concept{grouping}
\concept{categorization}
\concept{discretization}
