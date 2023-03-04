\name{curveRep}
\alias{curveRep}
\alias{print.curveRep}
\alias{plot.curveRep}
\alias{curveSmooth}
\title{Representative Curves}
\description{\code{curveRep} finds representative curves from a
  relatively large collection of curves.  The curves usually represent
  time-response profiles as in serial (longitudinal or repeated) data
  with possibly unequal time points and greatly varying sample sizes per
  subject.  After excluding records containing missing \code{x} or
  \code{y}, records are first stratified into \code{kn} groups having similar
  sample sizes per curve (subject).  Within these strata, curves are
  next stratified according to the distribution of \code{x} points per
  curve (typically measurement times per subject).  The
  \code{\link[cluster]{clara}} clustering/partitioning function is used
  to do this, clustering on one, two, or three \code{x} characteristics
  depending on the minimum sample size in the current interval of sample
  size.  If the interval has a minimum number of unique \code{values} of
  one, clustering is done on the single \code{x} values.  If the minimum
  number of unique \code{x} values is two, clustering is done to create
  groups that are similar on both \code{min(x)} and \code{max(x)}.  For
  groups containing no fewer than three unique \code{x} values,
  clustering is done on the trio of values \code{min(x)}, \code{max(x)},
  and the longest gap between any successive \code{x}.  Then within
  sample size and \code{x} distribution strata, clustering of
  time-response profiles is based on \code{p} values of \code{y} all
  evaluated at the same \code{p} equally-spaced \code{x}'s within the
  stratum.  An option allows per-curve data to be smoothed with
  \code{\link{lowess}} before proceeding.  Outer \code{x} values are
  taken as extremes of \code{x} across all curves within the stratum.
  Linear interpolation within curves is used to estimate \code{y} at the
  grid of \code{x}'s.  For curves within the stratum that do not extend
  to the most extreme \code{x} values in that stratum, extrapolation
  uses flat lines from the observed extremes in the curve unless
  \code{extrap=TRUE}. The \code{p} \code{y} values are clustered using
  \code{\link[cluster]{clara}}.

  \code{print} and \code{plot} methods show results.  By specifying an
  auxiliary \code{idcol} variable to \code{plot}, other variables such
  as treatment may be depicted to allow the analyst to determine for
  example whether subjects on different treatments are assigned to
  different time-response profiles.  To write the frequencies of a
  variable such as treatment in the upper left corner of each panel
  (instead of the grand total number of clusters in that panel), specify
  \code{freq}.

  \code{curveSmooth} takes a set of curves and smooths them using
  \code{\link{lowess}}.  If the number of unique \code{x} points in a curve is
  less than \code{p}, the smooth is evaluated at the unique \code{x}
  values.  Otherwise it is evaluated at an equally spaced set of
  \code{x} points over the observed range.  If fewer than 3 unique
  \code{x} values are in a curve, those points are used and smoothing is not done.
}
\usage{
curveRep(x, y, id, kn = 5, kxdist = 5, k = 5, p = 5,
         force1 = TRUE, metric = c("euclidean", "manhattan"),
         smooth=FALSE, extrap=FALSE, pr=FALSE)

\method{print}{curveRep}(x, \dots)

\method{plot}{curveRep}(x, which=1:length(res),
                        method=c('all','lattice','data'),
                        m=NULL, probs=c(.5, .25, .75), nx=NULL, fill=TRUE,
                        idcol=NULL, freq=NULL, plotfreq=FALSE,
                        xlim=range(x), ylim=range(y),
                        xlab='x', ylab='y', colorfreq=FALSE, \dots)
curveSmooth(x, y, id, p=NULL, pr=TRUE)

}
\arguments{
  \item{x}{a numeric vector, typically measurement times.
	For \code{plot.curveRep} is an object created by \code{curveRep}.}
  \item{y}{a numeric vector of response values}
  \item{id}{a vector of curve (subject) identifiers, the same length as
	\code{x} and \code{y}}
  \item{kn}{number of curve sample size groups to construct.
	\code{curveRep} tries to divide the data into equal numbers of
	curves across sample size intervals.}
  \item{kxdist}{maximum number of x-distribution clusters to derive
	using \code{clara}}
  \item{k}{maximum number of x-y profile clusters to derive using \code{clara}}
  \item{p}{number of \code{x} points at which to interpolate \code{y}
	for profile clustering.  For \code{curveSmooth} is the number of
	equally spaced points at which to evaluate the lowess smooth, and if
  \code{p} is omitted the smooth is evaluated at the original \code{x}
  values (which will allow \code{curveRep} to still know the \code{x}
  distribution}
  \item{force1}{By default if any curves have only one point, all curves
	consisting of one point will be placed in a separate stratum.  To
	prevent this separation, set \code{force1 = FALSE}.}
  \item{metric}{see \code{\link[cluster]{clara}}}
  \item{smooth}{By default, linear interpolation is used on raw data to
	obtain \code{y} values to cluster to determine x-y profiles.
	Specify \code{smooth = TRUE} to replace observed points with
	\code{\link{lowess}} before computing \code{y} points on the grid.
	Also, when \code{smooth} is used, it may be desirable to use
	\code{extrap=TRUE}.}
  \item{extrap}{set to \code{TRUE} to use linear extrapolation to
	evaluate \code{y} points for x-y clustering.  Not recommended unless
	smoothing has been or is being done.}
  \item{pr}{set to \code{TRUE} to print progress notes}
  \item{which}{an integer vector specifying which sample size intervals
	to plot.  Must be specified if \code{method='lattice'} and must be a
	single number in that case.}
  \item{method}{The default makes individual plots of possibly all
	x-distribution by sample size by cluster combinations.  Fewer may be
	plotted by specifying \code{which}.  Specify \code{method='lattice'}
	to show a lattice \code{xyplot} of a single sample size interval,
	with x distributions going across and clusters going down.  To not
	plot but instead return a data frame for a single sample size
	interval, specify \code{method='data'}}
  \item{m}{the number of curves in a cluster to randomly sample if there
	are more than \code{m} in a cluster.  Default is to draw all curves
	in a cluster.  For \code{method = "lattice"} you can specify
	\code{m = "quantiles"} to use the \code{xYplot} function to show
	quantiles of \code{y} as a function of \code{x}, with the quantiles
	specified by the \code{probs} argument.  This cannot be used to draw
	a group containing \code{n = 1}.}
  \item{nx}{applies if \code{m = "quantiles"}.  See \code{\link{xYplot}}.}
  \item{probs}{3-vector of probabilities with the central quantile
	first.  Default uses quartiles.}
  \item{fill}{for \code{method = "all"}, by default if a sample size
	x-distribution stratum did not have enough curves to stratify into
	\code{k} x-y profiles, empty graphs are drawn so that a matrix of
	graphs will have the next row starting with a different sample size
	range or x-distribution.  See the example below.}
  \item{idcol}{a named vector to be used as a table lookup for color
	assignments (does not apply when \code{m = "quantile"}).  The names of
	this vector are curve \code{id}s and the values are color names or
	numbers.}
  \item{freq}{a named vector to be used as a table lookup for a grouping
	variable such as treatment.  The names are curve \code{id}s and
	values are any values useful for grouping in a frequency tabulation.}
  \item{plotfreq}{set to \code{TRUE} to plot the frequencies from the
	\code{freq} variable as horizontal bars instead of printing them.
	Applies only to \code{method = "lattice"}.  By default the largest bar
	is 0.1 times the length of a panel's x-axis.  Specify
	\code{plotfreq = 0.5} for example to make the longest bar half this long.}
  \item{colorfreq}{set to \code{TRUE} to color the frequencies printed by 
	\code{plotfreq} using the colors provided by \code{idcol}.}
  \item{xlim, ylim, xlab, ylab}{plotting parameters.  Default ranges are
	the ranges in the entire set of raw data given to \code{curveRep}.}
  \item{\dots}{arguments passed to other functions.}
}
\value{a list of class \code{"curveRep"} with the following elements
  \item{res}{a hierarchical list first split by sample size intervals,
	then by x distribution clusters, then containing a vector of cluster
  numbers with \code{id} values as a names attribute}
  \item{ns}{a table of frequencies of sample sizes per curve after
	removing \code{NA}s}
  \item{nomit}{total number of records excluded due to \code{NA}s}
  \item{missfreq}{a table of frequencies of number of \code{NA}s
	excluded per curve}
  \item{ncuts}{cut points for sample size intervals}
  \item{kn}{number of sample size intervals}
  \item{kxdist}{number of clusters on x distribution}
  \item{k}{number of clusters of curves within sample size and
	distribution groups}
  \item{p}{number of points at which to evaluate each curve for clustering}
  \item{x}{}
  \item{y}{}
  \item{id}{input data after removing \code{NA}s}
  \code{curveSmooth} returns a list with elements \code{x,y,id}.
}
\details{
  In the graph titles for the default graphic output, \code{n} refers to the
  minimum sample size, \code{x} refers to the sequential x-distribution
  cluster, and \code{c} refers to the sequential x-y profile cluster.  Graphs
  from \code{method = "lattice"} are produced by
  \code{\link[lattice]{xyplot}} and in the panel titles
  \code{distribution} refers to the x-distribution stratum and
  \code{cluster} refers to the x-y profile cluster.
}
\references{
  Segal M. (1994): Representative curves for longitudinal data via
  regression trees.  J Comp Graph Stat 3:214-233.

  
  Jones MC, Rice JA (1992): Displaying the important features of large
  collections of similar curves.  Am Statistician 46:140-145.

  
  Zheng X, Simpson JA, et al (2005): Data from a study of effectiveness
  suggested potential prognostic factors related to the patterns of
  shoulder pain.  J Clin Epi 58:823-830.
  }
\author{
  Frank Harrell\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{fh@fharrell.com}
}
\note{The references describe other methods for deriving
  representative curves, but those methods were not used here.  The last
  reference which used a cluster analysis on principal components
  motivated \code{curveRep} however.  The \code{kml} package does k-means clustering of longitudinal data with imputation.}
\seealso{\code{\link[cluster]{clara}},\code{\link[Hmisc]{dataRep}}}
\examples{
\dontrun{
# Simulate 200 curves with per-curve sample sizes ranging from 1 to 10
# Make curves with odd-numbered IDs have an x-distribution that is random
# uniform [0,1] and those with even-numbered IDs have an x-dist. that is
# half as wide but still centered at 0.5.  Shift y values higher with
# increasing IDs
set.seed(1)
N <- 200
nc <- sample(1:10, N, TRUE)
id <- rep(1:N, nc)
x <- y <- id
for(i in 1:N) {
  x[id==i] <- if(i \%\% 2) runif(nc[i]) else runif(nc[i], c(.25, .75))
  y[id==i] <- i + 10*(x[id==i] - .5) + runif(nc[i], -10, 10)
}

w <- curveRep(x, y, id, kxdist=2, p=10)
w
par(ask=TRUE, mfrow=c(4,5))
plot(w)                # show everything, profiles going across
par(mfrow=c(2,5))
plot(w,1)              # show n=1 results
# Use a color assignment table, assigning low curves to green and
# high to red.  Unique curve (subject) IDs are the names of the vector.
cols <- c(rep('green', N/2), rep('red', N/2))
names(cols) <- as.character(1:N)
plot(w, 3, idcol=cols)
par(ask=FALSE, mfrow=c(1,1))

plot(w, 1, 'lattice')  # show n=1 results
plot(w, 3, 'lattice')  # show n=4-5 results
plot(w, 3, 'lattice', idcol=cols)  # same but different color mapping
plot(w, 3, 'lattice', m=1)  # show a single "representative" curve
# Show median, 10th, and 90th percentiles of supposedly representative curves
plot(w, 3, 'lattice', m='quantiles', probs=c(.5,.1,.9))
# Same plot but with much less grouping of x variable
plot(w, 3, 'lattice', m='quantiles', probs=c(.5,.1,.9), nx=2)

# Use ggplot2 for one sample size interval
z <- plot(w, 2, 'data')
require(ggplot2)
ggplot(z, aes(x, y, color=curve)) + geom_line() +
       facet_grid(distribution ~ cluster) +
       theme(legend.position='none') +
       labs(caption=z$ninterval[1])


# Smooth data before profiling.  This allows later plotting to plot
# smoothed representative curves rather than raw curves (which
# specifying smooth=TRUE to curveRep would do, if curveSmooth was not used)
d <- curveSmooth(x, y, id)
w <- with(d, curveRep(x, y, id))

# Example to show that curveRep can cluster profiles correctly when
# there is no noise.  In the data there are four profiles - flat, flat
# at a higher mean y, linearly increasing then flat, and flat at the
# first height except for a sharp triangular peak

set.seed(1)
x <- 0:100
m <- length(x)
profile <- matrix(NA, nrow=m, ncol=4)
profile[,1] <- rep(0, m)
profile[,2] <- rep(3, m)
profile[,3] <- c(0:3, rep(3, m-4))
profile[,4] <- c(0,1,3,1,rep(0,m-4))
col <- c('black','blue','green','red')
matplot(x, profile, type='l', col=col)
xeval <- seq(0, 100, length.out=5)
s <- x %in% xeval
matplot(x[s], profile[s,], type='l', col=col)

id <- rep(1:100, each=m)
X <- Y <- id
cols <- character(100)
names(cols) <- as.character(1:100)
for(i in 1:100) {
  s <- id==i
  X[s] <- x
  j <- sample(1:4,1)
  Y[s] <- profile[,j]
  cols[i] <- col[j]
}
table(cols)
yl <- c(-1,4)
w <- curveRep(X, Y, id, kn=1, kxdist=1, k=4)
plot(w, 1, 'lattice', idcol=cols, ylim=yl)
# Found 4 clusters but two have same profile
w <- curveRep(X, Y, id, kn=1, kxdist=1, k=3)
plot(w, 1, 'lattice', idcol=cols, freq=cols, plotfreq=TRUE, ylim=yl)
# Incorrectly combined black and red because default value p=5 did
# not result in different profiles at x=xeval
w <- curveRep(X, Y, id, kn=1, kxdist=1, k=4, p=40)
plot(w, 1, 'lattice', idcol=cols, ylim=yl)
# Found correct clusters because evaluated curves at 40 equally
# spaced points and could find the sharp triangular peak in profile 4
}
}
\keyword{multivariate}
\keyword{hplot}
\concept{repeated measures}
\concept{longitudinal data}
\concept{serial data}
\concept{representative curves}
\concept{descriptive statistics}
\concept{exploratory data analysis}
