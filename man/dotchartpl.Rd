\name{dotchartpl}
\alias{dotchartpl}
\title{Enhanced Version of dotchart Function for plotly}
\description{
	This function produces a \code{plotly} interactive graphic and accepts
	a different format of data input than the other \code{dotchart}x
	functions.  It was written to handle a hierarchical data structure
	including strata that further subdivide the main classes.  Strata,
	indicated by the \code{mult} variable, are shown  on the same
	horizontal line, and if the variable \code{big} is \code{FALSE} will
	appear slightly below the main line, using smaller symbols, and having
	some transparency.  This is intended to handle output such as that
	from the \code{summaryP} function when there is a superpositioning
	variable \code{group} and a stratification variable \code{mult},
	especially when the data have been run through the \code{addMarginal}
	function to create \code{mult} categories labelled \code{"All"} for
	which the user will specify \code{big=TRUE} to indicate non-stratified
	estimates (stratified only on \code{group}) to emphasize.

	When viewing graphics that used \code{mult} and \code{big}, the user
	can click on the legends for the small points for \code{group}s to
	vanish the finely stratified estimates.

	When \code{group} is used by \code{mult} and \code{big} are not, and
	when the \code{group} variable has exactly two distinct values, you
	can specify \code{refgroup} to get the difference between two
	proportions in addition to the individual proportions.  The individual
	proportions are plotted, but confidence intervals for the difference
	are shown in hover text and half-width confidence intervals for the
	difference, centered at the midpoint of the proportions, are shown.
	These have the property of intersecting the two proportions if and
	only if there is no significant difference at the \code{1 - conf.int}
	level.
	}
\usage{
dotchartpl(x, major, minor=NULL, group=NULL, mult=NULL,
           big=NULL, htext=NULL, num=NULL, denom=NULL,
           lower=NULL, upper=NULL,
           refgroup=NULL, sortdiff=TRUE, conf.int=0.95,
           minkeep=NA, xlim=NULL, xlab='Proportion',
           tracename=NULL, limitstracename=NULL,
           width=800,
           col=colorspace::rainbow_hcl)
}
\arguments{
  \item{x}{a numeric vector used for values on the \code{x}-axis}
	\item{major}{major vertical category, e.g., variable labels}
	\item{minor}{minor vertical category, e.g. category levels within
		variables}
	\item{group}{superpositioning variable such as treatment}
	\item{mult}{strata names for further subdivisions without
		\code{group}s}
	\item{big}{omit if all levels of \code{mult} are equally important or
		if \code{mult} is omitted.  Otherwise denotes major (larger points,
		right on horizontal lines) vs. minor (smaller, transparent points
		slightly below the line).}
	\item{htext}{additional hover text per point}
	\item{num}{if \code{x} represents proportions, optinally specifies to
		numerators, to be used in fractions added to hover text.  When
		\code{num} is given, \code{x} is automatically added to hover text,
		rounded to 3 digits after the decimal point.}
	\item{denom}{like \code{num} but for denominators}
	\item{lower}{lower limits for optional error bars}
	\item{upper}{upper limits for optional error bars}
	\item{refgroup}{if \code{group} is specified and there are exactly two
	groups, specify the character string for the reference group in
	computing difference in proportions.  For example if
	\code{refgroup='A'} and the \code{group} levels are \code{'A','B'},
	you will get B - A.}
  \item{sortdiff}{\code{minor} categories are sorted by descending
	values of the difference in proportions when \code{refgroup} is used,
	unless you specify \code{sortdiff=FALSE}}
  \item{conf.int}{confidence level for computing confidence intervals
	for the difference in two proportions.  Specify \code{conf.int=FALSE}
	to suppress confidence intervals.}
  \item{minkeep}{if \code{refgroup} and \code{minkeep} are both given,
		observations that are at or above \code{minkeep} for at least one of
		the groups are retained.  The defaults to to keep all observations.}
	\item{xlim}{\code{x}-axis limits}
	\item{xlab}{\code{x}-axis label}
	\item{tracename}{\code{plotly} trace name if \code{group} is not used}
	\item{limitstracename}{\code{plotly} trace name for \code{lower} and
		\code{upper} if \code{group} is not used}
	\item{col}{a function or vector of colors to assign to \code{group}.
	If a function it will be evaluated with an argument equal to the
	number of distinct groups.}
	\item{width}{width of plot in pixels}
	}
\value{a \code{plotly} object.  An attribute \code{levelsRemoved} is
	added if \code{minkeep} is used and any categories were omitted from
	the plot as a result.  This is a character vector with categories
	removed.  If \code{major} is present, the strings are of the form
	\code{major:minor}}
\author{Frank Harrell}
\seealso{\code{\link{dotchartp}}}
\examples{
\dontrun{
set.seed(1)
d <- expand.grid(major=c('Alabama', 'Alaska', 'Arkansas'),
                 minor=c('East', 'West'),
                 group=c('Female', 'Male'),
                 city=0:2)
n <- nrow(d)
d$num   <- round(100*runif(n))
d$denom <- d$num + round(100*runif(n))
d$x     <- d$num / d$denom
d$lower <- d$x - runif(n)
d$upper <- d$x + runif(n)

with(d,
 dotchartpl(x, major, minor, group, city, lower=lower, upper=upper,
            big=city==0, num=num, denom=denom, xlab='x'))

# Show half-width confidence intervals for Female - Male differences
# after subsetting the data to have only one record per
# state/region/group
d <- subset(d, city == 0)
with(d,
 dotchartpl(x, major, minor, group, num=num, denom=denom, refgroup='Male')
)

n <- 500
set.seed(1)
d <- data.frame(
  race         = sample(c('Asian', 'Black/AA', 'White'), n, TRUE),
  sex          = sample(c('Female', 'Male'), n, TRUE),
  treat        = sample(c('A', 'B'), n, TRUE),
  smoking      = sample(c('Smoker', 'Non-smoker'), n, TRUE),
  hypertension = sample(c('Hypertensive', 'Non-Hypertensive'), n, TRUE),
  region       = sample(c('North America','Europe','South America',
                          'Europe', 'Asia', 'Central America'), n, TRUE))

d <- upData(d, labels=c(race='Race', sex='Sex'))

dm <- addMarginal(d, region)
s <- summaryP(race + sex + smoking + hypertension ~
                region + treat,  data=dm)

s$region <- ifelse(s$region == 'All', 'All Regions', as.character(s$region))

with(s, 
 dotchartpl(freq / denom, major=var, minor=val, group=treat, mult=region,
            big=region == 'All Regions', num=freq, denom=denom)
)

s2 <- s[- attr(s, 'rows.to.exclude1'), ]
with(s2, 
     dotchartpl(freq / denom, major=var, minor=val, group=treat, mult=region,
                big=region == 'All Regions', num=freq, denom=denom)
)
# Note these plots can be created by plot.summaryP when options(grType='plotly')
}
}
\keyword{hplot}
