\name{summaryP}
\alias{summaryP}
\alias{plot.summaryP}
\alias{ggplot.summaryP}
\alias{latex.summaryP}
\title{Multi-way Summary of Proportions}
\description{
	\code{summaryP} produces a tall and thin data frame containing
	numerators (\code{freq}) and denominators (\code{denom}) after
	stratifying the data by a series of variables.  A special capability
	to group a series of related yes/no variables is included through the
	use of the \code{\link{ynbind}} function, for which the user specials a final
	argument \code{label} used to label the panel created for that group
	of related variables.
	
	If \code{options(grType='plotly')} is not in effect,
	the \code{plot} method for \code{summaryP}	displays proportions as a
	multi-panel dot chart using the \code{lattice} package's \code{dotplot}
	function with a special \code{panel} function.  Numerators and
	denominators of proportions are also included as text, in the same
	colors as used by an optional \code{groups} variable.  The
	\code{formula} argument used in the \code{dotplot} call is constructed,
	but the user can easily reorder the variables by specifying
	\code{formula}, with elements named \code{val} (category levels),
	\code{var} (classification variable name), \code{freq} (calculated
	result) plus the overall cross-classification variables excluding
	\code{groups}.  If \code{options(grType='plotly')} is in effect, the
	\code{plot} method makes an entirely different display using
	\code{Hmisc::dotchartpl} with \code{plotly} if \code{marginVal} is
	specified, whereby a stratification
	variable causes more finely stratified estimates to be shown slightly
	below the lines, with smaller and translucent symbols if \code{data}
	has been run through \code{addMarginal}.  The marginal summaries are
	shown as the main estimates and the user can turn off display of the
	stratified estimates, or view their details with hover text.

	The \code{ggplot} method for \code{summaryP} does not draw numerators
	and denominators but the chart is more compact than using the
	\code{plot} method with base graphics because \code{ggplot2}
	does not repeat category names the same way as \code{lattice} does.
	Variable names that are too long to fit in panel strips are renamed
	(1), (2), etc. and an attribute \code{"fnvar"} is added to the result;
	this attribute is a character string defining the abbreviations,
	useful in a figure caption.  The \code{ggplot2} object has
	\code{label}s for points plotted, used by \code{plotly::ggplotly} as
	hover text (see example).

	The \code{latex} method produces one or more LaTeX \code{tabular}s
	containing a table representation of the result, with optional
	side-by-side display if \code{groups} is specified.  Multiple
	\code{tabular}s result from the presence of non-group stratification
	factors.
}
\usage{
summaryP(formula, data = NULL, subset = NULL,
         na.action = na.retain, sort=TRUE,
         asna = c("unknown", "unspecified"), \dots)
\method{plot}{summaryP}(x, formula=NULL, groups=NULL,
         marginVal=NULL, marginLabel=marginVal,
         refgroup=NULL, exclude1=TRUE,  xlim = c(-.05, 1.05),
         text.at=NULL, cex.values = 0.5,
         key = list(columns = length(groupslevels), x = 0.75,
                    y = -0.04, cex = 0.9,
                    col = lattice::trellis.par.get('superpose.symbol')$col,
                    corner=c(0,1)),
         outerlabels=TRUE, autoarrange=TRUE,
         col=colorspace::rainbow_hcl, \dots)
\method{ggplot}{summaryP}(data, mapping, groups=NULL, exclude1=TRUE,
           xlim=c(0, 1), col=NULL, shape=NULL, size=function(n) n ^ (1/4),
           sizerange=NULL, abblen=5, autoarrange=TRUE, addlayer=NULL,
           \dots, environment)
\method{latex}{summaryP}(object, groups=NULL, exclude1=TRUE, file='', round=3,
                           size=NULL, append=TRUE, \dots)
}
\arguments{
  \item{formula}{a formula with the variables for whose levels
		proportions are computed on the left hand side, and major
		classification variables on the right.  The formula need to include
		any variable later used as \code{groups}, as the data summarization
		does not distinguish between superpositioning and paneling.  For the
	plot method, \code{formula} can provide an overall to the default
	formula for \code{dotplot()}.}
  \item{data}{an optional data frame.  For \code{ggplot.summaryP}
	  \code{data} is the result of \code{summaryP}.}
  \item{subset}{an optional subsetting expression or vector}
  \item{na.action}{function specifying how to handle \code{NA}s.  The
		default is to keep all \code{NA}s in the analysis frame.}
  \item{sort}{set to \code{FALSE} to not sort category levels in
		descending order of global proportions}
  \item{asna}{character vector specifying level names to consider the
		same as \code{NA}.  Set \code{asna=NULL} to not consider any.}
	\item{x}{an object produced by \code{summaryP}}
  \item{groups}{a character string containing the name of a
   	superpositioning variable for obtaining 
		further stratification within a horizontal line in the dot chart.}
	\item{marginVal}{if \code{options(grType='plotly')} is in effect and
	the data given to \code{summaryP} were run through \code{addMarginal},
	specifies the category name that represents marginal summaries
	(usually \code{"All"}).}
  \item{marginLabel}{specifies a different character string to use than
	the value of \code{marginVal}.  For example, if marginal proportions
	were computed over all \code{region}s, one may specify
	\code{marginVal="All", marginLabel="All Regions"}.  \code{marginLabel}
	is only used for formatting graphical output.}
  \item{refgroup}{used when doing a \code{plotly} chart and a two-level
	 group variable was used, resulting in the half-width confidence
  	interval for the difference in two proportions to be shown, and the
		actual confidence limits and the difference added to hover text.  See
	\code{dotchartpl} for more details.}
  \item{exclude1}{By default, \code{ggplot}, \code{plot}, and
	\code{latex} methods for \code{summaryP} remove redundant entries 
		from tables for variables with only two levels.  For example, if you
		print the proportion of females, you don't need to print the
		proportion of males.  To override this, set \code{exclude1=FALSE}.}
  \item{xlim}{\code{x}-axis limits.  Default is \code{c(0,1)}.}
	\item{text.at}{specify to leave unused space to the right of each
	panel to prevent numerators and denominators from touching data
	points.  \code{text.at} is the upper limit for scaling panels'
	\code{x}-axes but tick marks are only labeled up to \code{max(xlim)}.}
  \item{cex.values}{character size to use for plotting numerators and
		denominators}
  \item{key}{a list to pass to the \code{auto.key} argument of
		\code{dotplot}.  To place a key above the entire chart use
		\code{auto.key=list(columns=2)} for example.}
	\item{outerlabels}{by default if there are two conditioning variables
		besides \code{groups}, the \code{latticeExtra} package's
		\code{useOuterStrips} function is used to put strip labels in the
		margins, usually resulting in a much prettier chart.  Set to
		\code{FALSE} to prevent usage of \code{useOuterStrips}.}
	\item{autoarrange}{If \code{TRUE}, the formula is re-arranged so that
 	 if there are two conditioning (paneling) variables, the variable with
	 the most levels is taken as the vertical condition.}
 \item{col}{a vector of colors to use to override defaults in
	 \code{ggplot}.  When \code{options(grType='plotly')}, see \code{dotchartpl}.}
 \item{shape}{a vector of plotting symbols to override \code{ggplot}
	 defaults}
 \item{mapping, environment}{not used; needed because of rules for generics}
 \item{size}{for \code{ggplot}, a function that transforms denominators
	into metrics used for the \code{size} aesthetic.  Default is the
	fourth root function so that the area of symbols is proportional to
	the square root of sample size.  Specify \code{NULL} to not vary point
	sizes. 	\code{size=sqrt} is a reasonable 	alternative.  Set
	\code{size} to an integer to categorize the denominators into
	\code{size} quantile groups using \code{cut2}. Unless \code{size} is
	an integer, the legend for sizes uses the minimum and maximum
	denominators and 6-tiles using \code{quantile(..., type=1)} so that
	actually occurring sample sizes are used as labels.  \code{size} is
	overridden to \code{NULL} if the range in denominators is less than 10
	or the ratio of the maximum to the minimum is less than 1.2.
	For	\code{latex}, \code{size} is an optional font size such as
	\code{"small"}}  
 \item{sizerange}{a 2-vector specifying the \code{range} argument to the
	\code{ggplot2} \code{scale_size_...} function, which is the
	range of sizes allowed for the points according to the denominator.
	The default is \code{sizerange=c(.7, 3.25)} but the lower limit is
	increased according to the ratio of maximum to minimum sample sizes.}
 \item{abblen}{labels of variables having only one level and having
	their name longer than \code{abblen} characters are 
	abbreviated and documented in \code{fnvar} (described elsewhere
	here).  The default \code{abblen=5} is good for labels plotted
	vertically.  If labels are rotated using \code{theme} a better value
	would be 12.}
 \item{\dots}{used only for \code{plotly} graphics and these arguments
	are passed to \code{dotchartpl}}
 \item{object}{an object produced by \code{summaryP}}
 \item{file}{file name, defaults to writing to console}
 \item{round}{number of digits to the right of the decimal place for
	 proportions}
 \item{append}{set to \code{FALSE} to start output over}
 \item{addlayer}{a \code{ggplot} layer to add to the plot object}
}
\value{\code{summaryP} produces a data frame of class
	\code{"summaryP"}.  The \code{plot} method produces a \code{lattice}
	object of class \code{"trellis"}.  The \code{latex} method produces an
	object of class \code{"latex"} with an additional attribute
	\code{ngrouplevels} specifying the number of levels of any
	\code{groups} variable and an attribute \code{nstrata} specifying the
	number of strata.
	}
\author{Frank Harrell
  \cr
  Department of Biostatistics
  \cr
  Vanderbilt University
  \cr
  \email{fh@fharrell.com}}
\seealso{\code{\link{bpplotM}}, \code{\link{summaryM}},
	\code{\link{ynbind}}, \code{\link{pBlock}},
	\code{\link[ggplot2]{ggplot}}, \code{\link{colorFacet}}
}
\examples{
n <- 100
f <- function(na=FALSE) {
  x <- sample(c('N', 'Y'), n, TRUE)
  if(na) x[runif(100) < .1] <- NA
  x
}
set.seed(1)
d <- data.frame(x1=f(), x2=f(), x3=f(), x4=f(), x5=f(), x6=f(), x7=f(TRUE),
                age=rnorm(n, 50, 10),
                race=sample(c('Asian', 'Black/AA', 'White'), n, TRUE),
                sex=sample(c('Female', 'Male'), n, TRUE),
                treat=sample(c('A', 'B'), n, TRUE),
                region=sample(c('North America','Europe'), n, TRUE))
d <- upData(d, labels=c(x1='MI', x2='Stroke', x3='AKI', x4='Migraines',
                 x5='Pregnant', x6='Other event', x7='MD withdrawal',
                 race='Race', sex='Sex'))
dasna <- subset(d, region=='North America')
with(dasna, table(race, treat))
s <- summaryP(race + sex + ynbind(x1, x2, x3, x4, x5, x6, x7, label='Exclusions') ~
              region + treat, data=d)
# add exclude1=FALSE below to include female category
plot(s, groups='treat')
require(ggplot2)
ggplot(s, groups='treat')

plot(s, val ~ freq | region * var, groups='treat', outerlabels=FALSE)
# Much better looking if omit outerlabels=FALSE; see output at
# https://hbiostat.org/R/Hmisc/summaryFuns.pdf
# See more examples under bpplotM

## For plotly interactive graphic that does not handle variable size
## panels well:
## require(plotly)
## g <- ggplot(s, groups='treat')
## ggplotly(g, tooltip='text')

## For nice plotly interactive graphic:
## options(grType='plotly')
## s <- summaryP(race + sex + ynbind(x1, x2, x3, x4, x5, x6, x7,
##                                   label='Exclusions') ~
##               treat, data=subset(d, region='Europe'))
##
## plot(s, groups='treat', refgroup='A')  # refgroup='A' does B-A differences


# Make a chart where there is a block of variables that
# are only analyzed for males.  Keep redundant sex in block for demo.
# Leave extra space for numerators, denominators
sb <- summaryP(race + sex +
               pBlock(race, sex, label='Race: Males', subset=sex=='Male') ~
               region, data=d)
plot(sb, text.at=1.3)
plot(sb, groups='region', layout=c(1,3), key=list(space='top'),
     text.at=1.15)
ggplot(sb, groups='region')
\dontrun{
plot(s, groups='treat')
# plot(s, groups='treat', outerlabels=FALSE) for standard lattice output
plot(s, groups='region', key=list(columns=2, space='bottom'))
require(ggplot2)
colorFacet(ggplot(s))

plot(summaryP(race + sex ~ region, data=d), exclude1=FALSE, col='green')

require(lattice)
# Make your own plot using data frame created by summaryP
useOuterStrips(dotplot(val ~ freq | region * var, groups=treat, data=s,
        xlim=c(0,1), scales=list(y='free', rot=0), xlab='Fraction',
        panel=function(x, y, subscripts, ...) {
          denom <- s$denom[subscripts]
          x <- x / denom
          panel.dotplot(x=x, y=y, subscripts=subscripts, ...) }))

# Show marginal summary for all regions combined
s <- summaryP(race + sex ~ region, data=addMarginal(d, region))
plot(s, groups='region', key=list(space='top'), layout=c(1,2))

# Show marginal summaries for both race and sex
s <- summaryP(ynbind(x1, x2, x3, x4, label='Exclusions', sort=FALSE) ~
              race + sex, data=addMarginal(d, race, sex))
plot(s, val ~ freq | sex*race)
}
}
\keyword{hplot}
\keyword{category}
\keyword{manip}
\concept{grouping}
\concept{stratification}
\concept{aggregation}
\concept{cross-classification}
