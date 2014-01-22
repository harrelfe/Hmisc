\name{summaryP}
\alias{summaryP}
\alias{plot.summaryP}
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
	
	The \code{plot} method for \code{summaryP}	displays proportions as a
	multi-panel dot chart using the \code{lattice} package's \code{dotplot}
	function with a special \code{panel} function.  Numerators and
	denominators of proportions are also included as text, in the same
	colors as used by an optional \code{groups} variable.  The
	\code{formula} argument used in the \code{dotplot} call is constructed,
	but the user can easily reorder the variables by specifying
	\code{formula}, with elements named \code{val} (category levels),
	\code{var} (classification variable name), \code{freq} (calculated
	result) plus the overall cross-classification variables excluding
	\code{groups}.

	The \code{latex} method produces one or more LaTeX \code{tabular}s
	containing a table representation of the result, with optional
	side-by-side display if \code{groups} is specified.  Multiple
	\code{tabular}s result from the presence of non-group stratification
	factors.
}
\usage{
summaryP(formula, data = NULL, subset = NULL,
         na.action = na.retain, exclude1=TRUE, sort=TRUE,
         asna = c("unknown", "unspecified"), \dots)
\method{plot}{summaryP}(x, formula, groups=NULL, xlim = c(-.05, 1.05),
         text.at=NULL, cex.values = 0.5,
         key = list(columns = length(groupslevels), x = 0.75,
                    y = -0.04, cex = 0.9,
                    col = trellis.par.get('superpose.symbol')$col,
                    corner=c(0,1)),
         outerlabels=TRUE, autoarrange=TRUE, \dots)
\method{latex}{summaryP}(object, groups=NULL, file='', round=3,
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
  \item{data}{an optional data frame}
  \item{subset}{an optional subsetting expression or vector}
  \item{na.action}{function specifying how to handle \code{NA}s.  The
		default is to keep all \code{NA}s in the analysis frame.}
  \item{exclude1}{By default, \code{summaryP} removes redundant entries
		from tables for variables with only two levels.  For example, if you
		print the proportion of females, you don't need to print the
		proportion of males.  To override this, set \code{exclude1=FALSE}.}
  \item{sort}{set to \code{FALSE} to not sort category levels in
		descending order of global proportions}
  \item{asna}{character vector specifying level names to consider the
		same as \code{NA}.  Set \code{asna=NULL} to not consider any.}
	\item{x}{an object produced by \code{summaryP}}
  \item{groups}{a character string containing the name of a
   	superpositioning variable for obtaining 
		further stratification within a horizontal line in the dot chart.}
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
 \item{\dots}{ignored}
 \item{object}{an object produced by \code{summaryP}}
 \item{file}{file name, defaults to writing to console}
 \item{round}{number of digits to the right of the decimal place for
	 proportions}
 \item{size}{optional font size such as \code{"small"}}
 \item{append}{set to \code{FALSE} to start output over}
}
\value{\code{summaryP} produces a data frame of class
	\code{"summaryP"}.  The \code{plot} method produces a \code{lattice}
	object of class \code{"trellis"}.  The \code{latex} method produces an
	object of class \code{"latex"} with an additional attribute
	\code{ngrouplevels} specifying the number of levels of any
	\code{groups} variable.
	}
\author{Frank Harrell
  \cr
  Department of Biostatistics
  \cr
  Vanderbilt University
  \cr
  \email{f.harrell@vanderbilt.edu}}
\seealso{\code{\link{bpplotM}}, \code{\link{summaryM}},
	\code{\link{ynbind}}, \code{\link{pBlock}}}
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
# add exclude1=FALSE to include female category
plot(s, groups='treat')

plot(s, val ~ freq | region * var, groups='treat', outerlabels=FALSE)
# Much better looking if omit outerlabels=FALSE; see output at
# http://biostat.mc.vanderbilt.edu/HmiscNew#summaryP
# See more examples under bpplotM

# Make a chart where there is a block of variables that
# are only analyzed for males.  Keep redundant sex in block for demo.
# Leave extra space for numerators, denominators
sb <- summaryP(race + sex +
               pBlock(race, sex, label='Race: Males', subset=sex=='Male') ~
               region, data=d)
plot(sb, text.at=1.3)
plot(sb, groups='region', layout=c(1,3), key=list(space='top'),
     text.at=1.15)
\dontrun{
plot(s, groups='treat')
# plot(s, groups='treat', outerlabels=FALSE) for standard lattice output
plot(s, groups='region', key=list(columns=2, space='bottom'))

plot(summaryP(race + sex ~ region, data=d, exclude1=FALSE), col='green')

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
