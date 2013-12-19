\name{summaryP}
\alias{summaryP}
\alias{plot.summaryP}
\title{Multi-way Summary of Proportions}
\description{
	\code{summaryP} produces a tall and thin data frame containing
	numerators (\code{freq}) and denominators (\code{denom}) after
	stratifying the data by a series of variables.  A special capability
	to group a series of related yes/no variables is included through the
	internal \code{yn} function, for which the user specials a final
	argument \code{label} used to label the panel created for that group
	of related variables.  For \code{yn}, a positive response is taken to
	be \code{y, yes, present} (ignoring case) or a \code{logical}
	\code{TRUE} value. 
	
	The plot method for \code{summaryP}	displays proportions as a
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
}
\usage{
summaryP(formula, data = NULL, subset = NULL,
         na.action = na.retain, exclude1=TRUE, sort=TRUE,
         asna = c("unknown", "unspecified"))
\method{plot}{summaryP}(x, formula, groups=NULL, xlim = c(0, 1), col = 1:2,
         pch = 1:2, cex.values = 0.5, xwidth=.125, ydelta = 0.04,
         key = list(columns = length(levels(groups)), x = 0.75,
                    y = -0.04, cex = 0.9, col = col, corner=c(0,1)),
         outerlabels=TRUE, \dots)
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
  \item{groups}{a superpositioning variable for obtaining
		further stratification within a horizontal line in the dot chart}
  \item{xlim}{\code{x}-axis limits.  Default is \code{c(0,1)}.}
  \item{col}{vector of color codes or names corresponding to levels of \code{groups}}
  \item{pch}{symbol codes corresponding to \code{groups}}
  \item{cex.values}{character size to use for plotting numerators and
		denominators}
	\item{xwidth}{fraction of a panel's \code{x}-axis width to set aside
		for the fraction text}
  \item{ydelta}{fraction of a panel height to use for positioning
		numerators and denominators below the horizontal line for a group}
  \item{key}{a list to pass to the \code{auto.key} argument of
		\code{dotplot}.  To place a key above the entire chart use
		\code{auto.key=list(columns=2)} for example.}
	\item{outerlabels}{by default if there are two conditioning variables
		besides \code{groups}, the \code{latticeExtra} package's
		\code{useOuterStrips} function is used to put strip labels in the
		margins, usually resulting in a much prettier chart.  Set to
		\code{FALSE} to prevent usage of \code{useOuterStrips}.}
	\item{\dots}{ignored}
}
\value{\code{summaryP} produces a data frame of class
	\code{"summaryP"}.  The \code{plot} method produces a \code{lattice}
	object of class \code{"trellis"}} 
\author{Frank Harrell
  \cr
  Department of Biostatistics
  \cr
  Vanderbilt University
  \cr
  \email{f.harrell@vanderbilt.edu}}
\seealso{\code{\link{bpplotM}}, \code{\link{summaryM}}}
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
s <- summaryP(race + sex + yn(x1, x2, x3, x4, x5, x6, x7, label='Exclusions') ~
              region + treat, data=d)
# add exclude1=FALSE to include female category
plot(s, val ~ freq | region * var, groups=treat, outerlabels=FALSE)
# Much better looking if omit outerlabels=FALSE; see output at
# http://biostat.mc.vanderbilt.edu/HmiscNew#summaryP
\dontrun{
plot(s, groups=treat)
# plot(s, groups=treat, outerlabels=FALSE) for standard lattice output
plot(s, groups=region, key=list(columns=2, space='bottom'))

plot(summaryP(race + sex ~ region, data=d, exclude1=FALSE), col='green')

# Make your own plot using data frame created by summaryP
dotplot(val ~ freq | region * var, groups=treat, data=s,
        xlim=c(0,1), scales=list(y='free', rot=0), xlab='Fraction',
        panel=function(x, y, subscripts, ...) {
          denom <- s$denom[subscripts]
          x <- x / denom
          panel.dotplot(x=x, y=y, subscripts=subscripts, ...) })
}
}
\keyword{hplot}
\keyword{category}
\keyword{manip}
\concept{grouping}
\concept{stratification}
\concept{aggregation}