\name{redun}
\alias{redun}
\alias{print.redun}
\title{Redundancy Analysis}
\description{
Uses flexible parametric additive models (see \code{\link{areg}} and its
use of regression splines), or alternatively to run a regular regression
after replacing continuous variables with ranks, to
determine how well each variable can be predicted from the remaining
variables.  Variables are dropped in a stepwise fashion, removing the
most predictable variable at each step. The remaining variables are used
to predict.  The process continues until no variable still in the list
of predictors can be predicted with an \eqn{R^2} or adjusted \eqn{R^2}
of at least \code{r2} or until dropping the variable with the highest
\eqn{R^2} (adjusted or ordinary) would cause a variable that was dropped
earlier to no longer be predicted at least at the \code{r2} level from
the now smaller list of predictors.

There is also an option \code{qrank} to expand each variable into two
columns containing the rank and square of the rank.  Whenever ranks are
used, they are computed as fractional ranks for numerical reasons.
}
\usage{
redun(formula, data=NULL, subset=NULL, r2 = 0.9,
      type = c("ordinary", "adjusted"), nk = 3, tlinear = TRUE,
      rank=qrank, qrank=FALSE,
      allcat=FALSE, minfreq=0, iterms=FALSE, pc=FALSE, pr = FALSE, ...)
\method{print}{redun}(x, digits=3, long=TRUE, ...)
}
\arguments{
  \item{formula}{a formula.  Enclose a variable in \code{I()} to force
	linearity.  Alternately, can be a numeric matrix, in which case the
	data are not run through \code{dataframeReduce}.  This is useful when
	running the data through \code{transcan} first for nonlinearly
	transforming the data.}
  \item{data}{a data frame, which must be omitted if \code{formula} is a
	matrix}
  \item{subset}{usual subsetting expression}
  \item{r2}{ordinary or adjusted \eqn{R^2} cutoff for redundancy}
  \item{type}{specify \code{"adjusted"} to use adjusted \eqn{R^2}}
  \item{nk}{number of knots to use for continuous variables.  Use
	\code{nk=0} to force linearity for all variables.}
  \item{tlinear}{set to \code{FALSE} to allow a variable to be automatically
	nonlinearly transformed (see \code{areg}) while being predicted.  By
  default, only continuous variables on the right hand side (i.e., while
  they are being predictors) are automatically transformed, using
  regression splines.  Estimating transformations for target (dependent)
  variables causes more overfitting than doing so for predictors.}
  \item{rank}{set to \code{TRUE} to replace non-categorical varibles
		with ranks before running the analysis.  This causes \code{nk} to be
		set to zero.}
	\item{qrank}{set to \code{TRUE} to also include squares of ranks to
		allow for non-monotonic transformations}
  \item{allcat}{set to \code{TRUE} to ensure that all categories of
	categorical variables having more than two categories are redundant
	(see details below)}
  \item{minfreq}{For a binary or categorical variable, there must be at
	least two categories with at least \code{minfreq} observations or
	the variable will be dropped and not checked for redundancy against
	other variables.  \code{minfreq} also specifies the minimum
	frequency of a category or its complement 
	before that category is considered when \code{allcat=TRUE}.}
  \item{iterms}{set to \code{TRUE} to consider derived terms (dummy
	variables and nonlinear spline components) as separate variables.
	This will perform a redundancy analysis on pieces of the variables.}
  \item{pc}{if \code{iterms=TRUE} you can set \code{pc} to \code{TRUE}
	to replace the submatrix of terms corresponding to each variable
	with the orthogonal principal components before doing the redundancy
	analysis.  The components are based on the correlation matrix.}
  \item{pr}{set to \code{TRUE} to monitor progress of the stepwise algorithm}
  \item{\dots}{arguments to pass to \code{dataframeReduce} to remove
	"difficult" variables from \code{data} if \code{formula} is
	\code{~.} to use all variables in \code{data} (\code{data} must be
	specified when these arguments are used).  Ignored for \code{print}.}
  \item{x}{an object created by \code{redun}}
  \item{digits}{number of digits to which to round \eqn{R^2} values when
	printing}
  \item{long}{set to \code{FALSE} to prevent the \code{print} method
	from printing the \eqn{R^2} history and the original \eqn{R^2} with
	which each variable can be predicted from ALL other variables.}
}
\value{an object of class \code{"redun"} including an element \code{"scores"}, a numeric matrix with all transformed values when each variable was the dependent variable and the first canonical variate was computed }
\details{
A categorical variable is deemed
redundant if a linear combination of dummy variables representing it can
be predicted from a linear combination of other variables.  For example,
if there were 4 cities in the data and each city's rainfall was also
present as a variable, with virtually the same rainfall reported for all
observations for a city, city would be redundant given rainfall (or
vice-versa; the one declared redundant would be the first one in the
formula). If two cities had the same rainfall, \code{city} might be
declared redundant even though tied cities might be deemed non-redundant
in another setting.  To ensure that all categories may be predicted well
from other variables, use the \code{allcat} option.  To ignore
categories that are too infrequent or too frequent, set \code{minfreq}
to a nonzero integer.  When the number of observations in the category
is below this number or the number of observations not in the category
is below this number, no attempt is made to predict observations being
in that category individually for the purpose of redundancy detection.}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{fh@fharrell.com}
}
\seealso{\code{\link{areg}}, \code{\link{dataframeReduce}},
  \code{\link{transcan}}, \code{\link{varclus}}, \code{\link{r2describe}},
  \code{subselect::genetic}}
\examples{
set.seed(1)
n <- 100
x1 <- runif(n)
x2 <- runif(n)
x3 <- x1 + x2 + runif(n)/10
x4 <- x1 + x2 + x3 + runif(n)/10
x5 <- factor(sample(c('a','b','c'),n,replace=TRUE))
x6 <- 1*(x5=='a' | x5=='c')
redun(~x1+x2+x3+x4+x5+x6, r2=.8)
redun(~x1+x2+x3+x4+x5+x6, r2=.8, minfreq=40)
redun(~x1+x2+x3+x4+x5+x6, r2=.8, allcat=TRUE)
# x5 is no longer redundant but x6 is
redun(~x1+x2+x3+x4+x5+x6, r2=.8, rank=TRUE)
redun(~x1+x2+x3+x4+x5+x6, r2=.8, qrank=TRUE)

# To help decode which variables made a particular variable redundant:
# r <- redun(...)
# r2describe(r$scores)
}
\keyword{smooth}
\keyword{regression}
\keyword{multivariate}
\keyword{methods}
\keyword{models}
\concept{data reduction}
