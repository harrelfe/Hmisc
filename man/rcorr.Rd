\name{rcorr}
\alias{rcorr}
\alias{print.rcorr}
\alias{spearman2}
\alias{spearman2.default}
\alias{spearman2.formula}
\alias{print.spearman2.formula}
\alias{plot.spearman2.formula}
\alias{spearman}
\alias{spearman.test}
\title{
Matrix of Correlations and Generalized Spearman Rank Correlation
}
\description{

\code{rcorr} Computes a matrix of Pearson's \code{r} or Spearman's
\code{rho} rank correlation coefficients for all possible pairs of
columns of a matrix.  Missing values are deleted in pairs rather than
deleting all rows of \code{x} having any missing variables.  Ranks are
computed using efficient algorithms (see reference 2), using midranks
for ties.

\code{spearman2} computes the square of Spearman's rho rank correlation
and a generalization of it in which \code{x} can relate
non-monotonically to \code{y}.  This is done by computing the Spearman
multiple rho-squared between \code{(rank(x), rank(x)^2)} and \code{y}.
When \code{x} is categorical, a different kind of Spearman correlation
used in the Kruskal-Wallis test is computed (and \code{spearman2} can do
the Kruskal-Wallis test).  This is done by computing the ordinary
multiple \code{R^2} between \code{k-1} dummy variables and
\code{rank(y)}, where \code{x} has \code{k} categories.  \code{x} can
also be a formula, in which case each predictor is correlated separately
with \code{y}, using non-missing observations for that predictor.
\code{print} and \code{plot} methods allow one to easily print or plot
the results of \code{spearman2(formula)}.  The adjusted \code{rho^2} is
also computed, using the same formula used for the ordinary adjusted
\code{R^2}.  The \code{F} test uses the unadjusted R2.  For \code{plot},
a dot chart is drawn which by default shows, in sorted order, the
adjusted \code{rho^2}.

\code{spearman} computes Spearman's rho on non-missing values of two
variables.  \code{spearman.test} is a simple version of \code{spearman2.default}.
}
\usage{
rcorr(x, y, type=c("pearson","spearman"))

\method{print}{rcorr}(x, ...)

spearman2(x, ...)

\method{spearman2}{default}(x, y, p=1, minlev=0, exclude.imputed=TRUE, ...)

\method{spearman2}{formula}(x, p=1, 
          data, subset, na.action, minlev=0, exclude.imputed=TRUE, ...)

\method{print}{spearman2.formula}(x, ...)

\method{plot}{spearman2.formula}(x, what=c('Adjusted rho2','rho2','P'),
     sort.=TRUE, main, xlab, \dots)

spearman(x, y)

spearman.test(x, y, p=1)
}
\arguments{
\item{x}{
a numeric matrix with at least 5 rows and at least 2 columns (if
\code{y} is absent).  For \code{spearman2}, the first argument may be a vector
of any type, including character or factor.  The first argument may also be a
formula, in which case all predictors are correlated individually with
the response variable.  \code{x} may be a formula for \code{spearman2}
in which case \code{spearman2.formula} is invoked.  Each
predictor in the right hand side of the formula is separately correlated
with the response variable.  For \code{print}, \code{x} is an object
produced by \code{rcorr} or \code{spearman2}. For \code{plot}, \code{x}
is a result returned by \code{spearman2}.  For \code{spearman} and
\code{spearman.test} \code{x} is a numeric vector, as is \code{y}.
}
\item{type}{
specifies the type of correlations to compute.  Spearman correlations
are the Pearson linear correlations computed on the ranks of non-missing
elements, using midranks for ties.
}
\item{y}{
a numeric vector or matrix which will be concatenated to \code{x}.  If
\code{y} is omitted for \code{rcorr}, \code{x} must be a matrix.
}
\item{p}{
for numeric variables, specifies the order of the Spearman \code{rho^2} to
use.  The default is \code{p=1} to compute the ordinary \code{rho^2}.  Use \code{p=2}
to compute the quadratic rank generalization to allow
non-monotonicity.  \code{p} is ignored for categorical predictors. 
}
\item{data, subset, na.action}{
the usual options for models.  Default for \code{na.action} is to retain
all values, NA or not, so that NAs can be deleted in only a pairwise
fashion.
}
\item{minlev}{
minimum relative frequency that a level of a categorical predictor
should have before it is pooled with other categories (see
\code{combine.levels}) in \code{spearman2}.  The default, \code{minlev=0} causes no pooling.
}
\item{exclude.imputed}{
set to \code{FALSE} to include imputed values (created by \code{impute}) in the calculations.
}
\item{what}{
specifies which statistic to plot
}
\item{sort.}{
set \code{sort.=FALSE} to suppress sorting variables by the statistic being plotted
}
\item{main}{
main title for plot.  Default title shows the name of the response
variable.
}
\item{xlab}{
x-axis label.  Default constructed from \code{what}.
}
\item{...}{
other arguments that are passed to \code{dotchart2}
}}
\value{
\code{rcorr} returns a list with elements \code{r}, the
matrix of correlations, \code{n} the
matrix of number of observations used in analyzing each pair of variables,
and \code{P}, the asymptotic P-values.
Pairs with fewer than 2 non-missing values have the r values set to NA.
The diagonals of \code{n} are the number of non-NAs for the single variable
corresponding to that row and column.  \code{spearman2.default} (the
function that is called for a single \code{x}, i.e., when there is no
formula) returns a vector of statistics for the variable.
\code{spearman2.formula} returns a matrix with rows corresponding to
predictors.
}
\details{
Uses midranks in case of ties, as described by Hollander and Wolfe.
P-values are approximated by using the \code{t} distribution.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{f.harrell@vanderbilt.edu}
}
\references{
Hollander M. and Wolfe D.A. (1973).  Nonparametric Statistical Methods.
New York: Wiley.

Press WH, Flannery BP, Teukolsky SA, Vetterling, WT (1988): Numerical
Recipes in C.  Cambridge: Cambridge University Press.
}
\seealso{
\code{\link{hoeffd}}, \code{\link{cor}}, \code{\link{combine.levels}}, \code{\link{varclus}}, \code{\link{dotchart2}}, \code{\link{impute}}
}
\examples{
x <- c(-2, -1, 0, 1, 2)
y <- c(4,   1, 0, 1, 4)
z <- c(1,   2, 3, 4, NA)
v <- c(1,   2, 3, 4, 5)
rcorr(cbind(x,y,z,v))

spearman2(x, y)
plot(spearman2(z ~ x + y + v, p=2))
}
\keyword{nonparametric}
\keyword{htest}




