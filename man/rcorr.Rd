\name{rcorr}
\alias{rcorr}
\alias{print.rcorr}
\title{Matrix of Correlations and P-values}
\description{
  \code{rcorr} Computes a matrix of Pearson's \code{r} or Spearman's
  \code{rho} rank correlation coefficients for all possible pairs of
  columns of a matrix.  Missing values are deleted in pairs rather than
  deleting all rows of \code{x} having any missing variables.  Ranks are
  computed using efficient algorithms (see reference 2), using midranks
  for ties.
}
\usage{
rcorr(x, y, type=c("pearson","spearman"))

\method{print}{rcorr}(x, \dots)
}
\arguments{
\item{x}{
  a numeric matrix with at least 5 rows and at least 2 columns (if
  \code{y} is absent).  For \code{print}, \code{x} is an object
  produced by \code{rcorr}.
}
\item{y}{
  a numeric vector or matrix which will be concatenated to \code{x}.  If
  \code{y} is omitted for \code{rcorr}, \code{x} must be a matrix.
}
\item{type}{
  specifies the type of correlations to compute.  Spearman correlations
  are the Pearson linear correlations computed on the ranks of non-missing
  elements, using midranks for ties.
}
\item{\dots}{argument for method compatiblity.}
}
\value{
\code{rcorr} returns a list with elements \code{r}, the
matrix of correlations, \code{n} the
matrix of number of observations used in analyzing each pair of variables,
\code{P}, the asymptotic P-values, and \code{type}.
Pairs with fewer than 2 non-missing values have the r values set to NA.
The diagonals of \code{n} are the number of non-NAs for the single variable
corresponding to that row and column.
}
\details{
Uses midranks in case of ties, as described by Hollander and Wolfe.
P-values are approximated by using the \code{t} or \code{F} distributions.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{fh@fharrell.com}
}
\references{
Hollander M. and Wolfe D.A. (1973).  Nonparametric Statistical Methods.
New York: Wiley.

Press WH, Flannery BP, Teukolsky SA, Vetterling, WT (1988): Numerical
Recipes in C.  Cambridge: Cambridge University Press.
}
\seealso{
\code{\link{hoeffd}}, \code{\link{cor}}, \code{\link{combine.levels}},
\code{\link{varclus}}, \code{\link{dotchart3}}, \code{\link{impute}},
\code{\link{chisq.test}}, \code{\link{cut2}}.
}
\examples{
x <- c(-2, -1, 0, 1, 2)
y <- c(4,   1, 0, 1, 4)
z <- c(1,   2, 3, 4, NA)
v <- c(1,   2, 3, 4, 5)
rcorr(cbind(x,y,z,v))
}
\keyword{nonparametric}
\keyword{htest}
\keyword{category}
