\name{hoeffd}
\alias{hoeffd}
\alias{print.hoeffd}
\title{
Matrix of Hoeffding's D Statistics
}
\description{
Computes a matrix of Hoeffding's (1948) \code{D} statistics for all
possible pairs of columns of a matrix.  \code{D} is a measure of the
distance between \code{F(x,y)} and \code{G(x)H(y)}, where \code{F(x,y)}
is the joint CDF of \code{X} and \code{Y}, and \code{G} and \code{H} are
marginal CDFs. Missing values are deleted in pairs rather than deleting
all rows of \code{x} having any missing variables.  The \code{D}
statistic is robust against a wide variety of alternatives to
independence, such as non-monotonic relationships.  The larger the value
of \code{D}, the more dependent are \code{X} and \code{Y} (for many
types of dependencies).  \code{D} used here is 30 times Hoeffding's
original \code{D}, and ranges from -0.5 to 1.0 if there are no ties in
the data.  \code{print.hoeffd} prints the information derived by
\code{hoeffd}.  The higher the value of \code{D}, the more dependent are
\code{x} and \code{y}.  \code{hoeffd} also computes the mean and maximum
absolute values of the difference between the joint empirical CDF and
the product of the marginal empirical CDFs.
}
\usage{
hoeffd(x, y)
\method{print}{hoeffd}(x, \dots)
}
\arguments{
\item{x}{
a numeric matrix with at least 5 rows and at least 2 columns (if
\code{y} is absent), or an object created by \code{hoeffd}
}
\item{y}{
a numeric vector or matrix which will be concatenated to \code{x}
}
\item{\dots}{ignored}
}
\value{
a list with elements \code{D}, the
matrix of D statistics, \code{n} the
matrix of number of observations used in analyzing each pair of variables,
and \code{P}, the asymptotic P-values.
Pairs with fewer than 5 non-missing values have the D statistic set to NA.
The diagonals of \code{n} are the number of non-NAs for the single variable
corresponding to that row and column.
}
\details{
Uses midranks in case of ties, as described by Hollander and Wolfe.
P-values are approximated by linear interpolation on the table
in Hollander and Wolfe, which uses the asymptotically equivalent
Blum-Kiefer-Rosenblatt statistic.  For \code{P<.0001} or \code{>0.5}, \code{P} values are
computed using a well-fitting linear regression function in \code{log P} vs.
the test statistic.
Ranks (but not bivariate ranks) are computed using efficient
algorithms (see reference 3).
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
Hoeffding W. (1948): A non-parametric test of independence.  Ann Math Stat
19:546--57.


Hollander M. and Wolfe D.A. (1973).  Nonparametric Statistical Methods,
pp. 228--235, 423. New York: Wiley.


Press WH, Flannery BP, Teukolsky SA, Vetterling, WT (1988): Numerical
Recipes in C.  Cambridge: Cambridge University Press.
}
\seealso{
\code{\link{rcorr}}, \code{\link{varclus}}
}
\examples{
x <- c(-2, -1, 0, 1, 2)
y <- c(4,   1, 0, 1, 4)
z <- c(1,   2, 3, 4, NA)
q <- c(1,   2, 3, 4, 5)
hoeffd(cbind(x,y,z,q))


# Hoeffding's test can detect even one-to-many dependency
set.seed(1)
x <- seq(-10,10,length=200)
y <- x*sign(runif(200,-1,1))
plot(x,y)
hoeffd(x,y)
}
\keyword{nonparametric}
\keyword{htest}
