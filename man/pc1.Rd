\name{pc1}
\alias{pc1}
\title{First Principal Component}
\description{
Given a numeric matrix which may or may not contain \code{NA}s,
\code{pc1} standardizes the columns to have mean 0 and variance 1 and
computes the first principal component using \code{\link{prcomp}}.  The
proportion of variance explained by this component is printed, and so
are the coefficients of the original (not scaled) variables.  These
coefficients may be applied to the raw data to obtain the first PC.
}
\usage{
pc1(x, hi)
}
\arguments{
  \item{x}{numeric matrix}
  \item{hi}{if specified, the first PC is scaled so that its maximum
	value is \code{hi} and its minimum value is zero}
}
\value{
  The vector of observations with the first PC.  An attribute
  \code{"coef"} is attached to this vector.  \code{"coef"} contains the
  raw-variable coefficients.
  }
\author{Frank Harrell}
\seealso{\code{\link{prcomp}}}
\examples{
set.seed(1)
x1 <- rnorm(100)
x2 <- x1 + rnorm(100)
w <- pc1(cbind(x1,x2))
attr(w,'coef')
}
\keyword{multivariate}
