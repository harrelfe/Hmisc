\name{hdquantile}
\alias{hdquantile}
\title{Harrell-Davis Distribution-Free Quantile Estimator}
\description{
Computes the Harrell-Davis (1982) quantile estimator and jacknife
standard errors of quantiles.  The quantile estimator is a weighted
linear combination or order statistics in which the order statistics
used in traditional nonparametric quantile estimators are given the
greatest weight.  In small samples the H-D estimator is more efficient
than traditional ones, and the two methods are asymptotically
equivalent.  The H-D estimator is the limit of a bootstrap average as
the number of bootstrap resamples becomes infinitely large.
}
\usage{
hdquantile(x, probs = seq(0, 1, 0.25),
           se = FALSE, na.rm = FALSE, names = TRUE, weights=FALSE)
}
\arguments{
  \item{x}{a numeric vector}
  \item{probs}{vector of quantiles to compute}
  \item{se}{set to \code{TRUE} to also compute standard errors}
  \item{na.rm}{set to \code{TRUE} to remove \code{NA}s from \code{x}
	before computing quantiles}
  \item{names}{set to \code{FALSE} to prevent names attributions from
	being added to quantiles and standard errors}
  \item{weights}{set to \code{TRUE} to return a \code{"weights"}
	attribution with the matrix of weights used in the H-D estimator
	corresponding to order statistics, with columns corresponding to
	quantiles.}
}
\details{
A Fortran routine is used to compute the jackknife leave-out-one
quantile estimates.  Standard errors are not computed for quantiles 0 or
1 (\code{NA}s are returned).
}
\value{
  A vector of quantiles.  If \code{se=TRUE} this vector will have an
  attribute \code{se} added to it, containing the standard errors.  If
  \code{weights=TRUE}, also has a \code{"weights"} attribute which is a matrix.
}
\references{
  Harrell FE, Davis CE (1982): A new distribution-free quantile
  estimator.  Biometrika 69:635-640.

  Hutson AD, Ernst MD (2000): The exact bootstrap mean and variance of
  an L-estimator.  J Roy Statist Soc B 62:89-94.
}
\author{Frank Harrell}
\seealso{\code{\link{quantile}}}
\examples{
set.seed(1)
x <- runif(100)
hdquantile(x, (1:3)/4, se=TRUE)

\dontrun{
# Compare jackknife standard errors with those from the bootstrap
library(boot)
boot(x, function(x,i) hdquantile(x[i], probs=(1:3)/4), R=400)
}
}
\keyword{univar}
