\name{GiniMd}
\alias{GiniMd}
\title{Gini's Mean Difference}
\description{
  \code{GiniMD} computes Gini's mean difference on a
  numeric vector.  This index is defined as the mean absolute difference
  between any two distinct elements of a vector.  For a Bernoulli
  (binary) variable with proportion of ones equal to \eqn{p} and sample
  size \eqn{n}, Gini's mean difference is
  \eqn{2\frac{n}{n-1}p(1-p)}{2np(1-p)/(n-1)}.  For a 
  trinomial variable (e.g., predicted values for a 3-level categorical
  predictor using two dummy variables) having (predicted)
  values \eqn{A, B, C} with corresponding proportions \eqn{a, b, c},
  Gini's mean difference is
  \eqn{2\frac{n}{n-1}[ab|A-B|+ac|A-C|+bc|B-C|]}{2n[ab|A-B|+ac|A-C|+bc|B-C|]/(n-1).}
}
\usage{
GiniMd(x, na.rm=FALSE)
}
\arguments{
\item{x}{a numeric vector (for \code{GiniMd})}
\item{na.rm}{set to \code{TRUE} if you suspect there may be \code{NA}s
  in \code{x}; these will then be removed.  Otherwise an error will
  result.}
}
\value{a scalar numeric}
\references{
David HA (1968): Gini's mean difference rediscovered.  Biometrika 55:573--575.
}
\author{
Frank Harrell\cr
Department of Biostatistics\cr
Vanderbilt University\cr
\email{fh@fharrell.com}
}
\examples{
set.seed(1)
x <- rnorm(40)
# Test GiniMd against a brute-force solution
gmd <- function(x) {
  n <- length(x)
  sum(outer(x, x, function(a, b) abs(a - b))) / n / (n - 1)
  }
GiniMd(x)
gmd(x)

z <- c(rep(0,17), rep(1,6))
n <- length(z)
GiniMd(z)
2*mean(z)*(1-mean(z))*n/(n-1)

a <- 12; b <- 13; c <- 7; n <- a + b + c
A <- -.123; B <- -.707; C <- 0.523
xx <- c(rep(A, a), rep(B, b), rep(C, c))
GiniMd(xx)
2*(a*b*abs(A-B) + a*c*abs(A-C) + b*c*abs(B-C))/n/(n-1)
}
\keyword{robust}
\keyword{univar}

