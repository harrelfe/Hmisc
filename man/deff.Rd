\name{deff}
\alias{deff}
\title{
  Design Effect and Intra-cluster Correlation
}
\description{
  Computes the Kish design effect and corresponding intra-cluster correlation
  for a single cluster-sampled variable
}
\usage{
deff(y, cluster)
}
\arguments{
  \item{y}{
    variable to analyze
  }
  \item{cluster}{
    a variable whose unique values indicate cluster membership.  Any
    type of variable is allowed.
  }
}
\value{
  a vector with named elements \code{n} (total number of non-missing
  observations), \code{clusters} (number of clusters after deleting
  missing data), \code{rho}(intra-cluster correlation), and \code{deff}
  (design effect).
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
\seealso{
\code{\link[rms]{bootcov}}, \code{\link[rms]{robcov}}
}
\examples{
set.seed(1)
blood.pressure <- rnorm(1000, 120, 15)
clinic <- sample(letters, 1000, replace=TRUE)
deff(blood.pressure, clinic)
}
\keyword{htest}
\concept{study design}
\concept{cluster sampling}
