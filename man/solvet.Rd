\name{solvet}
\alias{solvet}
\title{
solve Function with tol argument
}
\description{
A slightly modified version of \code{solve} that allows a tolerance argument
for singularity (\code{tol}) which is passed to \code{qr}.
}
\usage{
solvet(a, b, tol=1e-09)
}
\arguments{
  \item{a}{a square numeric matrix}
  \item{b}{a numeric vector or matrix}
  \item{tol}{tolerance for detecting linear dependencies in columns of
	\code{a}}
  }
\seealso{
\code{\link{solve}}
}
\keyword{array}
\keyword{algebra}

