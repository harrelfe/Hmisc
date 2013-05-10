\name{approxExtrap}
\alias{approxExtrap}
\title{Linear Extrapolation}
\description{
  Works in conjunction with the \code{\link{approx}} function to do linear
  extrapolation.  \code{\link{approx}} in R does not support extrapolation at
  all, and it is buggy in S-Plus 6. 
}
\usage{
approxExtrap(x, y, xout, method = "linear", n = 50, rule = 2, f = 0,
             ties = "ordered", na.rm = FALSE)
}
\arguments{
  \item{x,y,xout,method,n,rule,f}{
    see \code{\link{approx}}
  }
  \item{ties}{
    applies only to R.  See \code{\link{approx}}
  }
  \item{na.rm}{
    set to \code{TRUE} to remove \code{NA}s in \code{x} and
    \code{y} before proceeding
  }
}
\details{
  Duplicates in \code{x} (and corresponding \code{y} elements) are removed
  before using \code{approx}.
}
\value{
  a vector the same length as \code{xout}
}
\author{Frank Harrell}
\seealso{\code{\link{approx}}}
\examples{
approxExtrap(1:3,1:3,xout=c(0,4))
}
\keyword{arith}
\keyword{dplot}
