\name{Lag}
\alias{Lag}
\title{Lag a Numeric, Character, or Factor Vector}
\description{
Shifts a vector \code{shift} elements later.  Character or factor
variables are padded with \code{""}, numerics with \code{NA}.  The shift
may be negative.
}
\usage{
Lag(x, shift = 1)
}
\arguments{
  \item{x}{a vector}
  \item{shift}{integer specifying the number of observations to
	be shifted to the right.  Negative values imply shifts to the left.}
}
\details{
A.ttributes of the original object are carried along to the new lagged
one.
}
\value{
a vector like \code{x}
}
\author{Frank Harrell}
\seealso{\code{\link{lag}}}
\examples{
Lag(1:5,2)
Lag(letters[1:4],2)
Lag(factor(letters[1:4]),-2)
# Find which observations are the first for a given subject
id <- c('a','a','b','b','b','c')
id != Lag(id)
!duplicated(id)
}
\keyword{manip}

