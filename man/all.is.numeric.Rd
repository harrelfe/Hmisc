\name{all.is.numeric}
\alias{all.is.numeric}
\title{Check if All Elements in Character Vector are Numeric}
\description{
Tests, without issuing warnings, whether all elements of a character
vector are legal numeric values, or optionally converts the vector to a
numeric vector.
}
\usage{
all.is.numeric(x, what = c("test", "vector"))
}
\arguments{
  \item{x}{a character vector}
  \item{what}{specify \code{what="vector"} to return a numeric vector if
  it passes the test, or the original character vector otherwise}
}
\value{a logical value if \code{what="test"} or a vector otherwise}
\author{Frank Harrell}
\seealso{\code{\link{as.numeric}}}
\examples{
all.is.numeric(c('1','1.2','3'))
all.is.numeric(c('1','1.2','3a'))
all.is.numeric(c('1','1.2','3'),'vector')
all.is.numeric(c('1','1.2','3a'),'vector')
}
\keyword{character}
