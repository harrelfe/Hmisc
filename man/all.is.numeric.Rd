\name{all.is.numeric}
\alias{all.is.numeric}
\title{Check if All Elements in Character Vector are Numeric}
\description{
Tests, without issuing warnings, whether all elements of a character
vector are legal numeric values, or optionally converts the vector to a
numeric vector.  Leading and trailing blanks in \code{x} are ignored.
}
\usage{
all.is.numeric(x, what = c("test", "vector", "nonnum"), extras=c('.','NA'))
}
\arguments{
  \item{x}{a character vector}
  \item{what}{specify \code{what="vector"} to return a numeric vector if
	it passes the test, or the original character vector otherwise, the
	default \code{"test"} to return \code{FALSE} if there are no
	non-missing non-\code{extra} values of \code{x} or there is at least
	one non-numeric value of \code{x}, or \code{"nonnum"} to return the
	vector of non-\code{extra}, non-NA, non-numeric values of \code{x}.}
  \item{extras}{a vector of character strings to count as numeric
	values, other than \code{""}.}
}
\value{a logical value if \code{what="test"} or a vector otherwise}
\author{Frank Harrell}
\seealso{\code{\link{as.numeric}}}
\examples{
all.is.numeric(c('1','1.2','3'))
all.is.numeric(c('1','1.2','3a'))
all.is.numeric(c('1','1.2','3'),'vector')
all.is.numeric(c('1','1.2','3a'),'vector')
all.is.numeric(c('1','',' .'),'vector')
all.is.numeric(c('1', '1.2', '3a'), 'nonnum')
}
\keyword{character}
