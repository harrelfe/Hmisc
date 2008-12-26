\name{nstr}
\alias{nstr}

\title{ Creates a string of arbitry length }
\description{
  Creates a vector of strings which consists of the string segment given in
  each element of the \code{string} vector repeated \code{times}.
}
\usage{
nstr(string, times)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{ character: vector of string segments to be
    repeated.  Will be recycled if argument \code{times} is longer.}
  \item{times}{ integer: vector of number of times to repeat the
    corisponding segment.  Will be recycled if argument \code{string} is
    longer.
  }
}
\value{
  returns a character vector the same length as the longest of the two arguments.
}
\author{ Charles Dupont }
\note{
  Will throw a warning if the length of the longer argment is not a even
  multiple of the shorter argument.
}
\seealso{ \code{\link{paste}}, \code{\link{rep}} }
\examples{
nstr(c("a"), c(0,3,4))

nstr(c("a", "b", "c"), c(1,2,3))

nstr(c("a", "b", "c"), 4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{character}% __ONLY ONE__ keyword per line
\keyword{utilities}
