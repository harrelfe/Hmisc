\name{makeNstr}
\alias{makeNstr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ creates a string that is a repeat of a substring }
\description{
Takes a character and creates a string that is the character repeated \code{len} times.
}
\usage{
makeNstr(char, len)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{char}{ character to be repeated }
  \item{len}{ number of times to repeat \code{char}. }
}
\value{
  A string that is \code{char} repeated \code{len} times.
}
\author{ Charles Dupont }
\seealso{ \code{\link[base]{paste}}, \code{\link[base]{rep}} }
\examples{
makeNstr(" ", 5)

\dontshow{
if(makeNstr(" ", 5) != "     ") stop("makeNstr failed test")
}
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ character }% __ONLY ONE__ keyword per line
\concept{string}
\concept{concat}
