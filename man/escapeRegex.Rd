\name{escapeRegex}
\alias{escapeRegex}
\alias{escapeBS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Escapes any characters that would have special meaning in a reqular expression. }
\description{
  Escapes any characters that would have special meaning in a reqular expression.
}
\usage{
escapeRegex(string)
escapeBS(string)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{ string being operated on. }
}
\details{
  \code{escapeRegex} will escape any characters that would have
  special meaning in a reqular expression. For any string
  \code{grep(regexpEscape(string), string)} will always be true.

  \code{escapeBS} will escape any backslash \samp{\\} in a string.
}
\value{
  The value of the string with any characters that would have
  special meaning in a reqular expression escaped.
}
\author{
  Charles Dupont\cr
  Department of Biostatistics\cr
  Vanderbilt University
}
\seealso{\code{\link[base]{grep}} }
\examples{
string <- "this\\\\(system) {is} [full]."
escapeRegex(string)

escapeBS(string)

\dontshow{
if(!any(grep(escapeRegex(string), string))) {
  stop("function escapeRegex failed test")
}

if(escapeBS(string) != "this\\\\\\\\(system) {is} [full].") {
  stop("function escapeBS failed test")
}
}
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ character }% __ONLY ONE__ keyword per line
\keyword{ programming }
