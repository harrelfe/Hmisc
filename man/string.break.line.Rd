\name{string.break.line}
\alias{string.break.line}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Break a string into many lines at carage retruns.}
\description{
  Takes a string and breaks it into seperate substrings where there are
  carrage returns.
}
\usage{
string.break.line(string)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{character vector to be separated into many lines.}
}
\value{
  Returns a list that is the same length of as the \code{string}
  argument.

  Each list element is a character vector.

  Each character vectors elements are the
  split lines of the corasponding element in the \code{string} argument vector.
}
\author{Charles Dupont}
\seealso{\code{\link{strsplit}}}
\examples{
a <- c('', 'this is a single line string',
       'This is a\nmulti-line string.')

b <- string.break.line(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
\keyword{character}% __ONLY ONE__ keyword per line
