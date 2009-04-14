\name{string.break.line}
\alias{string.break.line}
\title{Break a String into Many Lines at Newlines}
\description{
  Takes a string and breaks it into seperate substrings where there are
  newline characters.
}
\usage{
string.break.line(string)
}
\arguments{
  \item{string}{character vector to be separated into many lines.}
}
\value{
  Returns a list that is the same length of as the \code{string}
  argument.

  Each list element is a character vector.

  Each character vectors elements are the
  split lines of the corresponding element in the \code{string} argument vector.
}
\author{Charles Dupont}
\seealso{\code{\link{strsplit}}}
\examples{
a <- c('', 'this is a single line string',
       'This is a\nmulti-line string.')

b <- string.break.line(a)
}
\keyword{print}
\keyword{character}
