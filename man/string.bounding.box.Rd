\name{string.bounding.box}
\alias{string.bounding.box}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Determine Diamentions of Strings}
\description{
  This determins the number of rows and maximum number of columns of
  each string in a vector.
}
\usage{
string.bounding.box(string, type = c("chars", "width"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{character vector, or a vector to be coerced to a
    character vector}
  \item{type}{character string: How the length of the string should be
    measured. See Details.
  }
}
\value{
  \item{rows}{vector containing the number of character rows in each string.}
  \item{columns}{vector containing the maximum number of characters
    or character columns in each string.
  }
}
\details{
  The length of a string can be measured in one of two ways.  See
  \code{\link{nchar}} Details section for more info.

  `chars' The number of human readable characters.

  `width' The number of columns used to print the string.
}
\author{Charles Dupont}
\note{
  compatable with Splus \code{string.bounding.box}
}
\seealso{\code{\link{nchar}}, \code{\link{stringDims}}}
\examples{
a <- c("this is a single line string", "This is a\nmulty line string")
stringDims(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
