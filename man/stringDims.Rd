\name{stringDims}
\alias{stringDims}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{String Dimentions}
\description{
  Finds the height and width of all the string in a character vector.
}
\usage{
stringDims(string)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{vector of strings}
}
\details{
  \code{stringDims} finds the number of characters in width and number of
  lines in height for each string in the \code{string} argument.
}
\value{
  \item{height}{a vector of the number of lines in each string.}
  \item{width}{a vector with the number of character columns in the
    longest line.}
}
\author{Charles Dupont}
\seealso{\code{\link{string.bounding.box}}, \code{\link{nchar}}}
\examples{
a <- c("this is a single line string", "This is a\nmulty line string")
stringDims(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
