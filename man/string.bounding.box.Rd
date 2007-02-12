\name{string.bounding.box}
\alias{string.bounding.box}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Determine Diamentions of Strings}
\description{
  This determins the number of rows and maximum number of columns of
  each string in a vector.
}
\usage{
string.bounding.box(string)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{vector of strings}
}
\value{
  \item{rows}{vector containing the number of character rows in each string.}
  \item{columns}{vector containing the maximum number of character
    columns in each string.}
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
