\name{string.bounding.box}
\alias{string.bounding.box}
\title{Determine Dimensions of Strings}
\description{
  This determines the number of rows and maximum number of columns of
  each string in a vector.
}
\usage{
string.bounding.box(string, type = c("chars", "width"))
}
\arguments{
  \item{string}{vector of strings}
  \item{type}{character: whether to count characters or screen columns}
}
\value{
  \item{rows}{vector containing the number of character rows in each string}
  \item{columns}{vector containing the maximum number of character
    columns in each string}
}
\author{Charles Dupont}
\seealso{\code{\link{nchar}}, \code{\link{stringDims}}}
\examples{
a <- c("this is a single line string", "This is a\nmulti-line string")
stringDims(a)
}
\keyword{print}
