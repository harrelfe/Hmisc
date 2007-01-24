\name{simplifyDims}
\alias{simplifyDims}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{List Simplification}
\description{
  Takes a list where each element is a group of rows that have been
  spanned by a multirow row and combines it into one large matrix.
}
\usage{
simplifyDims(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{list of spanned rows}
}
\details{
  All rows must have the same number of columns.  This is used to format
  the list for printing.
}
\value{
  a matrix that contains all of the spanned rows.
}
\author{Charles Dupont}
\seealso{\code{\link{rbind}}}
\examples{
a <- list(a = matrix(1:25, ncol=5), b = matrix(1:10, ncol=5), c = 1:5)

simplifyDims(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
