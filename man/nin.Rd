\name{\%nin\%}
\alias{\%nin\%}
\title{
  Find Matching (or Non-Matching) Elements
}
\description{
  \code{\%nin\%} is a binary operator, which returns a logical vector indicating
  if there is a match or not for its left operand. A true vector element
  indicates no match in left operand, false indicates a match.
}
\usage{
x \%nin\% table
}
\arguments{
  \item{x}{
    a vector (numeric, character, factor)
  }
  \item{table}{
    a vector (numeric, character, factor), matching the mode of \code{x}
  }
}
\value{
  vector of logical values with length equal to length of \code{x}.
}
\seealso{
  \code{\link{match}} \code{\link{\%in\%}}
}
\examples{
c('a','b','c') \%nin\% c('a','b')
}
\keyword{manip}
\keyword{character}
