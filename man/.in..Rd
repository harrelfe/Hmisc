\name{.in.}
\alias{\%nin\%}
\title{
Find Matching (or Non-Matching) Elements
}
\description{
  For two vectors or scalars \code{a} and \code{b},
  \code{\%nin\%} returns
  \code{TRUE} if the element in \code{a} is not in \code{b}.
}
\usage{
a \%nin\% b
}
\arguments{
\item{a}{
a vector (numeric, character, factor)
}
\item{b}{
a vector (numeric, character, factor), matching the mode of a
}}
\value{
vector of logical values with length equal to length of \code{a}.
}
\seealso{
\code{\link{match}}
}
\examples{
c('a','b','c') %nin% c('a','b')
}
\keyword{manip}
\keyword{character}

