\name{\%nin\%}
\alias{\%nin\%}
\title{
Find Matching (or Non-Matching) Elements
}
\description{
\%nin\% is a binary operator, which returns a logical vector indicating
if there is a match or not for its left operand. A true vector element
indicates no match in left operand, false indicates a match.
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
\code{\link{match}} \code{\link{\%in\%}}
}
\examples{
c('a','b','c') \%nin\% c('a','b')
}
\keyword{manip}
\keyword{character}
