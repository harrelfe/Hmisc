\name{.in.}
\alias{\%in\%}
\alias{\%nin\%}
\title{
Find Matching (or Non-Matching) Elements
}
\description{
For two vectors or scalars \code{a} and \code{b}, \code{a \%in\% b}
returns a vector of logical values corresponding to the elements in
\code{a}.  A value is \code{TRUE} if that element of \code{a} is found
somewhere in \code{b}, \code{FALSE} otherwise.  If \code{a} is a factor
object and \code{b} is numeric, converts \code{a} to its integer codes
before the comparison.  If \code{a} is numeric and \code{b} is
\code{factor}, converts \code{b} to codes.  \code{\%nin\%} returns
\code{TRUE} if the element in \code{a} is not in \code{b}.
}
\usage{
a \%in\% b
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
w <- factor(c("a","b","c"))
w \%in\% c("b","c")
w \%in\% c(2,3)      #same as previous, but with warning


#Suppose that a variable x has levels "a", "b1", "b2" and you
#want to classify levels "b1" and "b2" as just "b".  You can use
#the \%in\% operator and do the following:
# levels(x)[levels(x) \%in\% c("b1","b2")] <- "b"
#Note that levels(x) <- list(b=c('b1','b2')) would be a better approach
}
\keyword{manip}
\keyword{character}

