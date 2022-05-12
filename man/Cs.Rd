\name{Cs}
\alias{Cs}
\alias{.q}
\title{
Character strings from unquoted names
}
\description{
\code{Cs} makes a vector of character strings from a list of valid R
names.  \code{.q} is similar but also makes uses of names of arguments.
}
\usage{
Cs(\dots)
.q(\dots)
}
\arguments{
\item{...}{
any number of names separated by commas.  For \code{.q} any names of
arguments will be used.
}}
\value{
character string vector.  For \code{.q} there will be a \code{names}
attribute to the vector if any names appeared in \dots.
}
\seealso{
sys.frame, deparse
}
\examples{
Cs(a,cat,dog)
# subset.data.frame <- dataframe[,Cs(age,sex,race,bloodpressure,height)]
.q(a, b, c, 'this and that')
.q(dog=a, giraffe=b, cat=c)
}
\keyword{character}
\keyword{utilities}
