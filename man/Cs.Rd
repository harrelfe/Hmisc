\name{Cs}
\alias{Cs}
\title{
Character strings from unquoted names
}
\description{
Makes a vector of character strings from a list of valid S names
}
\usage{
Cs(\dots)
}
\arguments{
\item{...}{
any number of names separated by commas
}}
\value{
character string vector
}
\seealso{
sys.frame, deparse
}
\examples{
Cs(a,cat,dog)
# subset.data.frame <- dataframe[,Cs(age,sex,race,bloodpressure,height)]
}
\keyword{character}
\keyword{utilities}
% Converted by Sd2Rd version 1.21.
