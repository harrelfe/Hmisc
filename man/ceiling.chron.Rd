\name{ceiling.chron}
\alias{ceiling.chron}
\alias{floor.chron}
\title{ return the minimum or maxium value in a date or time. }
\description{
  \code{ceiling.chron} returns maximum value that is 1 less then the next value on the unit selected in \code{units}.
  \code{floor.chron} returns the value that is equal to the first time unit selected in \code{units}.
}
\usage{
ceiling.chron(x, units = c("minutes", "hours", "days", "months", "years"))
floor.chron(x, units = c("minutes", "hours", "days", "months", "years"))
}
\arguments{
  \item{x}{ date to be floored or ceilinged }
  \item{units}{ unit to that is is rounded up or down to. }
}
\value{
  An object of class \code{chron}.
}
\author{ Charles Dupont }
\seealso{ \code{\link[chron]{dates}} \code{\link[chron]{times}} }
\examples{
library(chron)
ceiling.chron(dates('7/13/1832'), units='months')  # '7/31/1832'
floor.chron(dates('7/13/1832'), units='years')     # '1/1/1832'
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ chron }% __ONLY ONE__ keyword per line
