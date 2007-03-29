\name{trunc.POSIXt}
\alias{trunc.POSIXt}
\alias{ceil.POSIXt}
\alias{ceil}
\alias{ceil.default}
\alias{round.POSIXt}
\title{ return the floor, ceiling, or rounded value of date or time to
  specified unit. }
\description{
  \code{trunc.POSIXt} returns the date truncated to the specified unit.
  \code{ceiling.POSIXt} returns next ceiling of the date at the unit selected in
  \code{units}.
  \code{floor.POSIXt} \code{trunk.POSIXt} by another name.
  \code{round.POSIXt} returns the date or time value rounded to nearest
  specified unit selected in \code{digits}.
}
\usage{
ceil(x, units)
\method{ceil}{default}(x, units)
\method{trunc}{POSIXt}(x, units = c("secs", "mins", "hours", "days", "months", "years"))
\method{ceil}{POSIXt}(x, units = c("secs", "mins", "hours", "days", "months", "years"))
\method{round}{POSIXt}(x, digits = c("secs", "mins", "hours", "days", "months", "years"))
}
\arguments{
  \item{x}{ date to be floored or ceilinged }
  \item{units}{ unit to that is is rounded up or down to. }
  \item{digits}{
    same as \code{units} but different name to be compatible
    with \code{\link{round}} generic.
  }
}
\value{
  An object of class \code{POSIXlt}.
}
\author{ Charles Dupont }
\seealso{ \code{\link{Date}} \code{\link{POSIXt}} \code{\link{POSIXlt}} }
\examples{
date <- ISOdate(1832, 7, 12)
ceil(date, units='months')  # '1-8-1832'
trunc(date, units='years')     # '1-1-1832'
round.POSIXt(date, digits='months')    # '1-7-1832'
}
\keyword{ manip }% at least one, from doc/KEYWORDS
\keyword{ utilities }
\keyword{ chron }% __ONLY ONE__ keyword per line
