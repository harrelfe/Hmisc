\name{units}
\alias{units}
\alias{units.default}
\alias{units.Surv}
\alias{units<-.default}
\title{
Units Attribute of a Vector  
}
\description{
Sets or retrieves the \code{"units"} attribute of an object.
For \code{units.default} replaces the builtin
version, which only works for time series objects.  If the variable is
also given a \code{label}, subsetting (using \code{[.labelled}) will
retain the \code{"units"} attribute.  For a \code{Surv} object,
\code{units} first looks for an overall \code{"units"} attribute, then
it looks for \code{units} for the \code{time2} variable then for \code{time1}.
When setting \code{"units"}, \code{value} is changed to lower case and any "s" at
the end is removed.
}
\usage{
units(x, \dots)
\method{units}{default}(x, none='', \dots)
\method{units}{Surv}(x, none='', \dots)
\method{units}{default}(x) <- value
}
\arguments{
\item{x}{any object}
\item{\dots}{ignored}
\item{value}{the units of the object, or ""}
\item{none}{value to which to set result if no appropriate attribute is
  found}
}
\value{
the units attribute of x, if any; otherwise, the \code{units} attribute of
the \code{tspar} attribute of \code{x} if any; otherwise the value
\code{none}.  Handling for \code{Surv} objects is different (see above).
}
\seealso{\code{\link{label}}}
\examples{
require(survival)
fail.time <- c(10,20)
units(fail.time) <- "Day"
describe(fail.time)
S <- Surv(fail.time)
units(S)

label(fail.time) <- 'Failure Time'
units(fail.time) <- 'Days'
fail.time

}
\keyword{utilities}
\keyword{interface}
