\name{units}
\alias{units}
\alias{units.default}
\alias{units<-.default}
\title{
Units Attribute of a Vector  
}
\description{
Sets or retrieves the \code{"units"} attribute of an object.
For \code{units.default} replaces the builtin
version, which only works for time series objects.  If the variable is
also given a \code{label}, subsetting (using \code{[.labelled}) will
retain the \code{"units"} attribute.  For S-Plus, the latter does not work.
}
\usage{
units(x, \dots)
\method{units}{default}(x, none='', \dots)
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
the \code{tspar} attribute of \code{x} if any; otherwise the value \code{none}
}
\seealso{\code{\link{label}}}
\examples{
fail.time <- c(10,20)
units(fail.time) <- "Day"
describe(fail.time)
label(fail.time) <- 'Failure Time'
fail.time
\dontrun{
f <- cph(Surv(fail.time, event) ~ xx)
plot(xx,xx2,xlab=paste(label(xx),", ",units(xx),"s",sep=""))
}
}
\keyword{utilities}
\keyword{interface}
% Converted by Sd2Rd version 1.21.
