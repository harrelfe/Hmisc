\name{eip}
\alias{eip}
\title{Edit In Place}
\description{
Invokes \code{edit( )} on the object \code{name} and stores the
resulting edited object in place of the original, even if this is a
remote place (as long as the user has write access).  This is more
useful in S-Plus than in \R.
}
\usage{
eip(name)
}
\arguments{
  \item{name}{an object, usually a function}
}
\value{
none
}
\author{Frank Harrell}
\seealso{\code{\link{edit}}}
\examples{
\dontrun{
eip(summary.formula}  # make temporary bug fix in central area
}}
\keyword{utilities}

