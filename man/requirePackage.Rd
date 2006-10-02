\name{requirePackage}
\alias{requirePackage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ require a packge and error if package is not installed }
\description{
require a package.  If package doesn't exist then throw an error.
}
\usage{
requirePackage(package, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{package}{ character vector containing the names of packages to load. }
  \item{\dots}{ arguments to be passed to require. }
}
\author{ Charles Dupont }
\seealso{ \code{\link[base]{library}}, \code{\link[base]{require}} }
\examples{
\dontrun{
requirePackage("Design")
}
}
\keyword{ utilities }% at least one, from doc/KEYWORDS
\keyword{ programming }% __ONLY ONE__ keyword per line
