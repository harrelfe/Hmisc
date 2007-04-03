\name{requirePackage}
\alias{requirePackage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  require a packge and error if package is not installed
}
\description{
  require a package.  If package doesn't exist then throw an error.
}
\usage{
requirePackage(package, character.only = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{package}{
    character vector containing the names of packages to load.
  }
  \item{character.only}{
    a logical indicating whether 'package' can be
    assumed to be character string.
  }
  \item{\dots}{
    arguments to be passed to require.
  }
}
\author{ Charles Dupont }
\seealso{
  \code{\link[base]{library}}, \code{\link[base]{require}}
}
\examples{
\dontrun{
requirePackage(methods)
}
\dontshow{
requirePackage(methods)
requirePackage('methods')
}
}
\keyword{ utilities }% at least one, from doc/KEYWORDS
\keyword{ programming }% __ONLY ONE__ keyword per line
