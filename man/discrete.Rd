\name{discrete}
\alias{as.discrete}
\alias{as.discrete.default}
\alias{discrete}
\alias{[<-.discrete}
\alias{[.discrete}
\alias{[[.discrete}
\alias{is.discrete}
\alias{is.na<-.discrete}
\alias{length<-.discrete}

\title{ Discrete Vector tools }
\description{
  \code{discrete} creates a discrete vector which is distinct from a
  continuous vector, or a factor/ordered vector.
  The other function are tools for manipulating descrete vectors.
}
\usage{
as.discrete(x, ...)
\method{as.discrete}{default}(x, ...)
discrete(x, levels = sort(unique.default(x), na.last = TRUE), exclude = NA)
\method{[}{discrete}(x, ...) <- value
\method{[}{discrete}(x, ..., drop = FALSE)
\method{[[}{discrete}(x, i)
is.discrete(x)
\method{is.na}{discrete}(x) <- value
\method{length}{discrete}(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector }
  \item{drop}{ Should unused levels be dropped. }
  \item{exclude}{logical: should \code{NA} be excluded. }
  \item{i}{ indexing vector }
  \item{levels}{ charater: list of individual level values }
  \item{value}{ index of elements to set to \code{NA} }
  \item{\dots}{ arguments to be passed to other functions }
}
\details{
  \code{as.discrete} converts a vector into a discrete vector.

  \code{discrete} creates a discrete vector from provided values.

  \code{is.discrete} tests to see if the vector is a discrete vector.
}
\value{
  \code{as.discrete}, \code{discrete} returns a vector of
  \code{discrete} type.

  \code{is.discrete} returan logical \code{TRUE} if the vector is of
  class discrete other wise it returns \code{FALSE}.
}
\author{ Charles Dupont}
\seealso{ \code{\link{[[}}, \code{\link{[}}, \code{\link{factor}} }
\examples{
a <- discrete(1:25)
a

is.discrete(a)

b <- as.discrete(2:4)
b
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
