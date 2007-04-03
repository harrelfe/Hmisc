\name{combine}
\alias{combine}
\alias{combine<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Element Merging }
\description{
  Merges an object by the names of its elements.  Inserting elements in
  \code{value} into \code{x} that do not exists in \code{x} and
  replacing elements in \code{x} that exists in \code{value} with
  \code{value} elements if \code{protect} is false.
}
\usage{
combine(x, value, protect = FALSE, ...)

combine(x, protect = FALSE, ...) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ named list or vector }
  \item{value}{ named list or vector }
  \item{protect}{
    logical; should elements in \code{x} be kept instead
    of elements in \code{value}?
  }
  \item{\dots}{ currently does nothing; included if ever want to make generic. }
}
\author{ Charles Dupont }
\seealso{ \code{\link{names}} }
\examples{
x <- 1:5
names(x) <- LETTERS[x]

y <- 6:10
names(y) <- LETTERS[y-2]

x                  # c(A=1,B=2,C=3,D=4,E=5)
y                  # c(D=6,E=7,F=8,G=9,H=10)

combine(x, y)      # c(A=1,B=2,C=3,D=6,E=7,F=8,G=9,H=10)
combine(x, y, protect=TRUE)      # c(A=1,B=2,C=3,D=5,E=5,F=8,G=9,H=10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }

