\name{inc-dec}
\alias{inc<-}
\alias{dec<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Increment and Decrement }
\description{
  \code{inc<-} increments \code{x} by \code{value}.
  Equivelent to \code{x <- x + value}.

  \code{dec<-} decrements \code{x} by the \code{value}. Equivelent to
  \code{x <- x - value}.
}
\usage{
inc(x) <- value

dec(x) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object to be incremented or decremented }
  \item{value}{ value by which \code{x} will be modified }
}
\author{ Charles Dupont }
\examples{
x <- 1:5
inc(x) <- 5
x            # c(6,7,8,9,10)

dec(x) <- 3
x            # c(3,4,5,6,7)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}
\keyword{ utilities }% __ONLY ONE__ keyword per line
