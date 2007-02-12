\name{partition}
\alias{partition}
\alias{partition.vector}
\alias{partition.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Patitions an object into different sets}
\description{
  Partitions an object into subsets of length defined in the \code{sep}
  argument.
}
\usage{
partition.vector(x, sep, ...)
partition.matrix(x, rowsep, colsep, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{object to be partitioned. }
  \item{sep}{determines how many elements should go into each set.  The
    sum of \code{sep} should be equal to the length of \code{x}.}
  \item{rowsep}{determins how many rows should go into each set.  The
    sum of \code{rowsep} must equal the number of rows in \code{x}.}
  \item{colsep}{determins how many columns should go into each set.  The
    sum of \code{colsep} must equal the number of columns in \code{x}.}
  \item{\dots}{arguments used in other methods of \code{partition}.}
}
\value{
  A list of equal length as \code{sep} containing the partitioned objects.
}
\author{Charles Dupont}
\seealso{ \code{\link{split}}}
\examples{
a <- 1:7
partition.vector(a, sep=c(1,3,2,1))
}
% R documentation directory.
\keyword{manip}
