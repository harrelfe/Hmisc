\name{mApply}
\concept{apply for matrix}
\concept{apply for vector}
\alias{mApply}

\title{Apply a Function to Rows of a Matrix or Vector}

\description{
\code{mApply} is like \code{tapply} except that the first argument can
be a matrix or a vector, and the output is cleaned up if \code{simplify=TRUE}.
It uses code adapted from Tony Plate (\email{tplate@blackmesacapital.com}) to
operate on grouped submatrices.

As \code{mApply} can be much faster than using \code{by}, it is often
worth the trouble of converting a data frame to a numeric matrix for
processing by \code{mApply}.  \code{asNumericMatrix} will do this, and
\code{matrix2dataFrame} will convert a numeric matrix back into a data
frame.
}

\usage{
mApply(X, INDEX, FUN, \dots, simplify=TRUE, keepmatrix=FALSE)
}

\arguments{
\item{X}{
a vector or matrix capable of being operated on by the
function specified as the \code{FUN} argument
}
\item{INDEX}{
list of factors, each of same number of rows as 'X' has.
}
\item{FUN}{
the function to be applied.  In the case of functions like
'+', '%*%', etc., the function name must be quoted.
}
\item{\dots}{
optional arguments to 'FUN'.
}
\item{simplify}{
set to 'FALSE' to suppress simplification of the result in to
an array, matrix, etc.
}
\item{keepmatrix}{set to \code{TRUE} to keep result as a matrix even if
  \code{simplify} is \code{TRUE}, in the case of only one stratum
  }
}
\value{
For \code{mApply}, the returned value is a vector, matrix, or list.
If \code{FUN} returns more than one number, the result is an array if
\code{simplify=TRUE} and is a list otherwise.  If a matrix is returned,
its rows correspond to unique combinations of \code{INDEX}.  If
\code{INDEX} is a list with more than one vector, \code{FUN} returns
more than one number, and \code{simplify=FALSE}, the returned value is a
list that is an array with the first dimension corresponding to the last
vector in \code{INDEX}, the second dimension corresponding to the next
to last vector in \code{INDEX}, etc., and the elements of the list-array
correspond to the values computed by \code{FUN}.  In this situation the
returned value is a regular array if \code{simplify=TRUE}.   The order
of dimensions is as previously but the additional (last) dimension
corresponds to values computed by \code{FUN}.
}

\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{fh@fharrell.com}
}

\seealso{
\code{\link{asNumericMatrix}}, \code{\link{matrix2dataFrame}}, \code{\link{tapply}},
\code{\link{sapply}}, \code{\link{lapply}}, \code{\link{mapply}}, \code{\link{by}}.
}

\examples{
require(datasets, TRUE)
a <- mApply(iris[,-5], iris$Species, mean)
}

\keyword{iteration}
\keyword{category}
