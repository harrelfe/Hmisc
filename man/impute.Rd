\name{impute}
\alias{impute}
\alias{impute.default}
\alias{print.impute}
\alias{summary.impute}
\alias{[.impute}
\alias{is.imputed}
\title{
Generic Functions and Methods for Imputation
}
\description{
These functions do simple and \code{transcan} 
imputation and print, summarize, and subscript
variables that have NAs filled-in with imputed values.  The simple
imputation method involves filling in NAs with constants,
with a specified single-valued function of the non-NAs, or from
a sample (with replacement) from the non-NA values (this is useful
in multiple imputation).
More complex imputations can be done
with the \code{transcan} function, which also works with the generic methods
shown here, i.e., \code{impute} can take a \code{transcan} object and use  the
imputed values created by \code{transcan} (with \code{imputed=TRUE})  to fill-in NAs.
The \code{print} method places * after variable values that were imputed.
The \code{summary} method summarizes all imputed values and then uses
the next \code{summary} method available for the variable.
The subscript method preserves attributes of the variable and subsets
the list of imputed values corresponding with how the variable was
subsetted.  The \code{is.imputed} function is for checking if observations
are imputed.
}
\usage{
impute(x, ...)

\method{impute}{default}(x, fun=median, ...)

\method{print}{impute}(x, ...)

\method{summary}{impute}(object, ...)

is.imputed(x)
}
\arguments{
\item{x}{
a vector or an object created by \code{transcan}, or a vector needing
basic unconditional imputation.  If there are no \code{NA}s and \code{x}
is a vector, it is returned unchanged.
}
\item{fun}{
the name of a function to use in computing the (single) 
imputed value from the non-NAs.  The default is \code{median}.
If instead of specifying a function as \code{fun}, a single value or vector
(numeric, or character if \code{object} is a factor) is specified,
those values are used for insertion.  \code{fun} can also be the character
string \code{"random"} to draw random values for imputation, with the random
values not forced to be the same if there are multiple NAs.
For a vector of constants, the vector must be of length one
(indicating the same value replaces all NAs) or must be as long as
the number of NAs, in which case the values correspond to consecutive NAs
to replace.  For a factor \code{object}, constants for imputation may include
character values not in the current levels of \code{object}.  In that
case new levels are added.
If \code{object} is of class \code{"factor"}, \code{fun} is ignored and the
most frequent category is used for imputation.
}
\item{object}{an object of class \code{"impute"}}
\item{...}{ignored}
}
\value{
a vector with class \code{"impute"} placed in front of existing classes.
For \code{is.imputed}, a vector of logical values is returned (all
\code{TRUE} if \code{object} is not of class \code{impute}).
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
\code{\link{transcan}}, \code{\link{impute.transcan}}, \code{\link{describe}}, \code{\link{na.include}}, \code{\link{sample}}
}
\examples{
age <- c(1,2,NA,4)
age.i <- impute(age)
# Could have used impute(age,2.5), impute(age,mean), impute(age,"random")
age.i
summary(age.i)
is.imputed(age.i)
}
\keyword{methods}
\keyword{math}
\keyword{htest}
\keyword{models}
