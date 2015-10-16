\name{addMarginal}
\alias{addMarginal}
\title{Add Marginal Observations}
\usage{
addMarginal(data, ..., label = "All", margloc=c('last', 'first'))
}
\arguments{
  \item{data}{a data frame}
	\item{\dots}{a list of names of variables to marginalize}
	\item{label}{category name for added marginal observations}
	\item{margloc}{location for marginal category within factor variable
		specifying categories.  Set to \code{"first"} to override the
		default - to put a category with value \code{label} as the first category.}
}
\description{
Given a data frame and the names of variable, doubles the
data frame for each variable with a new category
\code{"All"} (or optionally \code{"Combined"}).
A new variable \code{.marginal.} is added to the resulting data frame,
with value \code{""} if the observation is an original one, and with
value equal to the names of the variable being marginalized (separated
by commas) otherwise.
}
\examples{
d <- expand.grid(sex=c('female', 'male'), country=c('US', 'Romania'),
                 reps=1:2)
addMarginal(d, sex, country)
}
\keyword{utilities}
\keyword{manip}
