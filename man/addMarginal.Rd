\name{addMarginal}
\alias{addMarginal}
\title{Add Marginal Observations}
\usage{
addMarginal(data, ..., label = "All", margloc=c('last', 'first'), nested)
}
\arguments{
  \item{data}{a data frame}
	\item{\dots}{a list of names of variables to marginalize}
	\item{label}{category name for added marginal observations}
	\item{margloc}{location for marginal category within factor variable
		specifying categories.  Set to \code{"first"} to override the
		default - to put a category with value \code{label} as the first
		category.}
	\item{nested}{a single unquoted variable name if used}
}
\description{
Given a data frame and the names of variable, doubles the
data frame for each variable with a new category
\code{"All"} by default, or by the value of \code{label}.
A new variable \code{.marginal.} is added to the resulting data frame,
with value \code{""} if the observation is an original one, and with
value equal to the names of the variable being marginalized (separated
by commas) otherwise.  If there is another stratification variable
besides the one in \dots, and that variable is nested inside the
variable in \dots, specify \code{nested=variable name} to have the value
of that variable set fo \code{label} whenever marginal observations are
created for \dots.  See the state-city example below.
}
\examples{
d <- expand.grid(sex=c('female', 'male'), country=c('US', 'Romania'),
                 reps=1:2)
addMarginal(d, sex, country)

# Example of nested variables
d <- data.frame(state=c('AL', 'AL', 'GA', 'GA', 'GA'),
                city=c('Mobile', 'Montgomery', 'Valdosto',
                       'Augusta', 'Atlanta'),
                x=1:5, stringsAsFactors=TRUE)
addMarginal(d, state, nested=city) # cite set to 'All' when state is
}
\keyword{utilities}
\keyword{manip}
