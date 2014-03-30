\name{nobsY}
\alias{nobsY}
\title{Compute Number of Observations for Left Hand Side of Formula}
\usage{
nobsY(formula, group=NULL, data = NULL, subset = NULL,
      na.action = na.retain, matrixna=c('all', 'any'))
}
\arguments{
  \item{formula}{a formula object}
	\item{group}{character string containing optional name of a
		stratification variable for computing sample sizes}
  \item{data}{a data frame}
  \item{subset}{an optional subsetting criterion}
  \item{na.action}{an optional \code{NA}-handling function}
	\item{matrixna}{set to \code{"all"} if an observation is to be
		considered \code{NA} if all the columns of the variable are
		\code{NA}, otherwise use \code{matrixna="any"} to consider the row
		missing if any of the columns are missing}
}
\value{an integer, with an attribute \code{"formula"} containing the
	original formula but with an \code{id} variable (if present) removed}
\description{
After removing any artificial observations added by
\code{addMarginal}, computes the number of
non-missing observations for all left-hand-side variables in
\code{formula}.  If \code{formula} contains a term \code{id(variable)}
\code{variable} is assumed to be a subject ID variable, and only unique
subject IDs are counted.  If group is given and its value is the name of
a variable in the right-hand-side of the model, an additional object
\code{nobsg} is returned that is a matrix with as many columns as there
are left-hand variables, and as many rows as there are levels to the
\code{group} variable.  This matrix has the further breakdown of unique
non-missing observations by \code{group}.  The concatenation of all ID
variables, is returned in a \code{list} element \code{id}.
}
\examples{
d <- expand.grid(sex=c('female', 'male', NA),
                 country=c('US', 'Romania'),
                 reps=1:2)
d$subject.id <- c(0, 0, 3:12)
dm <- addMarginal(d, sex, country)
dim(dm)
nobsY(sex + country ~ 1, data=d)
nobsY(sex + country ~ id(subject.id), data=d)
nobsY(sex + country ~ id(subject.id) + reps, group='reps', data=d)
nobsY(sex ~ 1, data=d)
nobsY(sex ~ 1, data=dm)
nobsY(sex ~ id(subject.id), data=dm)
}
\keyword{utilities}
\keyword{manip}
