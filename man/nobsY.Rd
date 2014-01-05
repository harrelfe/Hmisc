\name{nobsY}
\alias{nobsY}
\title{Compute Number of Observations for Left Hand Side of Formula}
\usage{
nobsY(formula, data = NULL, subset = NULL, na.action = na.retain)
}
\arguments{
  \item{formula}{a formula object}
  \item{data}{a data frame}
  \item{subset}{an optional subsetting criterion}
  \item{na.action}{an optional \code{NA}-handling function}
}
\value{an integer, with an attribute \code{"formula"} containing the
	original formula but with an \code{id} variable (if present) removed}
\description{
After removing any artificial observations added by
\code{addMarginal}, computes the maximum number of
non-missing observations for any left-hand-side variable in
\code{formula}.  If \code{formula} contains a term \code{id(variable)}
\code{variable} is assumed to be a subject ID variable, and only unique
subject IDs are counted.
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
nobsY(sex ~ 1, data=d)
nobsY(sex ~ 1, data=dm)
}
\keyword{utilities}
\keyword{manip}
