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
\description{
After removing any artificial observations added by
\code{addMarginal}, computes the maximum number of
non-missing observations for any left-hand-side variable in
\code{formula}.
}
\examples{
d <- expand.grid(sex=c('female', 'male', NA),
                 country=c('US', 'Romania'),
                 reps=1:2)
dm <- addMarginal(d, sex, country)
dim(d)
nobsY(sex + country ~ 1, data=d)
nobsY(sex ~ 1, data=d)
nobsY(sex ~ 1, data=dm)
}
\keyword{utilities}
\keyword{manip}
