\name{rMultinom}
\alias{rMultinom}
\title{Generate Multinomial Random Variables with Varying Probabilities}
\description{
Given a matrix of multinomial probabilities where rows correspond to
observations and columns to categories (and each row sums to 1),
generates a matrix with the same number of rows as has \code{probs} and
with \code{m} columns.  The columns represent multinomial cell numbers,
and within a row the columns are all samples from the same multinomial
distribution.  The code is a modification of that in the
\code{impute.polyreg} function in the \code{MICE} package.
}
\usage{
rMultinom(probs, m)
}
\arguments{
  \item{probs}{matrix of probabilities}
  \item{m}{number of samples for each row of \code{probs}}
}
\value{
  an integer matrix having \code{m} columns
}
\seealso{\code{\link{rbinom}}}
\examples{
set.seed(1)
w <- rMultinom(rbind(c(.1,.2,.3,.4),c(.4,.3,.2,.1)),200)
t(apply(w, 1, table)/200)
}
\keyword{distribution}

