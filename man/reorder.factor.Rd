\name{reorder.factor}
\alias{reorder.factor}
\title{Reorder Factor Levels}
\description{
Reorders the levels of a factor variable by the values or the summarized
values of another variable
}
\usage{
reorder.factor(x, v, FUN = mean, ...)
}
\arguments{
  \item{x}{a factor variable}
  \item{v}{a numeric variable the same length as \code{x}}
  \item{FUN}{a statistical summarization function applied to \code{v} by
  levels of \code{x}}
  \item{\dots}{other arguments passed to \code{FUN}}
}
\value{
  a new factor vector
}
\seealso{\code{\link{factor}}}
\examples{
x <- factor(c('a','b','b','c'))
v <- c(3,-1,1,-5)
w <- reorder.factor(x, v)  # uses FUN=mean
w
levels(w)
class(w)
}
\keyword{manip}

