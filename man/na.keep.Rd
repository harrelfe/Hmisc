\name{na.keep}
\alias{na.keep}
\title{
Do-nothing na.action
}
\description{
Does not delete rows containing NAs, but does add details concerning
the distribution of the response variable if \code{options(na.detail.response=TRUE)}.
This \code{na.action} is primarily for use with \code{describe.formula}. 
}
\usage{
na.keep(mf)
}
\arguments{
\item{mf}{
a model frame
}}
\value{
the same model frame with the \code{"na.action"} attribute


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
\code{\link{na.omit}}, \code{\link{na.delete}}, \code{\link{model.frame.default}}, \code{\link{na.detail.response}},
\code{\link{naresid}}, \code{\link{naprint}}, \code{\link{describe}}
}
\examples{
options(na.action="na.keep", na.detail.response=TRUE)
x1 <- runif(20)
x2 <- runif(20)
x2[1:4] <- NA
y <- rnorm(20)
describe(y ~ x1*x2)
}
\keyword{models}
% Converted by Sd2Rd version 1.21.
