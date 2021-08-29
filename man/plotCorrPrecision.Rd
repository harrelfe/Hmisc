\name{plotCorrPrecision}
\alias{plotCorrPrecision}
\title{Plot Precision of Estimate of Pearson Correlation Coefficient}
\description{
This function plots the precision (margin of error) of the
product-moment linear 
correlation coefficient r vs. sample size, for a given vector of
correlation coefficients \code{rho}.  Precision is defined as the larger
of the upper confidence limit minus rho and rho minus the lower confidence
limit.  \code{labcurve} is used to automatically label the curves.
}
\usage{
plotCorrPrecision(rho = c(0, 0.5), n = seq(10, 400, length.out = 100),
                  conf.int = 0.95, offset=0.025, \dots)
}
\arguments{
  \item{rho}{single or vector of true correlations.  A worst-case
	precision graph results from rho=0}
  \item{n}{vector of sample sizes to use on the x-axis}
  \item{conf.int}{confidence coefficient; default uses 0.95 confidence
	limits}
  \item{offset}{see \code{\link{labcurve}}}
  \item{\dots}{other arguments to \code{\link{labcurve}}}
}
\author{Xing Wang and Frank Harrell}
\seealso{\code{\link{rcorr}},\code{\link{cor}},\code{\link{cor.test}}}
\examples{
plotCorrPrecision()
plotCorrPrecision(rho=0)
}
\keyword{htest}
