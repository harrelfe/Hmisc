\name{smean.sd}
\alias{smean.cl.normal}
\alias{smean.sd}
\alias{smean.sdl}
\alias{smean.cl.boot}
\alias{smedian.hilow}
\title{
Compute Summary Statistics on a Vector
}
\description{
A number of statistical summary functions is provided for use
with \code{summary.formula} and \code{summarize} (as well as
\code{tapply} and by themselves).
\code{smean.cl.normal} computes 3 summary variables: the sample mean and
lower and upper Gaussian confidence limits based on the t-distribution.
\code{smean.sd} computes the mean and standard deviation.
\code{smean.sdl} computes the mean plus or minus a constant times the
standard deviation.
\code{smean.cl.boot} is a very fast implementation of the basic
nonparametric bootstrap for obtaining confidence limits for the
population mean without assuming normality.
These functions all delete NAs automatically.
\code{smedian.hilow} computes the sample median and a selected pair of
outer quantiles having equal tail areas.
}
\usage{
smean.cl.normal(x, mult=qt((1+conf.int)/2,n-1), conf.int=.95, na.rm=TRUE)

smean.sd(x, na.rm=TRUE)

smean.sdl(x, mult=2, na.rm=TRUE)

smean.cl.boot(x, conf.int=.95, B=1000, na.rm=TRUE, reps=FALSE)

smedian.hilow(x, conf.int=.95, na.rm=TRUE)
}
\arguments{
\item{x}{
for summary functions \code{smean.*}, \code{smedian.hilow}, a numeric vector
from which NAs will be removed automatically
}
\item{na.rm}{
defaults to \code{TRUE} unlike built-in functions, so that by
default \code{NA}s are automatically removed
}
\item{mult}{
for \code{smean.cl.normal} is the multiplier of the standard error of the
mean to use in obtaining confidence limits of the population mean
(default is appropriate quantile of the t distribution).  For
\code{smean.sdl}, \code{mult} is the multiplier of the standard deviation used
in obtaining a coverage interval about the sample mean.  The default
is \code{mult=2} to use plus or minus 2 standard deviations.
}
\item{conf.int}{
for \code{smean.cl.normal} and \code{smean.cl.boot} specifies the confidence
level (0-1) for interval estimation of the population mean.  For
\code{smedian.hilow}, \code{conf.int} is the coverage probability the outer
quantiles should target.  When the default, 0.95, is used, the lower
and upper quantiles computed are 0.025 and 0.975.
}
\item{B}{
number of bootstrap resamples for \code{smean.cl.boot}
}
\item{reps}{
set to \code{TRUE} to have \code{smean.cl.boot} return the vector of bootstrapped
means as the \code{reps} attribute of the returned object
}
}
\value{
a vector of summary statistics
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
  \code{\link{summarize}}, \code{\link{summary.formula}}
}
\examples{
set.seed(1)
x <- rnorm(100)
smean.sd(x)
smean.sdl(x)
smean.cl.normal(x)
smean.cl.boot(x)
smedian.hilow(x, conf.int=.5)  # 25th and 75th percentiles

# Function to compute 0.95 confidence interval for the difference in two means
# g is grouping variable
bootdif <- function(y, g) {
 g <- as.factor(g)
 a <- attr(smean.cl.boot(y[g==levels(g)[1]], B=2000, reps=TRUE),'reps')
 b <- attr(smean.cl.boot(y[g==levels(g)[2]], B=2000, reps=TRUE),'reps')
 meandif <- diff(tapply(y, g, mean, na.rm=TRUE))
 a.b <- quantile(b-a, c(.025,.975))
 res <- c(meandif, a.b)
 names(res) <- c('Mean Difference','.025','.975')
 res
}

}
\keyword{nonparametric}
\keyword{htest}
\concept{bootstrap}
