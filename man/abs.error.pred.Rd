\name{abs.error.pred}
\alias{abs.error.pred}
\alias{print.abs.error.pred}
\title{
  Indexes of Absolute Prediction Error for Linear Models
}
\description{
  Computes the mean and median of various absolute errors related to
  ordinary multiple regression models.  The mean and median absolute
  errors correspond to the mean square due to regression, error, and
  total. The absolute errors computed are derived from \eqn{\hat{Y} -
    \mbox{median($\hat{Y}$)}}{Yhat - median(Yhat)},
  \eqn{\hat{Y} - Y}{Yhat - Y}, and \eqn{Y -
    \mbox{median($Y$)}}{Y - median(Y)}.  The function also
  computes ratios that correspond to \eqn{R^2} and \eqn{1 - R^2} (but
  these ratios do not add to 1.0); the \eqn{R^2} measure is the ratio of
  mean or median absolute \eqn{\hat{Y} - \mbox{median($\hat{Y}$)}}{Yhat
    - median(Yhat)} to the mean or median absolute \eqn{Y -
    \mbox{median($Y$)}}{Y - median(Y)}. The \eqn{1 - R^2} or SSE/SST
  measure is the mean or median absolute \eqn{\hat{Y} - Y}{Yhat - Y}
  divided by the mean or median absolute \eqn{\hat{Y} -
   \mbox{median($Y$)}}{Y - median(Y)}.
}
\usage{
abs.error.pred(fit, lp=NULL, y=NULL)

\method{print}{abs.error.pred}(x, \dots)
}
\arguments{
  \item{fit}{
    a fit object typically from \code{\link{lm}} or \code{\link[rms]{ols}}
    that contains a y vector (i.e., you should have specified
    \code{y=TRUE} to the fitting function) unless the \code{y} argument
    is given to \code{abs.error.pred}.  If you do not specify the
    \code{lp} argument, \code{fit} must contain \code{fitted.values} or
    \code{linear.predictors}.  You must specify \code{fit} or both of
    \code{lp} and \code{y}.
  }
  \item{lp}{
    a vector of predicted values (Y hat above) if \code{fit} is not given
  }
  \item{y}{
    a vector of response variable values if \code{fit} (with
    \code{y=TRUE} in effect) is not given
  }
  \item{x}{an object created by \code{abs.error.pred}}
  \item{\dots}{unused}
}
\value{
  a list of class \code{abs.error.pred} (used by
  \code{print.abs.error.pred}) containing two matrices:
  \code{differences} and \code{ratios}.
}
\author{
  Frank Harrell\cr
  Department of Biostatistics\cr
  Vanderbilt University School of Medicine\cr
  \email{fh@fharrell.com}
}
\seealso{
  \code{\link{lm}}, \code{\link[rms]{ols}}, \code{\link{cor}},
  \code{\link[rms]{validate.ols}}
}
\references{
  Schemper M (2003): Stat in Med 22:2299-2308.

  Tian L, Cai T, Goetghebeur E, Wei LJ (2007): Biometrika 94:297-311.
}
\examples{
set.seed(1)         # so can regenerate results
x1 <- rnorm(100)
x2 <- rnorm(100)
y  <- exp(x1+x2+rnorm(100))
f <- lm(log(y) ~ x1 + poly(x2,3), y=TRUE)
abs.error.pred(lp=exp(fitted(f)), y=y)
rm(x1,x2,y,f)
}
\keyword{robust}
\keyword{regression}
\keyword{models}
\concept{predictive accuracy}
