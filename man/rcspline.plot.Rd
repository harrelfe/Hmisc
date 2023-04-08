\name{rcspline.plot}
\alias{rcspline.plot}
\title{
  Plot Restricted Cubic Spline Function
}
\description{
  Provides plots of the estimated restricted cubic spline function
  relating a single predictor to the response for a logistic or Cox
  model. The \code{rcspline.plot} function does not allow for
  interactions as do \code{\link[rms]{lrm}} and \code{\link[rms]{cph}}, but it can
  provide detailed output for checking spline fits. This function uses
  the \code{\link{rcspline.eval}}, \code{\link[rms]{lrm.fit}}, and Therneau's
  \code{\link[survival:survival-internal]{coxph.fit}} functions and plots the estimated spline
  regression and confidence limits, placing summary statistics on the
  graph. If there are no adjustment variables, \code{rcspline.plot} can
  also plot two alternative estimates of the regression function when
  \code{model="logistic"}: proportions or logit proportions on grouped
  data, and a nonparametric estimate. The nonparametric regression
  estimate is based on smoothing the binary responses and taking the
  logit transformation of the smoothed estimates, if desired. The
  smoothing uses \code{\link{supsmu}}.
}
\usage{
rcspline.plot(x,y,model=c("logistic", "cox", "ols"), xrange, event, nk=5,
              knots=NULL, show=c("xbeta","prob"), adj=NULL, xlab, ylab,
              ylim, plim=c(0,1), plotcl=TRUE, showknots=TRUE, add=FALSE,
              subset, lty=1, noprint=FALSE, m, smooth=FALSE, bass=1,
              main="auto", statloc)
}
\arguments{
  \item{x}{
    a numeric predictor
  }
  \item{y}{
    a numeric response. For binary logistic regression, \code{y} should
    be either 0 or 1.
  }
  \item{model}{
    \code{"logistic"} or \code{"cox"}. For \code{"cox"}, uses the
    \code{coxph.fit} function with \code{method="efron"} arguement set.
  }
  \item{xrange}{
    range for evaluating \code{x}, default is f and
    \eqn{1 - f} quantiles of \code{x}, where
    \eqn{f = \frac{10}{\max{(n, 200)}}}{f = 10/max(n, 200)}
  }
  \item{event}{
    event/censoring indicator if \code{model="cox"}. If \code{event} is
    present, \code{model} is assumed to be \code{"cox"}
  }
  \item{nk}{
    number of knots
  }
  \item{knots}{
    knot locations, default based on quantiles of \code{x} (by
    \code{\link{rcspline.eval}}) 
  }
  \item{show}{
    \code{"xbeta"} or \code{"prob"} - what is plotted on \verb{y}-axis
  }
  \item{adj}{
    optional matrix of adjustment variables
  }
  \item{xlab}{
    \verb{x}-axis label, default is the \dQuote{label} attribute of
    \code{x} 
  }
  \item{ylab}{
    \verb{y}-axis label, default is the \dQuote{label} attribute of
    \code{y}
  }
  \item{ylim}{
    \verb{y}-axis limits for logit or log hazard
  }
  \item{plim}{
    \verb{y}-axis limits for probability scale
  }
  \item{plotcl}{
    plot confidence limits
  }
  \item{showknots}{
    show knot locations with arrows
  }
  \item{add}{
    add this plot to an already existing plot
  }
  \item{subset}{
    subset of observations to process, e.g. \code{sex == "male"}
  }
  \item{lty}{
    line type for plotting estimated spline function
  }
  \item{noprint}{
    suppress printing regression coefficients and standard errors
  }
  \item{m}{
    for \code{model="logistic"}, plot grouped estimates with
    triangles. Each group contains \code{m} ordered observations on
    \code{x}.
  }
  \item{smooth}{
    plot nonparametric estimate if \code{model="logistic"} and
    \code{adj} is not specified
  }
  \item{bass}{
    smoothing parameter (see \code{supsmu})
  }
  \item{main}{
    main title, default is \code{"Estimated Spline Transformation"}
  }
  \item{statloc}{
    location of summary statistics. Default positioning by clicking left
    mouse button where upper left corner of statistics should
    appear. Alternative is \code{"ll"} to place below the graph on the
    lower left, or the actual \code{x} and \code{y} coordinates. Use
    \code{"none"} to suppress statistics.
  }
}
\value{
  list with components (\samp{knots}, \samp{x}, \samp{xbeta},
  \samp{lower}, \samp{upper}) which are respectively the knot locations,
  design matrix, linear predictor, and lower and upper confidence limits
}
\author{
  Frank Harrell  \cr
  Department of Biostatistics, Vanderbilt University  \cr
  \email{fh@fharrell.com}
}
\seealso{
  \code{\link[rms]{lrm}}, \code{\link[rms]{cph}}, \code{\link{rcspline.eval}},
  \code{\link[graphics]{plot}}, \code{\link{supsmu}},
  \code{\link[survival:survival-internal]{coxph.fit}},
  \code{\link[rms]{lrm.fit}}
}
\examples{
#rcspline.plot(cad.dur, tvdlm, m=150)
#rcspline.plot(log10(cad.dur+1), tvdlm, m=150)
}
\keyword{regression}
\keyword{models}
