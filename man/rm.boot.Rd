\name{rm.boot}
\alias{rm.boot}
\alias{plot.rm.boot}
\title{
  Bootstrap Repeated Measurements Model
}
\description{
  For a dataset containing a time variable, a scalar response variable,
  and an optional subject identification variable, obtains least squares
  estimates of the coefficients of a restricted cubic spline function or
  a linear regression in time after adjusting for subject effects
  through the use of subject dummy variables.  Then the fit is
  bootstrapped \code{B} times, either by treating time and subject ID as
  fixed (i.e., conditioning the analysis on them) or as random
  variables.  For the former, the residuals from the original model fit
  are used as the basis of the bootstrap distribution.  For the latter,
  samples are taken jointly from the time, subject ID, and response
  vectors to obtain unconditional distributions.
  
  If a subject \code{id} variable is given, the bootstrap sampling will
  be based on samples with replacement from subjects rather than from
  individual data points.  In other words, either none or all of a given
  subject's data will appear in a bootstrap sample.  This cluster
  sampling takes into account any correlation structure that might exist
  within subjects, so that confidence limits are corrected for
  within-subject correlation.  Assuming that ordinary least squares
  estimates, which ignore the correlation structure, are consistent
  (which is almost always true) and efficient (which would not be true
  for certain correlation structures or for datasets in which the number
  of observation times vary greatly from subject to subject), the
  resulting analysis will be a robust, efficient repeated measures
  analysis for the one-sample problem.
  
  Predicted values of the fitted models are evaluated by default at a
  grid of 100 equally spaced time points ranging from the minimum to
  maximum observed time points.  Predictions are for the average subject
  effect.  Pointwise confidence intervals are optionally computed
  separately for each of the points on the time grid.  However,
  simultaneous confidence regions that control the level of confidence
  for the entire regression curve lying within a band are often more
  appropriate, as they allow the analyst to draw conclusions about
  nuances in the mean time response profile that were not stated
  apriori.  The method of \cite{Tibshirani (1997)} is used to easily
  obtain simultaneous confidence sets for the set of coefficients of the
  spline or linear regression function as well as the average  intercept
  parameter (over subjects).  Here one computes the objective criterion
  (here both the -2 log likelihood evaluated at the bootstrap estimate
  of beta but with respect to the original design matrix and response
  vector, and the sum of squared errors in predicting the original
  response vector) for the original fit as well as for all of the
  bootstrap fits.  The confidence set of the regression coefficients is
  the set of all coefficients that are associated with objective
  function values that are less than or equal to say the 0.95 quantile
  of the vector of \eqn{\code{B} + 1} objective function values.  For
  the coefficients satisfying this condition, predicted curves are
  computed at the time grid, and minima and maxima of these curves are
  computed separately at each time point toderive the final
  simultaneous confidence band.
  
  By default, the log likelihoods that are computed for obtaining the
  simultaneous confidence band assume independence within subject.  This
  will cause problems unless such log likelihoods have very high rank
  correlation with the log likelihood allowing for dependence.  To allow
  for correlation or to estimate the correlation function, see the
  \code{cor.pattern} argument below.
}
\usage{
rm.boot(time, y, id=seq(along=time), subset,
        plot.individual=FALSE,
        bootstrap.type=c('x fixed','x random'),
        nk=6, knots, B=500, smoother=supsmu, 
        xlab, xlim, ylim=range(y), 
        times=seq(min(time), max(time), length=100),
        absorb.subject.effects=FALSE, 
        rho=0, cor.pattern=c('independent','estimate'), ncor=10000,
        \dots)


\method{plot}{rm.boot}(x, obj2, conf.int=.95,
     xlab=x$xlab, ylab=x$ylab, 
     xlim, ylim=x$ylim,
     individual.boot=FALSE,
     pointwise.band=FALSE,
     curves.in.simultaneous.band=FALSE,
     col.pointwise.band=2,
     objective=c('-2 log L','sse','dep -2 log L'), add=FALSE, ncurves,
     multi=FALSE, multi.method=c('color','density'),
     multi.conf   =c(.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.99),
     multi.density=c( -1,90,80,70,60,50,40,30,20,10,  7,  4),
     multi.col    =c(  1, 8,20, 5, 2, 7,15,13,10,11,  9, 14),
     subtitles=TRUE, \dots)
}
\arguments{
  \item{time}{
    numeric time vector
  }
  \item{y}{
    continuous numeric response vector of length the same as \code{time}.
    Subjects having multiple measurements have the measurements strung out.
  }
  \item{x}{
    an object returned from \code{rm.boot}
  }
  \item{id}{
    subject ID variable.  If omitted, it is assumed that each
    time-response pair is measured on a different subject.
  }
  \item{subset}{
    subset of observations to process if not all the data
  }
  \item{plot.individual}{
    set to \code{TRUE} to plot nonparametrically smoothed time-response
    curves for each subject
  }  
  \item{bootstrap.type}{
    specifies whether to treat the time and subject ID variables as
    fixed or random
  }
  \item{nk}{
    number of knots in the restricted cubic spline function fit.  The
    number of knots may be 0 (denoting linear regression) or an integer
    greater than 2 in which k knots results in \eqn{k - 1}
    regression coefficients excluding the intercept.  The default is 6
    knots.
  }
  \item{knots}{
    vector of knot locations.  May be specified if \code{nk} is
    omitted. 
  }
  \item{B}{
    number of bootstrap repetitions.  Default is 500.
  }
  \item{smoother}{
    a smoothing function that is used if \code{plot.individual=TRUE}.
    Default is \code{\link{supsmu}}.
  }
  \item{xlab}{
    label for x-axis.  Default is \code{"units"} attribute of the
    original \code{time} variable, or \code{"Time"} if no such
    attribute was defined using the \code{units} function.
  }
  \item{xlim}{
    specifies x-axis plotting limits.  Default is to use range of times
    specified to \code{rm.boot}.
  }
  \item{ylim}{
    for \code{rm.boot} this is a vector of y-axis limits used if
    \code{plot.individual=TRUE}.  It is also passed along for later use
    by \code{plot.rm.boot}.  For \code{plot.rm.boot}, \code{ylim} can
    be specified, to override the value stored in the object stored by
    \code{rm.boot}.  The default is the actual range of \code{y} in the
    input data.
  }
  \item{times}{
    a sequence of times at which to evaluated fitted values and
    confidence limits.  Default is 100 equally spaced points in the
    observed range of \code{time}.
  }
  \item{absorb.subject.effects}{
    If \code{TRUE}, adjusts the response vector \code{y} before
    re-sampling so that the subject-specific effects in the initial
    model fit are all zero.  Then in re-sampling, subject effects are
    not used in the models.  This will downplay one of the sources of
    variation.  This option is used mainly for checking for consistency
    of results, as the re-sampling analyses are simpler when
    \code{absort.subject.effects=TRUE}.
  }
  \item{rho}{
    The log-likelihood function that is used as the basis of
    simultaneous confidence bands assumes normality with independence
    within subject. To check the robustness of this assumption, if
    \code{rho} is not zero, the log-likelihood under multivariate
    normality within subject, with constant correlation \code{rho}
    between any two time points, is also computed.  If the two
    log-likelihoods have the same ranks across re-samples, alllowing
    the correlation structure does not matter.  The agreement in ranks
    is quantified using the Spearman rank correlation coefficient.  The
    \code{\link{plot}} method allows the non-zero intra-subject
    correlation log-likelihood to be used in deriving the simultaneous
    confidence band.  Note that this approach does assume
    homoscedasticity.
  }
  \item{cor.pattern}{
    More generally than using an equal-correlation structure, you can
    specify a function of two time vectors that generates as many
    correlations as the length of these vectors.  For example,
    \code{cor.pattern=function(time1,time2) 0.2^(abs(time1-time2)/10)}
    would specify a dampening serial correlation pattern.
    \code{cor.pattern} can also be a list containing vectors \code{x}
    (a vector of absolute time differences) and \code{y} (a
    corresponding vector of correlations).  To estimate the correlation
    function as a function of absolute time differences within
    subjects, specify \code{cor.pattern="estimate"}.  The products of
    all possible pairs of residuals (or at least up to \code{ncor} of
    them) within subjects will be related to the absolute time
    difference.  The correlation function is estimated by computing the
    sample mean of the products of standardized residuals, stratified
    by absolute time difference.  The correlation for a zero time
    difference is set to 1 regardless of the \code{\link{lowess}}
    estimate.  NOTE: This approach fails in the presence of large
    subject effects; correcting for such effects removes too much of
    the correlation structure in the residuals.
  }
  \item{ncor}{
    the maximum number of pairs of time values used in estimating the
    correlation function if \code{cor.pattern="estimate"}
  }
  \item{\dots}{
    other arguments to pass to \code{smoother} if \code{plot.individual=TRUE}
  }
  \item{obj2}{
    a second object created by \code{rm.boot} that can also be passed
    to \code{plot.rm.boot}.  This is used for two-sample problems for
    which the time profiles are allowed to differ between the two
    groups.  The bootstrapped predicted y values for the second fit are
    subtracted from the fitted values for the first fit so that the
    predicted mean response for group 1 minus the predicted mean
    response for group 2 is what is plotted. The confidence bands that
    are plotted are also for this difference.  For the simultaneous
    confidence band, the objective criterion is taken to be the sum of
    the objective criteria (-2 log L or sum of squared errors) for the
    separate fits for the two groups. The \code{times} vectors must
    have been identical for both calls to \code{rm.boot}, although
    \code{NA}s can be inserted by the user of one or both of the time
    vectors in the \code{rm.boot} objects so as to suppress certain
    sections of the difference curve from being plotted.
  }
  \item{conf.int}{
    the confidence level to use in constructing simultaneous, and
    optionally pointwise, bands.  Default is 0.95.
  }
  \item{ylab}{
    label for y-axis.  Default is the \code{"label"} attribute of the
    original \code{y} variable, or \code{"y"} if no label was assigned
    to \code{y} (using the \code{label} function, for example).
  }
  \item{individual.boot}{
    set to \code{TRUE} to plot the first 100 bootstrap regression fits
  }
  \item{pointwise.band}{
    set to \code{TRUE} to draw a pointwise confidence band in addition
    to the simultaneous band
  }
  \item{curves.in.simultaneous.band}{
    set to \code{TRUE} to draw all bootstrap regression fits that had a
    sum of squared errors (obtained by predicting the original \code{y}
    vector from the original \code{time} vector and \code{id} vector)
    that was less that or equal to the \code{conf.int} quantile of all
    bootstrapped models (plus the original model). This will show how
    the point by point max and min were computed to form the
    simultaneous confidence band.
  }
  \item{col.pointwise.band}{
    color for the pointwise confidence band.  Default is \samp{2},
    which defaults to red for default Windows S-PLUS setups.
  }
  \item{objective}{
    the default is to use the -2 times log of the Gaussian likelihood
    for computing the simultaneous confidence region.  If neither
    \code{cor.pattern} nor \code{rho} was specified to \code{rm.boot},
    the independent homoscedastic Gaussian likelihood is
    used. Otherwise the dependent homoscedastic likelihood is used
    according to the specified or estimated correlation
    pattern. Specify \code{objective="sse"} to instead use the sum of
    squared errors.
  }
  \item{add}{
    set to \code{TRUE} to add curves to an existing plot.  If you do
    this, titles and subtitles are omitted.
  }
  \item{ncurves}{
    when using \code{individual.boot=TRUE} or
    \code{curves.in.simultaneous.band=TRUE}, you can plot a random
    sample of \code{ncurves} of the fitted curves instead of plotting
    up to \code{B} of them.
  }
  \item{multi}{
    set to \code{TRUE} to draw multiple simultaneous confidence bands
    shaded with different colors.  Confidence levels vary over the
    values in the \code{multi.conf} vector.
  }
  \item{multi.method}{
    specifies the method of shading when \code{multi=TRUE}.  Default is
    to use colors, with the default colors chosen so that when the
    graph is printed under S-Plus for Windows 4.0 to an HP LaserJet
    printer, the confidence regions are naturally ordered by darkness
    of gray-scale. Regions closer to the point estimates (i.e., the
    center) are darker. Specify \code{multi.method="density"} to
    instead use densities of lines drawn per inch in the confidence
    regions, with all regions drawn with the default color. The
    \code{\link{polygon}} function is used to shade the regions.
  }
  \item{multi.conf}{
    vector of confidence levels, in ascending order.  Default is to use
    12 confidence levels ranging from 0.05 to 0.99.
  }
  \item{multi.density}{
    vector of densities in lines per inch corresponding to
    \code{multi.conf}. As is the convention in the
    \code{\link{polygon}} function, a density of -1 indicates a solid
    region.
  }
  \item{multi.col}{
    vector of colors corresponding to \code{multi.conf}.  See
    \code{multi.method} for rationale.
  }
  \item{subtitles}{
    set to \code{FALSE} to suppress drawing subtitles for the plot
  }
}
\value{
  an object of class \code{rm.boot} is returned by \code{rm.boot}. The
  principal object stored in the returned object is a matrix of
  regression coefficients for the original fit and all of the bootstrap
  repetitions (object \code{Coef}), along with vectors of the
  corresponding -2 log likelihoods are sums of squared errors. The
  original fit object from \code{lm.fit.qr} is stored in
  \code{fit}. For this fit, a cell means model is used for the
  \code{id} effects.

  \code{plot.rm.boot} returns a list containing the vector of times used
  for plotting along with the overall fitted values, lower and upper
  simultaneous confidence limits, and optionally the pointwise
  confidence limits.
}
\details{
  Observations having missing \code{time} or \code{y} are excluded from
  the analysis.


  As most repeated measurement studies consider the times as design
  points, the fixed covariable case is the default. Bootstrapping the
  residuals from the initial fit assumes that the model is correctly
  specified.  Even if the covariables are fixed, doing an unconditional
  bootstrap is still appropriate, and for large sample sizes
  unconditional confidence intervals are only slightly wider than
  conditional ones.  For moderate to small sample sizes, the
  \code{bootstrap.type="x random"} method can be fairly conservative.
    

  If not all subjects have the same number of observations (after
  deleting observations containing missing values) and if
  \code{bootstrap.type="x fixed"}, bootstrapped residual vectors may
  have a length m that is different from the number of original
  observations n.  If \eqn{m > n} for a bootstrap
  repetition, the first n elements of the randomly drawn residuals
  are used. If \eqn{m < n}, the residual vector is appended
  with a random sample with replacement of length \eqn{n - m} from itself.  A warning message is issued if this happens.
  If the number of time points per subject varies, the bootstrap results
  for \code{bootstrap.type="x fixed"} can still be invalid, as this
  method assumes that a vector (over subjects) of all residuals can be
  added to the original yhats, and varying number of points will cause
  mis-alignment.

  For \code{bootstrap.type="x random"} in the presence of significant
  subject effects, the analysis is approximate as the subjects used in
  any one bootstrap fit will not be the entire list of subjects.  The
  average (over subjects used in the bootstrap sample) intercept is used
  from that bootstrap sample as a predictor of average subject effects
  in the overall sample.

  Once the bootstrap coefficient matrix is stored by \code{rm.boot},
  \code{plot.rm.boot} can be run multiple times with different options
  (e.g, different confidence levels).

  See \code{\link[rms]{bootcov}} in the \pkg{rms} library for a general
  approach to handling repeated measurement data for ordinary linear
  models, binary and ordinal models, and survival models, using the
  unconditional bootstrap.  \code{\link[rms]{bootcov}} does not handle bootstrapping
  residuals.
}
\author{
  Frank Harrell  \cr
  Department of Biostatistics  \cr
  Vanderbilt University School of Medicine  \cr
  \email{fh@fharrell.com}
}
\references{
  Feng Z, McLerran D, Grizzle J (1996): A comparison of statistical methods for
  clustered data analysis with Gaussian error.  Stat in Med 15:1793--1806.

  Tibshirani R, Knight K (1997):Model search and inference by bootstrap 
  "bumping".  Technical Report, Department of Statistics, University of Toronto.
  \cr
  \url{https://www.jstor.org/stable/1390820}. Presented at the Joint Statistical
  Meetings, Chicago, August 1996.


  Efron B, Tibshirani R (1993): An Introduction to the Bootstrap.
  New York: Chapman and Hall.


  Diggle PJ, Verbyla AP (1998): Nonparametric estimation of covariance
  structure in logitudinal data.  Biometrics 54:401--415.


  Chapman IM, Hartman ML, et al (1997): Effect of aging on the
  sensitivity of growth hormone secretion to insulin-like growth
  factor-I negative feedback.  J Clin Endocrinol Metab 82:2996--3004.

  Li Y, Wang YG (2008): Smooth bootstrap methods for analysis of
  longitudinal data.  Stat in Med 27:937-953. (potential improvements to
  cluster bootstrap; not implemented here)
}
\seealso{
  \code{\link{rcspline.eval}}, \code{\link{lm}}, \code{\link{lowess}},
  \code{\link{supsmu}}, \code{\link[rms]{bootcov}},
  \code{\link{units}}, \code{\link{label}}, \code{\link{polygon}},
  \code{\link{reShape}}
}
\examples{
# Generate multivariate normal responses with equal correlations (.7)
# within subjects and no correlation between subjects
# Simulate realizations from a piecewise linear population time-response
# profile with large subject effects, and fit using a 6-knot spline
# Estimate the correlation structure from the residuals, as a function
# of the absolute time difference


# Function to generate n p-variate normal variates with mean vector u and
# covariance matrix S
# Slight modification of function written by Bill Venables
# See also the built-in function rmvnorm
mvrnorm <- function(n, p = 1, u = rep(0, p), S = diag(p)) {
  Z <- matrix(rnorm(n * p), p, n)
  t(u + t(chol(S)) \%*\% Z)
}


n     <- 20         # Number of subjects
sub   <- .5*(1:n)   # Subject effects


# Specify functional form for time trend and compute non-stochastic component
times <- seq(0, 1, by=.1)
g     <- function(times) 5*pmax(abs(times-.5),.3)
ey    <- g(times)


# Generate multivariate normal errors for 20 subjects at 11 times
# Assume equal correlations of rho=.7, independent subjects


nt    <- length(times)
rho   <- .7


        
set.seed(19)        
errors <- mvrnorm(n, p=nt, S=diag(rep(1-rho,nt))+rho)
# Note:  first random number seed used gave rise to mean(errors)=0.24!


# Add E[Y], error components, and subject effects
y      <- matrix(rep(ey,n), ncol=nt, byrow=TRUE) + errors + 
          matrix(rep(sub,nt), ncol=nt)


# String out data into long vectors for times, responses, and subject ID
y      <- as.vector(t(y))
times  <- rep(times, n)
id     <- sort(rep(1:n, nt))


# Show lowess estimates of time profiles for individual subjects
f <- rm.boot(times, y, id, plot.individual=TRUE, B=25, cor.pattern='estimate',
             smoother=lowess, bootstrap.type='x fixed', nk=6)
# In practice use B=400 or 500
# This will compute a dependent-structure log-likelihood in addition
# to one assuming independence.  By default, the dep. structure
# objective will be used by the plot method  (could have specified rho=.7)
# NOTE: Estimating the correlation pattern from the residual does not
# work in cases such as this one where there are large subject effects


# Plot fits for a random sample of 10 of the 25 bootstrap fits
plot(f, individual.boot=TRUE, ncurves=10, ylim=c(6,8.5))


# Plot pointwise and simultaneous confidence regions
plot(f, pointwise.band=TRUE, col.pointwise=1, ylim=c(6,8.5))


# Plot population response curve at average subject effect
ts <- seq(0, 1, length=100)
lines(ts, g(ts)+mean(sub), lwd=3)


\dontrun{
#
# Handle a 2-sample problem in which curves are fitted 
# separately for males and females and we wish to estimate the
# difference in the time-response curves for the two sexes.  
# The objective criterion will be taken by plot.rm.boot as the 
# total of the two sums of squared errors for the two models
#
knots <- rcspline.eval(c(time.f,time.m), nk=6, knots.only=TRUE)
# Use same knots for both sexes, and use a times vector that 
# uses a range of times that is included in the measurement 
# times for both sexes
#
tm <- seq(max(min(time.f),min(time.m)),
          min(max(time.f),max(time.m)),length=100)


f.female <- rm.boot(time.f, bp.f, id.f, knots=knots, times=tm)
f.male   <- rm.boot(time.m, bp.m, id.m, knots=knots, times=tm)
plot(f.female)
plot(f.male)
# The following plots female minus male response, with 
# a sequence of shaded confidence band for the difference
plot(f.female,f.male,multi=TRUE)


# Do 1000 simulated analyses to check simultaneous coverage 
# probability.  Use a null regression model with Gaussian errors


n.per.pt <- 30
n.pt     <- 10


null.in.region <- 0


for(i in 1:1000) {
  y    <- rnorm(n.pt*n.per.pt)
  time <- rep(1:n.per.pt, n.pt)
#  Add the following line and add ,id=id to rm.boot to use clustering
#  id   <- sort(rep(1:n.pt, n.per.pt))
#  Because we are ignoring patient id, this simulation is effectively
#  using 1 point from each of 300 patients, with times 1,2,3,,,30 


  f <- rm.boot(time, y, B=500, nk=5, bootstrap.type='x fixed')
  g <- plot(f, ylim=c(-1,1), pointwise=FALSE)
  null.in.region <- null.in.region + all(g$lower<=0 & g$upper>=0)
  prn(c(i=i,null.in.region=null.in.region))
}


# Simulation Results: 905/1000 simultaneous confidence bands 
# fully contained the horizontal line at zero
}
}
\keyword{regression}
\keyword{multivariate}
\keyword{htest}
\keyword{hplot}
\concept{bootstrap}
\concept{repeated measures}
\concept{longitudinal data}
