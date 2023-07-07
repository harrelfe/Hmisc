\name{transace}
\alias{transace}
\alias{ggplot.transace}
\alias{print.transace}
\alias{areg.boot}
\alias{print.areg.boot}
\alias{plot.areg.boot}
\alias{predict.areg.boot}
\alias{summary.areg.boot}
\alias{print.summary.areg.boot}
\alias{Function.areg.boot}
\alias{Mean}
\alias{Mean.areg.boot}
\alias{Quantile}
\alias{Quantile.areg.boot}
\alias{monotone}
\alias{smearingEst}
\title{
  Additive Regression and Transformations using ace or avas
}
\description{
  \code{transace} is \code{\link[acepack]{ace}} packaged for easily automatically
  transforming all variables in a formula without a left-hand side.
	\code{transace} is a fast 
  one-iteration version of \code{\link{transcan}} without imputation of
  \code{NA}s.  The \code{ggplot} method makes nice transformation plots
	using \code{ggplot2}.  Binary variables are automatically kept linear,
	and character or factor variables are automatically treated as categorical.

  \code{areg.boot} uses \code{\link{areg}} or
  \code{\link[acepack]{avas}} to fit additive regression models allowing
  all variables in the model (including the left-hand-side) to be
  transformed, with transformations chosen so as to optimize certain
  criteria.  The default method uses \code{\link{areg}} whose goal it is
  to maximize \eqn{R^2}. \code{method="avas"} explicity tries to
  transform the response variable so as to stabilize the variance of the
  residuals. All-variables-transformed models tend to inflate \code{R^2}
  and it can be difficult to get confidence limits for each
  transformation. \code{areg.boot} solves both of these problems using
  the bootstrap.  As with the \code{\link[rms]{validate}} function in the
  \pkg{rms} library, the Efron bootstrap is used to estimate the
  optimism in the apparent \eqn{R^2}, and this optimism is subtracted
  from the apparent \eqn{R^2} to optain a bias-corrected \eqn{R^2}.
  This is done however on the transformed response variable scale.

  Tests with 3 predictors show that the \code{\link[acepack]{avas}} and
  \code{\link[acepack]{ace}} estimates are unstable unless the sample size
  exceeds 350.  Apparent \eqn{R^2} with low sample sizes can be very
  inflated, and bootstrap estimates of \eqn{R^2} can be even more
  unstable in such cases, resulting in optimism-corrected \eqn{R^2} that
  are much lower even than the actual \eqn{R^2}.  The situation can be
  improved a little by restricting predictor transformations to be
  monotonic.  On the other hand, the \code{areg} approach allows one to
  control overfitting by specifying the number of knots to use for each
  continuous variable in a restricted cubic spline function.

  For \code{method="avas"} the response transformation is restricted to
  be monotonic.  You can specify restrictions for transformations of
  predictors (and linearity for the response).  When the first argument
  is a formula, the function automatically determines which variables
  are categorical (i.e., \code{factor}, \code{category}, or character
  vectors).  Specify linear transformations by enclosing variables by
  the identify function (\code{I()}), and specify monotonicity by using
  \code{monotone(variable)}.  Monotonicity restrictions are not
  allowed with \code{method="areg"}.
  
  The \code{\link{summary}} method for \code{areg.boot} computes
  bootstrap estimates of standard errors of differences in predicted
  responses (usually on the original scale) for selected levels of each
  predictor against the lowest level of the predictor.  The smearing
  estimator (see below) can be used here to estimate differences in
  predicted means, medians, or many other statistics.  By default,
  quartiles are used for continuous predictors and all levels are used
  for categorical ones.  See \cite{Details} below.  There is also a
  \code{\link{plot}} method for plotting transformation estimates,
  transformations for individual bootstrap re-samples, and pointwise
  confidence limits for transformations. Unless you already have a
  \code{par(mfrow=)} in effect with more than one row or column,
  \code{plot} will try to fit the plots on one page.  A
  \code{\link{predict}} method computes predicted values on the original
  or transformed response scale, or a matrix of transformed
  predictors. There is a \code{\link{Function}} method for producing a
  list of \R functions that perform the final fitted transformations.
  There is also a \code{\link{print}} method for \code{areg.boot}
  objects.

  When estimated means (or medians or other statistical parameters) are
  requested for models fitted with \code{areg.boot} (by
  \code{summary.areg.boot} or \code{predict.areg.boot}), the
  \dQuote{smearing} estimator of \cite{Duan (1983)} is used.  Here we
  estimate the mean of the untransformed response by computing the
  arithmetic mean of \eqn{ginverse(lp + residuals)},
  where ginverse is the inverse of the nonparametric
  transformation of the response (obtained by reverse linear
  interpolation), lp is the linear predictor for an individual
  observation on the transformed scale, and residuals is the
  entire vector of residuals estimated from the fitted model, on the
  transformed scales (n residuals for n original observations).  The
  \code{smearingEst} function computes the general smearing estimate.
  For efficiency \code{smearingEst} recognizes that quantiles are
  transformation-preserving, i.e., when one wishes to estimate a
  quantile of the untransformed distribution one just needs to compute
  the inverse transformation of the transformed estimate after the
  chosen quantile of the vector of residuals is added to it. When the
  median is desired, the estimate is
  \eqn{ginverse(lp + \mbox{median}(residuals))}{
    ginverse(lp + median(residuals))}.
  See the last example for how \code{smearingEst} can be used outside of
  \code{areg.boot}.

  \code{Mean} is a generic function that returns an \R function to
  compute the estimate of the mean of a variable.  Its input is
  typically some kind of model fit object.  Likewise, \code{Quantile} is
  a generic quantile function-producing function.  \code{Mean.areg.boot}
  and \code{Quantile.areg.boot} create functions of a vector of linear
  predictors that transform them into the smearing estimates of the mean
  or quantile of the response variable,
  respectively. \code{Quantile.areg.boot} produces exactly the same
  value as \code{predict.areg.boot} or \code{smearingEst}.  \code{Mean}
  approximates the mapping of linear predictors to means over an evenly
  spaced grid of by default 200 points.  Linear interpolation is used
  between these points.  This approximate method is much faster than the
  full smearing estimator once \code{Mean} creates the function.  These
  functions are especially useful in \code{\link[rms]{nomogram}} (see the
  example on hypothetical data).
}
\usage{
transace(formula, trim=0.01, data=environment(formula))

\method{print}{transace}(x, \dots)

\method{ggplot}{transace}(data, mapping, ..., environment, nrow=NULL)

areg.boot(x, data, weights, subset, na.action=na.delete, 
          B=100, method=c("areg","avas"), nk=4, evaluation=100, valrsq=TRUE, 
          probs=c(.25,.5,.75), tolerance=NULL)

\method{print}{areg.boot}(x, \dots)

\method{plot}{areg.boot}(x, ylim, boot=TRUE, col.boot=2, lwd.boot=.15,
     conf.int=.95, \dots)

smearingEst(transEst, inverseTrans, res,
            statistic=c('median','quantile','mean','fitted','lp'),
            q)

\method{summary}{areg.boot}(object, conf.int=.95, values, adj.to,
        statistic='median', q, \dots)

\method{print}{summary.areg.boot}(x, \dots)

\method{predict}{areg.boot}(object, newdata,
        statistic=c("lp", "median",
                    "quantile", "mean", "fitted", "terms"),
        q=NULL, \dots) 

\method{Function}{areg.boot}(object, type=c('list','individual'),
         ytype=c('transformed','inverse'),
         prefix='.', suffix='', pos=-1, \dots)

Mean(object, \dots)

Quantile(object, \dots)

\method{Mean}{areg.boot}(object, evaluation=200, \dots)

\method{Quantile}{areg.boot}(object, q=.5, \dots)
}
\arguments{
	\item{formula}{a formula without a left-hand-side variable.  Variables
		may be enclosed in \code{monotone(), linear(), categorical()} to
		make certain assumptions about transformations.  \code{categorical}
		and \code{linear} need not be specified if they can be summized from
		the variable values.}
  \item{x}{
    for \code{areg.boot} \code{x} 
    is a formula.  For \code{print} or \code{plot}, an object created by
    \code{areg.boot} or \code{transace}.  For
		\code{print.summary.areg.boot}, and object 
    created by \code{summary.areg.boot}.  For \code{ggplot} is
		the result of \code{transace}.
  }
  \item{object}{
    an object created by \code{areg.boot}, or a model fit object
    suitable for \code{Mean} or \code{Quantile}.
  }
  \item{transEst}{
    a vector of transformed values.  In log-normal regression these
    could be predicted log(Y) for example.
  }
  \item{inverseTrans}{
    a function specifying the inverse transformation needed to change
    \code{transEst} to the original untransformed scale.
    \code{inverseTrans} may also be a 2-element list defining a mapping
    from the transformed values to untransformed values.  Linear
    interpolation is used in this case to obtain untransform values.
  }
	\item{trim}{quantile to which to trim original and transformed values
		for continuous variables for purposes of plotting the
		transformations with \code{ggplot.transace}}
	\item{nrow}{the number of rows to graph for \code{transace}
		transformations, with the default chosen by \code{ggplot2}}
  \item{data}{
    data frame to use if \code{x} is a formula and variables are not
    already in the search list.  For \code{ggplot} is a \code{transace} object.
  }
	\item{environment,mapping}{ignored}
  \item{weights}{
    a numeric vector of observation weights.  By default, all
    observations are weighted equally.
  }
  \item{subset}{
    an expression to subset data if \code{x} is a formula
  }
  \item{na.action}{
    a function specifying how to handle \code{NA}s.  Default is \code{\link{na.delete}}.
  }
  \item{B}{
    number of bootstrap samples (default=100)
  }
  \item{method}{
    \code{"areg"} (the default) or \code{"avas"}
  }
  \item{nk}{
    number of knots for continuous variables not restricted to be
    linear.  Default is 4.  One or two is not allowed.  \code{nk=0}
    forces linearity for all continuous variables.
  }
  \item{evaluation}{
    number of equally-spaced points at which to evaluate (and save) the
    nonparametric transformations derived by \code{\link[acepack]{avas}} or
    \code{\link[acepack]{ace}}.  Default is 100.  For \code{Mean.areg.boot},
    \code{evaluation} is the number of points at which to evaluate exact
    smearing estimates, to approximate them using linear interpolation
    (default is 200).
  }
  \item{valrsq}{
    set to \code{TRUE} to more quickly do bootstrapping without
    validating \eqn{R^2}
  }
  \item{probs}{
    vector probabilities denoting the quantiles of continuous predictors
    to use in estimating effects of those predictors
  }
  \item{tolerance}{
    singularity criterion; list source code for the
    \code{\link{lm.fit.qr.bare}} function.
  }
  \item{res}{
    a vector of residuals from the transformed model.  Not required when
    \code{statistic="lp"} or \code{statistic="fitted"}.
  }
  \item{statistic}{
    statistic to estimate with the smearing estimator.  For
    \code{smearingEst}, the default results in computation of the sample
    median of the model residuals, then \code{smearingEst} adds the
    median residual and back-transforms to get estimated median
    responses on the original scale.  \code{statistic="lp"} causes
    predicted transformed responses to be computed.  For
    \code{smearingEst}, the result (for \code{statistic="lp"}) is the
    input argument \code{transEst}.  \code{statistic="fitted"} gives
    predicted untransformed responses, i.e.,
    \eqn{ginverse(lp)}, where ginverse is the inverse
    of the estimated response transformation, estimated by reverse
    linear interpolation on the tabulated nonparametric response
    transformation or by using an explicit analytic
    function. \code{statistic="quantile"} generalizes \code{"median"} to
    any single quantile \code{q} which must be specified.  \code{"mean"}
    causes the population mean response to be estimated.  For
    \code{predict.areg.boot}, \code{statistic="terms"} returns a matrix
    of transformed predictors. \code{statistic} can also be any \R
    function that computes a single value on a vector of values, such as
    \code{statistic=var}.  Note that in this case the function name is
    not quoted.
  }
  \item{q}{
    a single quantile of the original response scale to estimate, when
    \code{statistic="quantile"}, or for \code{Quantile.areg.boot}.
  }
  \item{ylim}{
    2-vector of y-axis limits
  }
  \item{boot}{
    set to \code{FALSE} to not plot any bootstrapped transformations.
    Set it to an integer k to plot the first k bootstrap
    estimates.
  }
  \item{col.boot}{
    color for bootstrapped transformations
  }
  \item{lwd.boot}{
    line width for bootstrapped transformations
  }
  \item{conf.int}{
    confidence level (0-1) for pointwise bootstrap confidence limits and
    for estimated effects of predictors in \code{summary.areg.boot}. The
    latter assumes normality of the estimated effects.
  }
  \item{values}{
    a list of vectors of settings of the predictors, for predictors for
    which you want to overide settings determined from \code{probs}.
    The list must have named components, with names corresponding to the
    predictors.  Example:
    \code{values=list(x1=c(2,4,6,8), x2=c(-1,0,1))} specifies that
    \code{summary} is to estimate the effect on \code{y} of changing
    \code{x1} from 2 to 4, 2 to 6, 2 to 8, and separately, of changing
    \code{x2} from -1 to 0 and -1 to 1.  
  }
  \item{adj.to}{
    a named vector of adjustment constants, for setting all other
    predictors when examining the effect of a single predictor in
    \code{summary}.  The more nonlinear is the transformation of
    \code{y} the more the adjustment settings will matter.  Default
    values are the medians of the values defined by \code{values} or
    \code{probs}.  You only need to name the predictors for which you
    are overriding the default settings. Example:
    \code{adj.to=c(x2=0,x5=10)} will set \code{x2} to 0 and \code{x5} to
    10 when assessing the impact of variation in the other predictors.
  }
  \item{newdata}{
    a data frame or list containing the same number of values of all of
    the predictors used in the fit.  For \code{\link{factor}} predictors
    the \samp{levels} attribute do not need to be in the same order as
    those used in the original fit, and not all levels need to be
    represented.  If \code{newdata} is omitted, you can still obtain
    linear predictors (on the transformed response scale) and fitted
    values (on the original response scale), but not \code{"terms"}.
  }
  \item{type}{
    specifies how \code{\link{Function}} is to return the series of
    functions that define the transformations of all variables.  By
    default a list is created, with the names of the list elements being
    the names of the variables.  Specify \code{type="individual"} to
    have separate functions created in the current environment
    (\code{pos=-1}, the default) or in location defined by \code{pos}
    if \code{where} is specified.  For the latter method, the names of
    the objects created are the names of the corresponding variables,
    prefixed by \code{prefix} and with \code{suffix} appended to the
    end. If any of \code{pos}, \code{prefix}, or
    \code{suffix} is specified, \code{type} is automatically set to
    \code{"individual"}.
  }
  \item{ytype}{
    By default the first function created by \code{Function} is the
    y-transformation.  Specify \code{ytype="inverse"} to instead create
    the inverse of the transformation, to be able to obtain originally
    scaled y-values.
  }
  \item{prefix}{
    character string defining the prefix for function names created when
    \code{type="individual"}.  By default, the function specifying the
    transformation for variable \code{x} will be named \code{.x}.
  }
  \item{suffix}{
    character string defining the suffix for the function names
  }
  \item{pos}{
	See \code{\link{assign}}.
  }
  \item{\dots}{
    arguments passed to other functions.  Ignored for
		\code{print.transace} and \code{ggplot.transace}.
  }
}
\value{
  \code{transace} returns a list of class \code{transace} containing
	these elements: \code{n} (number of non-missing observations used), \code{transformed} (a matrix containing transformed values), \code{rsq} (vector of \eqn{R^2} with which each
	variable can be predicted from the others), \code{omitted} (row
	numbers of data that were deleted due to \code{NA}s),
	\code{trantab} (compact transformation lookups), \code{levels}
	(original levels of character and factor	varibles if the input was a
	data frame), \code{trim} (value of \code{trim} passed to
	\code{transace}), \code{limits} (the limits for plotting raw and
	transformed variables, computed from \code{trim}), and \code{type} (a
	vector of transformation types used for the variables).
  
  \code{areg.boot} returns a list of class \samp{areg.boot} containing
  many elements, including (if \code{valrsq} is \code{TRUE})
  \code{rsquare.app} and \code{rsquare.val}.  \code{summary.areg.boot}
  returns a list of class \samp{summary.areg.boot} containing a matrix
  of results for each predictor and a vector of adjust-to settings.  It
  also contains the call and a \samp{label} for the statistic that was
  computed.  A \code{print} method for these objects handles the
  printing.  \code{predict.areg.boot} returns a vector unless
  \code{statistic="terms"}, in which case it returns a
  matrix. \code{Function.areg.boot} returns by default a list of
  functions whose argument is one of the variables (on the original
  scale) and whose returned values are the corresponding transformed
  values.  The names of the list of functions correspond to the names of
  the original variables.  When \code{type="individual"},
  \code{Function.areg.boot} invisibly returns the vector of names of the
  created function objects. \code{Mean.areg.boot} and
  \code{Quantile.areg.boot} also return functions.
  
  \code{smearingEst} returns a vector of estimates of distribution
  parameters of class \samp{labelled} so that \code{print.labelled} wil
  print a label documenting the estimate that was used (see
  \code{\link{label}}).  This label can be retrieved for other purposes
  by using e.g. \code{label(obj)}, where obj was the vector
  returned by \code{smearingEst}.
}
\details{
  As \code{transace} only does one iteration over the predictors, it may
  not find optimal transformations and it will be dependent on the order
  of the predictors in \code{x}.

  \code{\link[acepack]{ace}} and \code{\link[acepack]{avas}} standardize transformed variables to have
  mean zero and variance one for each bootstrap sample, so if a
  predictor is not important it will still consistently have a positive
  regression coefficient.  Therefore using the bootstrap to estimate
  standard errors of the additive least squares regression coefficients
  would not help in drawing inferences about the importance of the
  predictors.  To do this, \code{summary.areg.boot} computes estimates
  of, e.g., the inter-quartile range effects of predictors in predicting
  the response variable (after untransforming it).  As an example, at
  each bootstrap repetition the estimated transformed value of one of
  the predictors is computed at the lower quartile, median, and upper
  quartile of the raw value of the predictor.  These transformed x
  values are then multipled by the least squares estimate of the partial
  regression coefficient for that transformed predictor in predicting
  transformed y.  Then these weighted transformed x values have the
  weighted transformed x value corresponding to the lower quartile
  subtracted from them, to estimate an x effect accounting for
  nonlinearity.  The last difference computed is then the standardized
  effect of raising x from its lowest to its highest quartile.  Before
  computing differences, predicted values are back-transformed to be on
  the original y scale in a way depending on \code{statistic} and
  \code{q}. The sample standard deviation of these effects (differences)
  is taken over the bootstrap samples, and this is used to compute
  approximate confidence intervals for effects andapproximate P-values,
  both assuming normality.
  
  \code{predict} does not re-insert \code{NA}s corresponding to
  observations that were dropped before the fit, when \code{newdata} is
  omitted.
  
  \code{statistic="fitted"} estimates the same quantity as
  \code{statistic="median"} if the residuals on the transformed response
  have a symmetric distribution. The two provide identical estimates
  when the sample median of the residuals is exactly zero. The sample
  mean of the residuals is constrained to be exactly zero although this
  does not simplify anything.
}
\author{
  Frank Harrell
  \cr
  Department of Biostatistics
  \cr
  Vanderbilt University School of Medicine
  \cr
  \email{fh@fharrell.com}
}
\seealso{
  \code{\link[acepack]{avas}}, \code{\link[acepack]{ace}},
  \code{\link[rms]{ols}}, \code{\link[rms]{validate}},
  \code{\link[rms]{predab.resample}}, \code{\link{label}},
  \code{\link[rms]{nomogram}}
}
\references{
Harrell FE, Lee KL, Mark DB (1996): Stat in Med 15:361--387.


Duan N (1983): Smearing estimate: A nonparametric retransformation
method.  JASA 78:605--610.


Wang N, Ruppert D (1995): Nonparametric estimation of the
transformation in the transform-both-sides regression model.  JASA
90:522--534. 


See \code{\link[acepack]{avas}}, \code{\link[acepack]{ace}} for primary references.
}
\examples{
# xtrans <- transace(~ monotone(age) + sex + blood.pressure + categorical(race.code))
# print(xtrans)  # show R^2s and a few other things
# ggplot(xtrans) # show transformations

# Generate random data from the model y = exp(x1 + epsilon/3) where
# x1 and epsilon are Gaussian(0,1)
set.seed(171)  # to be able to reproduce example
x1 <- rnorm(200)
x2 <- runif(200)  # a variable that is really unrelated to y]
x3 <- factor(sample(c('cat','dog','cow'), 200,TRUE))  # also unrelated to y
y  <- exp(x1 + rnorm(200)/3)
f  <- areg.boot(y ~ x1 + x2 + x3, B=40)
f
plot(f)
# Note that the fitted transformation of y is very nearly log(y)
# (the appropriate one), the transformation of x1 is nearly linear,
# and the transformations of x2 and x3 are essentially flat 
# (specifying monotone(x2) if method='avas' would have resulted
# in a smaller confidence band for x2)


summary(f)


# use summary(f, values=list(x2=c(.2,.5,.8))) for example if you
# want to use nice round values for judging effects


# Plot Y hat vs. Y (this doesn't work if there were NAs)
plot(fitted(f), y)  # or: plot(predict(f,statistic='fitted'), y)


# Show fit of model by varying x1 on the x-axis and creating separate
# panels for x2 and x3.  For x2 using only a few discrete values
newdat <- expand.grid(x1=seq(-2,2,length=100),x2=c(.25,.75),
                      x3=c('cat','dog','cow'))
yhat <- predict(f, newdat, statistic='fitted')  
# statistic='mean' to get estimated mean rather than simple inverse trans.
xYplot(yhat ~ x1 | x2, groups=x3, type='l', data=newdat)


\dontrun{
# Another example, on hypothetical data
f <- areg.boot(response ~ I(age) + monotone(blood.pressure) + race)
# use I(response) to not transform the response variable
plot(f, conf.int=.9)
# Check distribution of residuals
plot(fitted(f), resid(f))
qqnorm(resid(f))
# Refit this model using ols so that we can draw a nomogram of it.
# The nomogram will show the linear predictor, median, mean.
# The last two are smearing estimators.
Function(f, type='individual')  # create transformation functions
f.ols <- ols(.response(response) ~ age + 
             .blood.pressure(blood.pressure) + .race(race))
# Note: This model is almost exactly the same as f but there
# will be very small differences due to interpolation of
# transformations
meanr <- Mean(f)      # create function of lp computing mean response
medr  <- Quantile(f)  # default quantile is .5
nomogram(f.ols, fun=list(Mean=meanr,Median=medr))


# Create S functions that will do the transformations
# This is a table look-up with linear interpolation
g <- Function(f)
plot(blood.pressure, g$blood.pressure(blood.pressure))
# produces the central curve in the last plot done by plot(f)
}


# Another simulated example, where y has a log-normal distribution
# with mean x and variance 1.  Untransformed y thus has median
# exp(x) and mean exp(x + .5sigma^2) = exp(x + .5)
# First generate data from the model y = exp(x + epsilon),
# epsilon ~ Gaussian(0, 1)


set.seed(139)
n <- 1000
x <- rnorm(n)
y <- exp(x + rnorm(n))
f <- areg.boot(y ~ x, B=20)
plot(f)       # note log shape for y, linear for x.  Good!
xs <- c(-2, 0, 2)
d <- data.frame(x=xs)
predict(f, d, 'fitted')
predict(f, d, 'median')   # almost same; median residual=-.001
exp(xs)                   # population medians
predict(f, d, 'mean')
exp(xs + .5)              # population means


# Show how smearingEst works
res <- c(-1,0,1)          # define residuals
y <- 1:5
ytrans <- log(y)
ys <- seq(.1,15,length=50)
trans.approx <- list(x=log(ys), y=ys)
options(digits=4)
smearingEst(ytrans, exp, res, 'fitted')          # ignores res
smearingEst(ytrans, trans.approx, res, 'fitted') # ignores res 
smearingEst(ytrans, exp, res, 'median')          # median res=0
smearingEst(ytrans, exp, res+.1, 'median')       # median res=.1
smearingEst(ytrans, trans.approx, res, 'median')
smearingEst(ytrans, exp, res, 'mean')
mean(exp(ytrans[2] + res))                       # should equal 2nd # above
smearingEst(ytrans, trans.approx, res, 'mean')
smearingEst(ytrans, trans.approx, res, mean)
# Last argument can be any statistical function operating
# on a vector that returns a single value
}
\keyword{nonparametric}
\keyword{smooth}
\keyword{multivariate}
\keyword{nonlinear}
\keyword{regression}
\concept{bootstrap}
\concept{model validation}
