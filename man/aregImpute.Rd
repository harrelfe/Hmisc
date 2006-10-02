\name{aregImpute}
\alias{aregImpute}
\alias{print.aregImpute}
\alias{plot.aregImpute}
\title{
Multiple Imputation using Additive Regression, Bootstrapping, and
Predictive Mean Matching
}
\description{
The \code{transcan} function creates flexible additive imputation models
but provides only an approximation to true multiple imputation as the
imputation models are fixed before all multiple imputations are
drawn.  This ignores variability caused by having to fit the
imputation models.  \code{aregImpute} takes all aspects of uncertainty in
the imputations into account by using the bootstrap to approximate the
process of drawing predicted values from a full Bayesian predictive
distribution.  Different bootstrap resamples are used for each of the
multiple imputations, i.e., for the \code{i}th imputation of a sometimes
missing variable, \code{i=1,2,\dots n.impute}, a flexible additive
model is fitted on a sample with replacement
from the original data and this model is used to predict all of the
original missing and non-missing values for the target variable.

\code{areg} is used to fit the imputation models.  Unless the identity
transformation is specified or \code{nk} is set to zero, \code{areg}
simultaneously find transformations of the target variable and of all of
the predictors, to get a good fit assuming additivity, maximizing
\eqn{R^2}, using the same canonical correlation method as
\code{transcan}.  When a categorical variable is being predicted, this
is Fisher's optimum scoring method.  For continuous variables.  The
default transformation for continuous variables is a restricted cubic
spline function unless \code{nk=0}.  allows nonmonotonic transformations
of target variables.

Instead of taking random draws from fitted imputation models using
random residuals as is done by \code{transcan}, \code{aregImpute} uses
predictive mean matching with optional weighted probability sampling of
donors rather than using only the closest match.  Predictive mean
matching works for binary, categorical, and continuous variables without
the need for iterative maximum likelihood fitting for binary and
categorical variables, and without the need for computing residuals or
for curtailing imputed values to be in the range of actual data.
Predictive mean matching is especially attractive when the variable
being imputed is also being transformed automatically.  See Details
below for more information about the algorithm.  A \code{"regression"}
method is also available that is similar to that used in
\code{transcan}.  This option should be used when mechanistic
missingness requires the use of extrapolation during imputation.

A \code{print} method summarizes the results, and a \code{plot} method plots
distributions of imputed values.
Typically, \code{fit.mult.impute} will be called after \code{aregImpute}.
}
\usage{
aregImpute(formula, data, subset, n.impute=5, group=NULL,
           nk=3, type=c('pmm','regression'),
           match=c('weighted','closest'), fweighted=0.2, burnin=3,
           x=FALSE, pr=TRUE, plotTrans=FALSE, tolerance=NULL)
\method{print}{aregImpute}(x, \dots)
\method{plot}{aregImpute}(x, nclass=NULL, type=c('ecdf','hist'), 
     diagnostics=FALSE, maxn=10, \dots)
}
\arguments{
\item{formula}{
an S model formula.  You can specify restrictions for transformations
of variables.  The function automatically determines which variables
are categorical (i.e., \code{factor}, \code{category}, or character vectors).
Binary variables are automatically restricted to be linear.  Force
linear transformations of continuous variables by enclosing variables
by the identify function (\code{I()}).
}
\item{x}{
  an object created by \code{aregImpute}.  For \code{aregImpute}, set
  \code{x} to \code{TRUE} to save the data matrix containing the final (number
  \code{n.impute}) imputations in the result.  This
  is needed if you want to later do out-of-sample imputation.
  Categorical variables are coded as integers in this matrix.
}
\item{data}{
}
\item{subset}{
These may be also be specified.  You may not specify \code{na.action} as
\code{na.retain} is always used.
}
\item{n.impute}{
number of multiple imputations.  \code{n.impute=5} is frequently
recommended but 10 or more doesn't hurt.
}
\item{group}{a character or factor variable the same length as the
  number of observations in \code{data} and containing no \code{NA}s.
  When \code{group} is present, causes a bootstrap sample of the
  observations corresponding to non-\code{NA}s of a target variable to
  have the same frequency distribution of \code{group} as the
  that in the non-\code{NA}s of the original sample.  This can handle
  k-sample problems as well as lower the chance that a bootstrap sample
  will have a missing cell when the original cell frequency was low.
}
\item{nk}{number of knots to use for continuous variables.  When both
  the target variable and the predictors are having optimum
  transformations estimated, there is more instability than with normal
  regression so the complexity of the model should decrease more sharply
  as the sample size decreases.  Hence set \code{nk} to 0 (to force
  linearity for non-categorical variables) or 3 (minimum number of knots
  possible with a linear tail-restricted cubic spline) for small sample
  sizes.  Simulated problems as in the examples section can assist in
  choosing \code{nk}.
  }
\item{type}{
  The default is \code{"pmn"} for predictive mean matching,
  which is a more nonparametric approach that will work for categorical
  as well as continuous predictors.  Alternatively, use
  \code{"regression"} when all variables that are sometimes missing are
  continuous and the missingness mechanism is such that entire intervals
  of population values are unobserved.  See the Details section for more
  information.  For the \code{plot} method, specify \code{type="hist"}
  to draw histograms of imputed values with rug plots at the top, or
  \code{type="ecdf"} (the default) to draw empirical CDFs with spike
  histograms at the bottom.
}
\item{match}{
  Defaults to \code{match="weighted"} to do weighted multinomial
  probability sampling using the tricube function (similar to lowess)
  as the weights.  The argument of the tricube function is the absolute
  difference in transformed predicted values of all the donors and of
  the target predicted value, divided by a scaling factor.
  The scaling factor in the tricube function is \code{fweighted} times
  the mean absolute difference between the target predicted value and
  all the possible donor predicted values.  Set \code{match="closest"}
  to find as the donor the observation having the closest predicted
  transformed value, even if that same donor is found repeatedly.}
\item{fweighted}{
  Smoothing parameter (multiple of mean absolute difference) used when
  \code{match="weighted"}, with a default value of 0.2.  Set
  \code{fweighted} to a number between 0.02 and 0.2 to force the donor
  to have a predicted value closer to the target, and set
  \code{fweighted} to larger values (but seldom larger than 1.0) to allow
  donor values to be less tightly matched.  See the examples below to
  learn how to study the relationship between \code{fweighted} and the
  standard deviation of multiple imputations within individuals.}
\item{burnin}{
  \code{aregImpute} does \code{burnin + n.impute} iterations of the
  entire modeling process.  The first \code{burnin} imputations are
  discarded.  More burn-in iteractions may be requied when multiple
  variables are missing on the same observations.}
\item{pr}{
set to \code{FALSE} to suppress printing of iteration messages
}
\item{plotTrans}{
  set to \code{TRUE} to plot \code{ace} or \code{avas} transformations
  for each variable for each of the multiple imputations.  This is
  useful for determining whether transformations are reasonable.  If
  transformations are too noisy or have long flat sections (resulting in
  "lumps" in the distribution of imputed values), it may be advisable to
  place restrictions on the transformations (monotonicity or linearity).
}
\item{tolerance}{singularity criterion; list the source code in the
  \code{lm.fit.qr.bare} function for details}
\item{nclass}{
number of bins to use in drawing histogram
}
\item{diagnostics}{
Specify \code{diagnostics=TRUE} to draw plots of imputed values against
sequential imputation numbers, separately for each missing
observations and variable. 
}
\item{maxn}{
Maximum number of observations shown for diagnostics.  Default is
\code{maxn=10}, which limits the number of observations plotted to at most
the first 10.
}
\item{...}{
other arguments that are ignored
}}
\value{
a list of class \code{"aregImpute"} containing the following elements:

\item{call}{
the function call expression
}
\item{formula}{
the formula specified to \code{aregImpute}
}
\item{match}{
the \code{match} argument
}
\item{fweighted}{
  the \code{fweighted} argument
  }
\item{n}{
total number of observations in input dataset
}
\item{p}{
number of variables
}
\item{na}{
list of subscripts of observations for which values were originally missing
}
\item{nna}{
named vector containing the numbers of missing values in the data
}
\item{type}{
vector of types of transformations used for each variable
(\code{"s","l","c"} for smooth spline, linear, or categorical with dummy
variables)
}
\item{nk}{number of knots used for smooth transformations}
\item{cat.levels}{
list containing character vectors specifying the \code{levels} of
categorical variables
}
\item{df}{degrees of freedom (number of parameters estimated) for each
  variable}
\item{n.impute}{
number of multiple imputations per missing value
}
\item{imputed}{
a list containing matrices of imputed values in the same format as
those created by \code{transcan}.  Categorical variables are coded using
their integer codes.  Variables having no missing values will have
\code{NULL} matrices in the list.
}
\item{x}{if \code{x} is \code{TRUE}, the original data matrix with
  integer codes for categorical variables}
\item{rsq}{
for the last round of imputations, a vector containing the R-squares
with which each sometimes-missing variable could be predicted from the
others by \code{ace} or \code{avas}.
}}
\details{
The sequence of steps used by the \code{aregImpute} algorithm is the
following.
\cr
(1) For each variable containing m \code{NA}s where m > 0, initialize the
\code{NA}s to values from a random sample (without replacement if
a sufficient number of non-missing values exist) of size m from the
non-missing values.
\cr
(2) For \code{burnin+n.impute} iterations do the following steps.  The
first \code{burnin} iterations provide a burn-in, and imputations are
saved only from the last \code{n.impute} iterations.
\cr
(3) For each variable containing any \code{NA}s, draw a sample with
replacement from the observations in the entire dataset in which the
current variable being imputed is non-missing.  Fit a flexible
additive model to predict this target variable while finding the
optimum transformation of it (unless the identity
transformation is forced).  Use this fitted flexible model to
predict the target variable in all of the original observations.
Impute each missing value of the target variable with the observed
value whose predicted transformed value is closest to the predicted
transformed value of the missing value (if \code{match="closest"} and
\code{type="pmm"}), 
or use a draw from a multinomial distribution with probabilities derived
from distance weights, if \code{match="weighted"} (the default).
\cr
(4) After these imputations are computed, use these random draw
imputations the next time the curent target variable is used as a
predictor of other sometimes-missing variables.

When \code{match="closest"}, predictive mean matching does not work well
when fewer than 3 variables are used to predict the target variable,
because many of the multiple imputations for an observation will be
identical.  In the extreme case of one right-hand-side variable and
assuming that only monotonic transformations of left and right-side
variables are allowed, every bootstrap resample will give predicted
values of the target variable that are monotonically related to
predicted values from every other bootstrap resample.  The same is true
for Bayesian predicted values.  This causes predictive mean matching to
always match on the same donor observation.

When the missingness mechanism for a variable is so systematic that the
distribution of observed values is truncated, predictive mean matching
does not work.  It will only yield imputed values that are near observed
values, so intervals in which no values are observed will not be
populated by imputed values.  For this case, the only hope is to make
regression assumptions and use extrapolation.  With
\code{type="regression"}, \code{aregImpute} will use linear
extrapolation to obtain a (hopefully) reasonable distribution of imputed
values.  The \code{"regression"} option causes \code{aregImpute} to
impute missing values by adding a random sample of residuals (with
replacement if there are more \code{NA}s than measured values) on the
transformed scale of the target variable.  After random residuals are
added, predicted random draws are obtained on the original untransformed
scale using reverse linear interpolation on the table of original and
transformed target values (linear extrapolation when a random residual
is large enough to put the random draw prediction outside the range of
observed values).  The bootstrap is used as with \code{type="pmm"} to
factor in the uncertainty of the imputation model.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
f.harrell@vanderbilt.edu
}
\references{
  Little R, An H.  Robust likelihood-based analysis of multivariate data
  with missing values. Statistica Sinica 14:933-952, 2004.

  van Buuren S, Brand JPL, Groothuis-Oudshoorn CGM, Rubin DB.  Fully
  conditional specifications in multivariate imputation.  Draft
  available from \url{http://web.inter.nl.net/users/S.van.Buuren/publications/FCS\%20(revised\%20Jan\%202005).pdf}.
  }
\seealso{
\code{\link{fit.mult.impute}}, \code{\link{transcan}}, \code{\link{areg}}, \code{\link{naclus}}, \code{\link{naplot}}, \code{\link[mice]{mice}},
\code{\link{dotchart2}}, \code{\link{Ecdf}}
}
\examples{
# Check that aregImpute can almost exactly estimate missing values when
# there is a perfect nonlinear relationship between two variables
# Fit restricted cubic splines with 4 knots for x1 and x2, linear for x3
set.seed(3)
x1 <- rnorm(200)
x2 <- x1^2
x3 <- runif(200)
m <- 30
x2[1:m] <- NA
a <- aregImpute(~x1+x2+I(x3), n.impute=5, nk=4, match='closest')
a
matplot(x1[1:m]^2, a$imputed$x2)
abline(a=0, b=1, lty=2)

x1[1:m]^2
a$imputed$x2


# Multiple imputation and estimation of variances and covariances of
# regression coefficient estimates accounting for imputation
# Example 1: large sample size, much missing data, no overlap in
# NAs across variables
x1 <- factor(sample(c('a','b','c'),1000,TRUE))
x2 <- (x1=='b') + 3*(x1=='c') + rnorm(1000,0,2)
x3 <- rnorm(1000)
y  <- x2 + 1*(x1=='c') + .2*x3 + rnorm(1000,0,2)
orig.x1 <- x1[1:250]
orig.x2 <- x2[251:350]
x1[1:250] <- NA
x2[251:350] <- NA
d <- data.frame(x1,x2,x3,y)

# Use 100 imputations to better check against individual true values
f <- aregImpute(~y + x1 + x2 + x3, n.impute=100, data=d)
f
par(mfrow=c(2,1))
plot(f)
modecat <- function(u) {
 tab <- table(u)
 as.numeric(names(tab)[tab==max(tab)][1])
}
table(orig.x1,apply(f$imputed$x1, 1, modecat))
par(mfrow=c(1,1))
plot(orig.x2, apply(f$imputed$x2, 1, mean))
fmi <- fit.mult.impute(y ~ x1 + x2 + x3, lm, f, 
                       data=d)
sqrt(diag(Varcov(fmi)))
fcc <- lm(y ~ x1 + x2 + x3)
summary(fcc)   # SEs are larger than from mult. imputation


# Example 2: Very discriminating imputation models,
# x1 and x2 have some NAs on the same rows, smaller n
set.seed(5)
x1 <- factor(sample(c('a','b','c'),100,TRUE))
x2 <- (x1=='b') + 3*(x1=='c') + rnorm(100,0,.4)
x3 <- rnorm(100)
y  <- x2 + 1*(x1=='c') + .2*x3 + rnorm(100,0,.4)
orig.x1 <- x1[1:20]
orig.x2 <- x2[18:23]
x1[1:20] <- NA
x2[18:23] <- NA
#x2[21:25] <- NA
d <- data.frame(x1,x2,x3,y)
n <- naclus(d)
plot(n); naplot(n)  # Show patterns of NAs
# 100 imputations to study them; normally use 5 or 10
f  <- aregImpute(~y + x1 + x2 + x3, n.impute=100, nk=0, data=d)
par(mfrow=c(2,3))
plot(f, diagnostics=TRUE, maxn=2)
# Note: diagnostics=TRUE makes graphs similar to those made by:
# r <- range(f$imputed$x2, orig.x2)
# for(i in 1:6) {  # use 1:2 to mimic maxn=2
#   plot(1:100, f$imputed$x2[i,], ylim=r,
#        ylab=paste("Imputations for Obs.",i))
#   abline(h=orig.x2[i],lty=2)
# }


table(orig.x1,apply(f$imputed$x1, 1, modecat))
par(mfrow=c(1,1))
plot(orig.x2, apply(f$imputed$x2, 1, mean))


fmi <- fit.mult.impute(y ~ x1 + x2, lm, f, 
                       data=d)
sqrt(diag(Varcov(fmi)))
fcc <- lm(y ~ x1 + x2)
summary(fcc)   # SEs are larger than from mult. imputation

# Study relationship between smoothing parameter for weighting function
# (multiplier of mean absolute distance of transformed predicted
# values, used in tricube weighting function) and standard deviation
# of multiple imputations.  SDs are computed from average variances
# across subjects.  match="closest" same as match="weighted" with
# small value of fweighted.
# This example also shows problems with predicted mean
# matching almost always giving the same imputed values when there is
# only one predictor (regression coefficients change over multiple
# imputations but predicted values are virtually 1-1 functions of each
# other)

set.seed(23)
x <- runif(200)
y <- x + runif(200, -.05, .05)
r <- resid(lsfit(x,y))
rmse <- sqrt(sum(r^2)/(200-2))   # sqrt of residual MSE

y[1:20] <- NA
d <- data.frame(x,y)
f <- aregImpute(~ x + y, n.impute=10, match='closest', data=d)
# As an aside here is how to create a completed dataset for imputation
# number 3 as fit.mult.impute would do automatically.  In this degenerate
# case changing 3 to 1-2,4-10 will not alter the results.
completed <- d
imputed <- impute.transcan(f, imputation=3, data=d, list.out=TRUE,
                           pr=FALSE, check=FALSE)
completed[names(imputed)] <- imputed
completed
sd <- sqrt(mean(apply(f$imputed$y, 1, var)))

ss <- c(0, .01, .02, seq(.05, 1, length=20))
sds <- ss; sds[1] <- sd

for(i in 2:length(ss)) {
  f <- aregImpute(~ x + y, n.impute=10, fweighted=ss[i])
  sds[i] <- sqrt(mean(apply(f$imputed$y, 1, var)))
}

plot(ss, sds, xlab='Smoothing Parameter', ylab='SD of Imputed Values',
     type='b')
abline(v=.2,  lty=2)  # default value of fweighted
abline(h=rmse, lty=2)  # root MSE of residuals from linear regression

\dontrun{
# Do a similar experiment for the Titanic dataset
getHdata(titanic3)
h <- lm(age ~ sex + pclass + survived, data=titanic3)
rmse <- summary(h)$sigma
set.seed(21)
f <- aregImpute(~ age + sex + pclass + survived, n.impute=10,
                data=titanic3, match='closest')
sd <- sqrt(mean(apply(f$imputed$age, 1, var)))

ss <- c(0, .01, .02, seq(.05, 1, length=20))
sds <- ss; sds[1] <- sd

for(i in 2:length(ss)) {
  f <- aregImpute(~ age + sex + pclass + survived, data=titanic3,
                  n.impute=10, fweighted=ss[i])
  sds[i] <- sqrt(mean(apply(f$imputed$age, 1, var)))
}

plot(ss, sds, xlab='Smoothing Parameter', ylab='SD of Imputed Values',
     type='b')
abline(v=.2,   lty=2)  # default value of fweighted
abline(h=rmse, lty=2)  # root MSE of residuals from linear regression
}
}
\keyword{smooth}
\keyword{regression}
\keyword{multivariate}
\keyword{methods}
\keyword{models}
\concept{bootstrap}
\concept{predictive mean matching}
\concept{imputation}
\concept{NA}
