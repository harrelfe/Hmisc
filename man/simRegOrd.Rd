\name{simRegOrd}
\alias{simRegOrd}
\title{Simulate Power for Adjusted Ordinal Regression Two-Sample Test}
\description{
	This function simulates the power of a two-sample test from a
	proportional odds ordinal logistic model for a continuous response
	variable- a generalization of the Wilcoxon test.  The continuous data
	model is normal with equal variance.  Nonlinear covariate
	adjustment is allowed, and the user can optionally specify discrete
	ordinal level overrides to the continuous response.  For example, if
	the main response is systolic blood pressure, one can add two ordinal
	categories higher than the highest observed blood pressure to capture
	heart attack or death.
}
\usage{
simRegOrd(n, nsim=1000, delta=0, odds.ratio=1, sigma,
          p=NULL, x=NULL, X=x, Eyx, alpha=0.05, pr=FALSE)
}
\arguments{
	\item{n}{combined sample size (both groups combined)}
	\item{nsim}{number of simulations to run}
	\item{delta}{difference in means to detect, for continuous portion of
		response variable}
	\item{odds.ratio}{odds ratio to detect for ordinal overrides of
		continuous portion}
	\item{sigma}{standard deviation for continuous portion of response}
	\item{p}{a vector of marginal cell probabilities which must add up to one.
		The \code{i}th element specifies the probability that a patient will be
		in response level \code{i} for the control arm for the discrete
		ordinal overrides.}
	\item{x}{optional covariate to adjust for - a vector of length
		\code{n}}
	\item{X}{a design matrix for the adjustment covariate \code{x} if
		present.  This could represent for example \code{x} and \code{x^2}
		or cubic spline components.}
	\item{Eyx}{a function of \code{x} that provides the mean response for
		the control arm treatment}
	\item{alpha}{type I error}
	\item{pr}{set to \code{TRUE} to see iteration progress}
	}
\value{
a list containing \code{n, delta, sigma, power, betas, se, pvals} where
\code{power} is the estimated power (scalar), and \code{betas, se,
	pvals} are \code{nsim}-vectors containing, respectively, the ordinal
model treatment effect estimate, standard errors, and 2-tailed
p-values.  When a model fit failed, the corresponding entries in
\code{betas, se, pvals} are \code{NA} and \code{power} is the proportion
of non-failed iterations for which the treatment p-value is significant
at the \code{alpha} level.
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
\seealso{\code{\link{popower}}}
\examples{
\dontrun{
## First use no ordinal high-end category overrides, and compare power
## to t-test when there is no covariate

n <- 100
delta <- .5
sd <- 1
require(pwr)
power.t.test(n = n / 2, delta=delta, sd=sd, type='two.sample')  # 0.70
set.seed(1)
w <- simRegOrd(n, delta=delta, sigma=sd, pr=TRUE)     # 0.686

## Now do ANCOVA with a quadratic effect of a covariate
n <- 100
x <- rnorm(n)
w <- simRegOrd(n, nsim=400, delta=delta, sigma=sd, x=x,
               X=cbind(x, x^2),
               Eyx=function(x) x + x^2, pr=TRUE)
w$power  # 0.68

## Fit a cubic spline to some simulated pilot data and use the fitted
## function as the true equation in the power simulation
require(rms)
N <- 1000
set.seed(2)
x <- rnorm(N)
y <- x + x^2 + rnorm(N, 0, sd=sd)
f <- ols(y ~ rcs(x, 4), x=TRUE)

n <- 100
j <- sample(1 : N, n, replace=n > N)
x <-   x[j]
X <- f$x[j,]
w <- simRegOrd(n, nsim=400, delta=delta, sigma=sd, x=x,
               X=X,
               Eyx=Function(f), pr=TRUE)
w$power  ## 0.70

## Finally, add discrete ordinal category overrides and high end of y
## Start with no effect of treatment on these ordinal event levels (OR=1.0)

w <- simRegOrd(n, nsim=400, delta=delta, odds.ratio=1, sigma=sd,
               x=x, X=X, Eyx=Function(f),
               p=c(.98, .01, .01),
               pr=TRUE)
w$power  ## 0.61   (0.3 if p=.8 .1 .1, 0.37 for .9 .05 .05, 0.50 for .95 .025 .025)

## Now assume that odds ratio for treatment is 2.5
## First compute power for clinical endpoint portion of Y alone
or <- 2.5
p <- c(.9, .05, .05)
popower(p, odds.ratio=or, n=100)   # 0.275
## Compute power of t-test on continuous part of Y alone
power.t.test(n = 100 / 2, delta=delta, sd=sd, type='two.sample')  # 0.70
## Note this is the same as the p.o. model power from simulation above
## Solve for OR that gives the same power estimate from popower
popower(rep(.01, 100), odds.ratio=2.4, n=100)   # 0.706
## Compute power for continuous Y with ordinal override
w <- simRegOrd(n, nsim=400, delta=delta, odds.ratio=or, sigma=sd,
               x=x, X=X, Eyx=Function(f),
               p=c(.9, .05, .05),
               pr=TRUE)
w$power  ## 0.72
}
}
\keyword{htest}
\keyword{category}
\concept{power}
\concept{study design}
\concept{ordinal logistic model}
\concept{ordinal response}
\concept{proportional odds model}
