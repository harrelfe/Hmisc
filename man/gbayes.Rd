\name{gbayes}
\alias{gbayes}
\alias{plot.gbayes}
\alias{gbayes2}
\alias{gbayesMixPredNoData}
\alias{gbayesMixPost}
\alias{gbayesMixPowerNP}
\alias{gbayes1PowerNP}
\title{
Gaussian Bayesian Posterior and Predictive Distributions
}
\description{
\code{gbayes} derives the (Gaussian) posterior and optionally the predictive
distribution when both the prior and the likelihood are Gaussian, and
when the statistic of interest comes from a 2-sample problem.
This function is especially useful in obtaining the expected power of
a statistical test, averaging over the distribution of the population
effect parameter (e.g., log hazard ratio) that is obtained using
pilot data.  \code{gbayes} is also useful for summarizing studies for
which the statistic of interest is approximately Gaussian with
known variance.  An example is given for comparing two proportions
using the angular transformation, for which the variance is
independent of unknown parameters except for very extreme probabilities.
A \code{plot} method is also given.  This plots the prior, posterior, and
predictive distributions on a single graph using a nice default for
the x-axis limits and using the \code{labcurve} function for automatic
labeling of the curves.


\code{gbayes2} uses the method of Spiegelhalter and Freedman (1986) to compute the
probability of correctly concluding that a new treatment is superior
to a control.  By this we mean that a 1-\code{alpha} normal
theory-based confidence interval for the new minus old treatment
effect lies wholly to the right of \code{delta.w}, where \code{delta.w} is the
minimally worthwhile treatment effect (which can be zero to be
consistent with ordinary null hypothesis testing, a method not always
making sense).  This kind of power function is averaged over a prior
distribution for the unknown treatment effect.  This procedure is
applicable to the situation where a prior distribution is not to be
used in constructing the test statistic or confidence interval, but is
only used for specifying the distribution of \code{delta}, the parameter of
interest.


Even though \code{gbayes2}
assumes that the test statistic has a normal distribution with known
variance (which is strongly a function of the sample size in the two
treatment groups), the prior distribution function can be completely
general.  Instead of using a step-function for the prior distribution
as Spiegelhalter and Freedman used in their appendix, \code{gbayes2} uses
the built-in \code{integrate} function for numerical integration.
\code{gbayes2} also allows the variance of the test statistic to be general
as long as it is evaluated by the user.  The conditional power given the
parameter of interest \code{delta} is \code{1 - pnorm((delta.w - delta)/sd + z)}, where z
is the normal critical value corresponding to 1 - \code{alpha}/2.

\code{gbayesMixPredNoData} derives the predictive distribution of a
statistic that is Gaussian given \code{delta} when no data have yet been
observed and when the prior is a mixture of two Gaussians.

\code{gbayesMixPost} derives the posterior density, cdf, or posterior
mean of \code{delta} given 
the statistic \code{x}, when the prior for \code{delta} is a mixture of two
Gaussians and when \code{x} is Gaussian given \code{delta}.

\code{gbayesMixPowerNP} computes the power for a test for \code{delta} > \code{delta.w}
for the case where (1) a Gaussian prior or mixture of two Gaussian priors
is used as the prior distribution, (2) this prior is used in forming
the statistical test or credible interval, (3) no prior is used for
the distribution of \code{delta} for computing power but instead a fixed
single \code{delta} is given (as in traditional frequentist hypothesis
tests), and (4) the test statistic has a Gaussian likelihood with
known variance (and mean equal to the specified \code{delta}).
\code{gbayesMixPowerNP} is handy where you want to use an earlier study in
testing for treatment effects in a new study, but you want to mix with
this prior a non-informative prior.  The mixing probability \code{mix} can
be thought of as the "applicability" of the previous study.  As with
\code{gbayes2}, power here means the probability that the new study will
yield a left credible interval that is to the right of \code{delta.w}.
\code{gbayes1PowerNP} is a special case of \code{gbayesMixPowerNP} when the
prior is a single Gaussian.
}
\usage{
gbayes(mean.prior, var.prior, m1, m2, stat, var.stat, 
       n1, n2, cut.prior, cut.prob.prior=0.025)

\method{plot}{gbayes}(x, xlim, ylim, name.stat='z', \dots)

gbayes2(sd, prior, delta.w=0, alpha=0.05, upper=Inf, prior.aux)

gbayesMixPredNoData(mix=NA, d0=NA, v0=NA, d1=NA, v1=NA,
                    what=c('density','cdf'))

gbayesMixPost(x=NA, v=NA, mix=1, d0=NA, v0=NA, d1=NA, v1=NA,
              what=c('density','cdf','postmean'))

gbayesMixPowerNP(pcdf, delta, v, delta.w=0, mix, interval,
                 nsim=0, alpha=0.05)

gbayes1PowerNP(d0, v0, delta, v, delta.w=0, alpha=0.05)
}
\arguments{
\item{mean.prior}{
mean of the prior distribution
}
\item{cut.prior,cut.prob.prior,var.prior}{
variance of the prior.  Use a large number such as 10000 to effectively
use a flat (noninformative) prior.  Sometimes it is useful to compute
the variance so that the prior probability that \code{stat} is greater than
some impressive value \code{u} is only \code{alpha}.  The correct
\code{var.prior} to use is then \code{((u-mean.prior)/qnorm(1-alpha))^2}.
You can specify \code{cut.prior=u} and \code{cut.prob.prior=alpha} (whose default is 0.025)
in place of \code{var.prior} to have \code{gbayes} compute the prior variance in this
manner. 
}
\item{m1}{
sample size in group 1
}
\item{m2}{
sample size in group 2
}
\item{stat}{
statistic comparing groups 1 and 2, e.g., log hazard ratio, difference
in means, difference in angular transformations of proportions
}
\item{var.stat}{
variance of \code{stat}, assumed to be known.  \code{var.stat} should either
be a constant (allowed if \code{n1} is not specified), or a function of
two arguments which specify the sample sizes in groups 1 and 2. 
Calculations will be approximate when the variance is estimated from the data.
}
\item{x}{
an object returned by \code{gbayes} or the value of the statistic which
is an estimator of delta, the parameter of interest
}
\item{sd}{
the standard deviation of the treatment effect
}
\item{prior}{
a function of possibly a vector of unknown treatment effects,
returning the prior density at those values
}
\item{pcdf}{
a function computing the posterior CDF of the treatment effect
\code{delta}, such as a function created by \code{gbayesMixPost} with
\code{what="cdf"}.
}
\item{delta}{
a true unknown single treatment effect to detect
}
\item{v}{
the variance of the statistic \code{x}, e.g., \code{s^2 * (1/n1 + 1/n2)}.
Neither \code{x} nor \code{v} need to be defined to
\code{gbayesMixPost}, as they can be defined at run time to the function
created by \code{gbayesMixPost}.
}
\item{n1}{
number of future observations in group 1, for obtaining a predictive
distribution
}
\item{n2}{
number of future observations in group 2
}
\item{xlim}{
vector of 2 x-axis limits.  Default is the mean of the posterior plus or
minus 6 standard deviations of the posterior.
}
\item{ylim}{
vector of 2 y-axis limits.  Default is the range over combined prior and 
posterior densities.
}
\item{name.stat}{
label for x-axis.  Default is \code{"z"}.
}
\item{...}{
optional arguments passed to \code{labcurve} from \code{plot.gbayes}
}
\item{delta.w}{
the minimum worthwhile treatment difference to detech.  The default is
zero for a plain uninteristing null hypothesis.
}
\item{alpha}{
type I error, or more accurately one minus the confidence level for a
two-sided confidence limit for the treatment effect
}
\item{upper}{
upper limit of integration over the prior distribution multiplied by
the normal likelihood for the treatment effect statistic.  Default is
infinity.
}
\item{prior.aux}{
argument to pass to \code{prior} from \code{integrate} through \code{gbayes2}.
Inside of \code{power} the argument must be named \code{prior.aux} if it
exists.  You can pass multiple parameters by passing \code{prior.aux} as a
list and pulling off elements of the list inside \code{prior}.  This setup
was used because of difficulties in passing \code{\dots} arguments through
\code{integrate} for some situations.
}
\item{mix}{
mixing probability or weight for the Gaussian prior having mean \code{d0}
and variance \code{v0}.  \code{mix} must be between 0 and 1, inclusive.
}
\item{d0}{
mean of the first Gaussian distribution (only Gaussian for
\code{gbayes1PowerNP} and is a required argument)
}
\item{v0}{
variance of the first Gaussian (only Gaussian for
\code{gbayes1PowerNP} and is a required argument)
}
\item{d1}{
mean of the second Gaussian (if \code{mix} < 1)
}
\item{v1}{
variance of the second Gaussian (if \code{mix} < 1).  Any of these last 5
arguments can be omitted to \code{gbayesMixPredNoData} as they can be
provided at run time to the function created by \code{gbayesMixPredNoData}.
}
\item{what}{
specifies whether the predictive density or the CDF is to be
computed.  Default is \code{"density"}.
}
\item{interval}{
a 2-vector containing the lower and upper limit for possible values of
the test statistic \code{x} that would result in a left credible interval
exceeding \code{delta.w} with probability 1-\code{alpha}/2
}
\item{nsim}{
defaults to zero, causing \code{gbayesMixPowerNP} to solve numerically for the
critical value of \code{x}, then to compute the power accordingly.  Specify
a nonzero number such as 20000 for \code{nsim} to instead have the function
estimate power by simulation.  In this case 0.95 confidence limits on
the estimated power are also computed.  This approach is sometimes
necessary if \code{uniroot} can't solve the equation for the critical value.
}}
\value{
\code{gbayes} returns a list of class \code{"gbayes"} containing the following
names elements: \code{mean.prior},\code{var.prior},\code{mean.post}, \code{var.post}, and
if \code{n1} is specified, \code{mean.pred} and \code{var.pred}.  Note that
\code{mean.pred} is  identical to \code{mean.post}.  \code{gbayes2} returns a single
number which is the probability of correctly rejecting the null
hypothesis in favor of the new treatment.  \code{gbayesMixPredNoData}
returns a function that can be used to evaluate the predictive density
or cumulative distribution.  \code{gbayesMixPost} returns a function that
can be used to evaluate the posterior density or cdf.  \code{gbayesMixPowerNP}
returns a vector containing two values if \code{nsim} = 0.  The first value is the
critical value for the test statistic that will make the left credible
interval > \code{delta.w}, and the second value is the power.  If \code{nsim} > 0,
it returns the power estimate and confidence limits for it if \code{nsim} >
0.  The examples show how to use these functions.  
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
\seealso{\code{\link{gbayesSeqSim}}}
\references{
Spiegelhalter DJ, Freedman LS, Parmar MKB (1994): Bayesian approaches to
randomized trials.  JRSS A 157:357--416.  Results for \code{gbayes} are derived from
Equations 1, 2, 3, and 6.


Spiegelhalter DJ, Freedman LS (1986): A predictive approach to
selecting the size of a clinical trial, based on subjective clinical
opinion.  Stat in Med 5:1--13.


Joseph, Lawrence and Belisle, Patrick (1997): Bayesian sample size
determination for normal means and differences between normal means.
The Statistician 46:209--226.

Grouin, JM, Coste M, Bunouf P, Lecoutre B (2007): Bayesian sample size
determination in non-sequential clinical trials: Statistical aspects and
some regulatory considerations.  Stat in Med 26:4914--4924.
}
\examples{
# Compare 2 proportions using the var stabilizing transformation
# arcsin(sqrt((x+3/8)/(n+3/4))) (Anscombe), which has variance 
# 1/[4(n+.5)]


m1 <- 100;     m2 <- 150
deaths1 <- 10; deaths2 <- 30


f <- function(events,n) asin(sqrt((events+3/8)/(n+3/4)))
stat <- f(deaths1,m1) - f(deaths2,m2)
var.stat <- function(m1, m2) 1/4/(m1+.5) + 1/4/(m2+.5)
cat("Test statistic:",format(stat),"  s.d.:",
    format(sqrt(var.stat(m1,m2))), "\n")
#Use unbiased prior with variance 1000 (almost flat)
b <- gbayes(0, 1000, m1, m2, stat, var.stat, 2*m1, 2*m2)
print(b)
plot(b)
#To get posterior Prob[parameter > w] use 
# 1-pnorm(w, b$mean.post, sqrt(b$var.post))


#If g(effect, n1, n2) is the power function to
#detect an effect of 'effect' with samples size for groups 1 and 2
#of n1,n2, estimate the expected power by getting 1000 random
#draws from the posterior distribution, computing power for
#each value of the population effect, and averaging the 1000 powers
#This code assumes that g will accept vector-valued 'effect'
#For the 2-sample proportion problem just addressed, 'effect'
#could be taken approximately as the change in the arcsin of
#the square root of the probability of the event


g <- function(effect, n1, n2, alpha=.05) {
  sd <- sqrt(var.stat(n1,n2))
  z <- qnorm(1 - alpha/2)
  effect <- abs(effect)
  1 - pnorm(z - effect/sd) + pnorm(-z - effect/sd)
}


effects <- rnorm(1000, b$mean.post, sqrt(b$var.post))
powers <- g(effects, 500, 500)
hist(powers, nclass=35, xlab='Power')
describe(powers)




# gbayes2 examples
# First consider a study with a binary response where the
# sample size is n1=500 in the new treatment arm and n2=300
# in the control arm.  The parameter of interest is the 
# treated:control log odds ratio, which has variance
# 1/[n1 p1 (1-p1)] + 1/[n2 p2 (1-p2)].  This is not
# really constant so we average the variance over plausible
# values of the probabilities of response p1 and p2.  We
# think that these are between .4 and .6 and we take a 
# further short cut


v <- function(n1, n2, p1, p2) 1/(n1*p1*(1-p1)) + 1/(n2*p2*(1-p2))
n1 <- 500; n2 <- 300
ps <- seq(.4, .6, length=100)
vguess <- quantile(v(n1, n2, ps, ps), .75)
vguess
#        75\% 
# 0.02183459


# The minimally interesting treatment effect is an odds ratio
# of 1.1.  The prior distribution on the log odds ratio is
# a 50:50 mixture of a vague Gaussian (mean 0, sd 100) and
# an informative prior from a previous study (mean 1, sd 1)


prior <- function(delta) 
  0.5*dnorm(delta, 0, 100)+0.5*dnorm(delta, 1, 1)
deltas <- seq(-5, 5, length=150)
plot(deltas, prior(deltas), type='l')


# Now compute the power, averaged over this prior
gbayes2(sqrt(vguess), prior, log(1.1))
# [1] 0.6133338


# See how much power is lost by ignoring the previous
# study completely


gbayes2(sqrt(vguess), function(delta)dnorm(delta, 0, 100), log(1.1))
# [1] 0.4984588


# What happens to the power if we really don't believe the treatment
# is very effective?  Let's use a prior distribution for the log
# odds ratio that is uniform between log(1.2) and log(1.3).
# Also check the power against a true null hypothesis


prior2 <- function(delta) dunif(delta, log(1.2), log(1.3))
gbayes2(sqrt(vguess), prior2, log(1.1))
# [1] 0.1385113


gbayes2(sqrt(vguess), prior2, 0)
# [1] 0.3264065


# Compare this with the power of a two-sample binomial test to
# detect an odds ratio of 1.25
bpower(.5, odds.ratio=1.25, n1=500, n2=300)
#     Power 
# 0.3307486


# For the original prior, consider a new study with equal
# sample sizes n in the two arms.  Solve for n to get a
# power of 0.9.  For the variance of the log odds ratio
# assume a common p in the center of a range of suspected
# probabilities of response, 0.3.  For this example we
# use a zero null value and the uniform prior above


v   <- function(n) 2/(n*.3*.7)
pow <- function(n) gbayes2(sqrt(v(n)), prior2)
uniroot(function(n) pow(n)-0.9, c(50,10000))$root
# [1] 2119.675
# Check this value
pow(2119.675)
# [1] 0.9


# Get the posterior density when there is a mixture of two priors,
# with mixing probability 0.5.  The first prior is almost
# non-informative (normal with mean 0 and variance 10000) and the
# second has mean 2 and variance 0.3.  The test statistic has a value
# of 3 with variance 0.4.
f <- gbayesMixPost(3, 4, mix=0.5, d0=0, v0=10000, d1=2, v1=0.3)


args(f)


# Plot this density
delta <- seq(-2, 6, length=150)
plot(delta, f(delta), type='l')


# Add to the plot the posterior density that used only
# the almost non-informative prior
lines(delta, f(delta, mix=1), lty=2)


# The same but for an observed statistic of zero
lines(delta, f(delta, mix=1, x=0), lty=3)


# Derive the CDF instead of the density
g <- gbayesMixPost(3, 4, mix=0.5, d0=0, v0=10000, d1=2, v1=0.3,
                   what='cdf')
# Had mix=0 or 1, gbayes1PowerNP could have been used instead
# of gbayesMixPowerNP below


# Compute the power to detect an effect of delta=1 if the variance
# of the test statistic is 0.2
gbayesMixPowerNP(g, 1, 0.2, interval=c(-10,12))


# Do the same thing by simulation
gbayesMixPowerNP(g, 1, 0.2, interval=c(-10,12), nsim=20000)


# Compute by what factor the sample size needs to be larger
# (the variance needs to be smaller) so that the power is 0.9
ratios <- seq(1, 4, length=50)
pow <- single(50)
for(i in 1:50) 
  pow[i] <- gbayesMixPowerNP(g, 1, 0.2/ratios[i], interval=c(-10,12))[2]


# Solve for ratio using reverse linear interpolation
approx(pow, ratios, xout=0.9)$y


# Check this by computing power
gbayesMixPowerNP(g, 1, 0.2/2.1, interval=c(-10,12))
# So the study will have to be 2.1 times as large as earlier thought
}
\keyword{htest}
\concept{study design}
\concept{power}

