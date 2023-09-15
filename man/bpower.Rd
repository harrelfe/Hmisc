\name{bpower}
\alias{bpower}
\alias{bsamsize}
\alias{ballocation}
\alias{bpower.sim}
\title{
Power and Sample Size for Two-Sample Binomial Test
}
\description{
Uses method of Fleiss, Tytun, and Ury (but without the continuity
correction) to estimate the power (or the sample size to achieve a given
power) of a two-sided test for the difference in two proportions.  The two
sample sizes are allowed to be unequal, but for \code{bsamsize} you must specify
the fraction of observations in group 1.  For power calculations, one
probability (\code{p1}) must be given, and either the other probability (\code{p2}),
an \code{odds.ratio}, or a \code{percent.reduction} must be given.  For \code{bpower} or
\code{bsamsize}, any or all of the arguments may be vectors, in which case they
return a vector of powers or sample sizes.  All vector arguments must have
the same length.


Given \code{p1, p2}, \code{ballocation} uses the method of Brittain and Schlesselman
to compute the optimal fraction of observations to be placed in group 1
that either (1) minimize the variance of the difference in two proportions,
(2) minimize the variance of the ratio of the two proportions, 
(3) minimize the variance of the log odds ratio, or
(4) maximize the power of the 2-tailed test for differences.  For (4)
the total sample size must be given, or the fraction optimizing
the power is not returned.  The fraction for (3) is one minus the fraction
for (1).


\code{bpower.sim} estimates power by simulations, in minimal time.  By using
\code{bpower.sim} you can see that the formulas without any continuity correction
are quite accurate, and that the power of a continuity-corrected test
is significantly lower.  That's why no continuity corrections are implemented
here.
}
\usage{
bpower(p1, p2, odds.ratio, percent.reduction, 
       n, n1, n2, alpha=0.05)


bsamsize(p1, p2, fraction=.5, alpha=.05, power=.8)


ballocation(p1, p2, n, alpha=.05)


bpower.sim(p1, p2, odds.ratio, percent.reduction, 
           n, n1, n2, 
           alpha=0.05, nsim=10000)
}
\arguments{
\item{p1}{
population probability in the group 1
}
\item{p2}{
probability for group 2
}
\item{odds.ratio}{odds ratio to detect}
\item{percent.reduction}{percent reduction in risk to detect}
\item{n}{
total sample size over the two groups.  If you omit this for
\code{ballocation}, the \code{fraction} which optimizes power will not be
returned.
}
\item{n1}{sample size in group 1}
\item{n2}{sample size in group 2.  \code{bpower}, if \code{n} is given,
\code{n1} and \code{n2} are set to \code{n/2}.}
\item{alpha}{type I assertion probability}
\item{fraction}{fraction of observations in group 1}
\item{power}{the desired probability of detecting a difference}
\item{nsim}{number of simulations of binomial responses}
}
\value{
for \code{bpower}, the power estimate; for \code{bsamsize}, a vector containing
the sample sizes in the two groups; for \code{ballocation}, a vector with
4 fractions of observations allocated to group 1, optimizing the four
criteria mentioned above.  For \code{bpower.sim}, a vector with three
elements is returned, corresponding to the simulated power and its
lower and upper 0.95 confidence limits.
}
\details{
For \code{bpower.sim}, all arguments must be of length one.
}
\section{AUTHOR}{
Frank Harrell


Department of Biostatistics


Vanderbilt University


\email{fh@fharrell.com}
}
\references{
Fleiss JL, Tytun A, Ury HK (1980): A simple approximation for calculating
sample sizes for comparing independent proportions.  Biometrics 36:343--6.


Brittain E, Schlesselman JJ (1982): Optimal allocation for the comparison
of proportions.  Biometrics 38:1003--9.


Gordon I, Watson R (1996): The myth of continuity-corrected sample size
formulae.  Biometrics 52:71--6.
}
\seealso{
\code{\link{samplesize.bin}}, \code{\link{chisq.test}}, \code{\link{binconf}}
}
\examples{
bpower(.1, odds.ratio=.9, n=1000, alpha=c(.01,.05))
bpower.sim(.1, odds.ratio=.9, n=1000)
bsamsize(.1, .05, power=.95)
ballocation(.1, .5, n=100)


# Plot power vs. n for various odds ratios  (base prob.=.1)
n  <- seq(10, 1000, by=10)
OR <- seq(.2,.9,by=.1)
plot(0, 0, xlim=range(n), ylim=c(0,1), xlab="n", ylab="Power", type="n")
for(or in OR) {
  lines(n, bpower(.1, odds.ratio=or, n=n))
  text(350, bpower(.1, odds.ratio=or, n=350)-.02, format(or))
}


# Another way to plot the same curves, but letting labcurve do the
# work, including labeling each curve at points of maximum separation
pow <- lapply(OR, function(or,n)list(x=n,y=bpower(p1=.1,odds.ratio=or,n=n)),
              n=n)
names(pow) <- format(OR)
labcurve(pow, pl=TRUE, xlab='n', ylab='Power')


# Contour graph for various probabilities of outcome in the control
# group, fixing the odds ratio at .8 ([p2/(1-p2) / p1/(1-p1)] = .8)
# n is varied also
p1 <- seq(.01,.99,by=.01)
n  <- seq(100,5000,by=250)
pow <- outer(p1, n, function(p1,n) bpower(p1, n=n, odds.ratio=.8))
# This forms a length(p1)*length(n) matrix of power estimates
contour(p1, n, pow)
}
\keyword{htest}
\keyword{category}
\concept{power}
\concept{study design}
