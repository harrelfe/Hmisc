\name{wtd.stats}
\alias{wtd.mean}
\alias{wtd.var}
\alias{wtd.quantile}
\alias{wtd.Ecdf}
\alias{wtd.table}
\alias{wtd.rank}
\alias{wtd.loess.noiter}
\alias{num.denom.setup}
\title{
Weighted Statistical Estimates
}
\description{
These functions compute various weighted versions of standard
estimators.  In most cases the \code{weights} vector is a vector the same
length of \code{x}, containing frequency counts that in effect expand \code{x}
by these counts.  \code{weights} can also be sampling weights, in which
setting \code{normwt} to \code{TRUE} will often be appropriate.  This results in
making \code{weights} sum to the length of the non-missing elements in
\code{x}.  \code{normwt=TRUE} thus reflects the fact that the true sample size is
the length of the \code{x} vector and not the sum of the original values of
\code{weights} (which would be appropriate had \code{normwt=FALSE}).  When \code{weights}
is all ones, the estimates are all identical to unweighted estimates
(unless one of the non-default quantile estimation options is
specified to \code{wtd.quantile}).  When missing data have already been
deleted for, \code{x}, \code{weights}, and (in the case of \code{wtd.loess.noiter}) \code{y},
specifying \code{na.rm=FALSE} will save computation time.  Omitting the
\code{weights} argument or specifying \code{NULL} or a zero-length vector will
result in the usual unweighted estimates.

\code{wtd.mean}, \code{wtd.var}, and \code{wtd.quantile} compute
weighted means, variances, and quantiles, respectively.  \code{wtd.Ecdf}
computes a weighted empirical distribution function.  \code{wtd.table}
computes a weighted frequency table (although only one stratification
variable is supported at present).  \code{wtd.rank} computes weighted
ranks, using mid--ranks for ties.  This can be used to obtain Wilcoxon
tests and rank correlation coefficients.  \code{wtd.loess.noiter} is a
weighted version of \code{loess.smooth} when no iterations for outlier
rejection are desired. This results in especially good smoothing when
\code{y} is binary.  \code{wtd.quantile} removes any observations with
zero weight at the beginning.  Previously, these were changing the
quantile estimates.

\code{num.denom.setup} is a utility function that allows one to deal with
observations containing numbers of events and numbers of trials, by
outputting two observations when the number of events and non-events
(trials - events) exceed zero.  A vector of subscripts is generated
that will do the proper duplications of observations, and a new binary
variable \code{y} is created along with usual cell frequencies (\code{weights})
for each of the \code{y=0}, \code{y=1} cells per observation.
}
\usage{
wtd.mean(x, weights=NULL, normwt="ignored", na.rm=TRUE)
wtd.var(x, weights=NULL, normwt=FALSE, na.rm=TRUE,
        method=c('unbiased', 'ML'))
wtd.quantile(x, weights=NULL, probs=c(0, .25, .5, .75, 1), 
             type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
             normwt=FALSE, na.rm=TRUE)
wtd.Ecdf(x, weights=NULL, 
         type=c('i/n','(i-1)/(n-1)','i/(n+1)'), 
         normwt=FALSE, na.rm=TRUE)
wtd.table(x, weights=NULL, type=c('list','table'), 
          normwt=FALSE, na.rm=TRUE)
wtd.rank(x, weights=NULL, normwt=FALSE, na.rm=TRUE)
wtd.loess.noiter(x, y, weights=rep(1,n),
                 span=2/3, degree=1, cell=.13333, 
                 type=c('all','ordered all','evaluate'), 
                 evaluation=100, na.rm=TRUE)
num.denom.setup(num, denom)
}
\arguments{
\item{x}{
a numeric vector (may be a character or \code{category} or \code{factor} vector
for \code{wtd.table})
}
\item{num}{
vector of numerator frequencies
}
\item{denom}{
vector of denominators (numbers of trials)
}
\item{weights}{
a numeric vector of weights
}
\item{normwt}{
specify \code{normwt=TRUE} to make \code{weights} sum to
\code{length(x)} after deletion of \code{NA}s.  If \code{weights} are
frequency weights, then \code{normwt} should be \code{FALSE}, and if
\code{weights} are normalization (aka reliability) weights, then
\code{normwt} should be \code{TRUE}. In the case of the former, no check
is made that \code{weights} are valid frequencies. 
}
\item{na.rm}{
set to \code{FALSE} to suppress checking for NAs
}
\item{method}{determines the estimator type; if \code{'unbiased'} (the
	default) then the usual unbiased estimate (using Bessel's correction)
	is returned, if \code{'ML'} then it is the maximum likelihood estimate
	for a Gaussian distribution. In the case of the latter, the
	\code{normwt} argument has no effect.  Uses \code{stats:cov.wt} for
	both methods.}
\item{probs}{
a vector of quantiles to compute.  Default is 0 (min), .25, .5, .75, 1
(max).
}
\item{type}{
For \code{wtd.quantile}, \code{type} defaults to \code{quantile} to use the same
interpolated order statistic method as \code{quantile}.  Set \code{type} to 
\code{"(i-1)/(n-1)"},\code{"i/(n+1)"}, or \code{"i/n"} to use the inverse of the
empirical distribution function, using, respectively, (wt - 1)/T,
wt/(T+1), or wt/T, where wt is the cumulative weight and T is the
total weight (usually total sample size).  These three values of
\code{type} are the possibilities for \code{wtd.Ecdf}.  For \code{wtd.table} the
default \code{type} is \code{"list"}, meaning that the function is to return a
list containing two vectors: \code{x} is the sorted unique values of \code{x}
and \code{sum.of.weights} is the sum of weights for that \code{x}.  This is the
default so that you don't have to convert the \code{names} attribute of the
result that can be obtained with \code{type="table"} to a numeric variable
when \code{x} was originally numeric.  \code{type="table"} for \code{wtd.table}
results in an object that is the same structure as those returned from
\code{table}.  For \code{wtd.loess.noiter} the default \code{type} is \code{"all"},
indicating that the function is to return a list containing all the
original values of \code{x} (including duplicates and without sorting) and
the smoothed \code{y} values corresponding to them.  Set \code{type="ordered
all"} to sort by \code{x}, and \code{type="evaluate"} to evaluate the smooth
only at \code{evaluation} equally spaced points between the observed limits
of \code{x}.
}
\item{y}{a numeric vector the same length as \code{x}}
\item{span, degree, cell, evaluation}{
see \code{loess.smooth}.  The default is linear (\code{degree}=1) and 100 points
to evaluation (if \code{type="evaluate"}).
}}
\value{
\code{wtd.mean} and \code{wtd.var} return scalars.  \code{wtd.quantile} returns a
vector the same length as \code{probs}.  \code{wtd.Ecdf} returns a list whose
elements \code{x} and \code{Ecdf} correspond to unique sorted values of \code{x}.
If the first CDF estimate is greater than zero, a point (min(x),0) is
placed at the beginning of the estimates.
See above for \code{wtd.table}.  \code{wtd.rank} returns a vector the same
length as \code{x} (after removal of NAs, depending on \code{na.rm}).  See above
for \code{wtd.loess.noiter}.
}
\details{
The functions correctly combine weights of observations having
duplicate values of \code{x} before computing estimates.

When \code{normwt=FALSE} the weighted variance will not equal the
unweighted variance even if the weights are identical.  That is because
of the subtraction of 1 from the sum of the weights in the denominator
of the variance formula.  If you want the weighted variance to equal the
unweighted variance when weights do not vary, use \code{normwt=TRUE}.
The articles by Gatz and Smith discuss alternative approaches, to arrive
at estimators of the standard error of a weighted mean.

\code{wtd.rank} does not handle NAs as elegantly as \code{rank} if
\code{weights} is specified.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University School of Medicine
\cr
\email{fh@fharrell.com}
\cr
Benjamin Tyner
\cr
\email{btyner@gmail.com}
}
\references{
Research Triangle Institute (1995): SUDAAN User's Manual, Release
6.40, pp. 8-16 to 8-17.

Gatz DF, Smith L (1995): The standard error of a weighted mean
concentration--I.  Bootstrapping vs other methods.  Atmospheric Env
11:1185-1193.

Gatz DF, Smith L (1995): The standard error of a weighted mean
concentration--II.  Estimating confidence intervals.  Atmospheric Env
29:1195-1200.

https://en.wikipedia.org/wiki/Weighted_arithmetic_mean
}
\seealso{
\code{\link{mean}}, \code{\link{var}}, \code{\link{quantile}}, \code{\link{table}}, \code{\link{rank}}, \code{\link{loess.smooth}}, \code{\link{lowess}},
\code{\link{plsmo}}, \code{\link{Ecdf}}, \code{\link{somers2}}, \code{\link{describe}}
}
\examples{
set.seed(1)
x <- runif(500)
wts <- sample(1:6, 500, TRUE)
std.dev <- sqrt(wtd.var(x, wts))
wtd.quantile(x, wts)
death <- sample(0:1, 500, TRUE)
plot(wtd.loess.noiter(x, death, wts, type='evaluate'))
describe(~x, weights=wts)
# describe uses wtd.mean, wtd.quantile, wtd.table
xg <- cut2(x,g=4)
table(xg)
wtd.table(xg, wts, type='table')

# Here is a method for getting stratified weighted means
y <- runif(500)
g <- function(y) wtd.mean(y[,1],y[,2])
summarize(cbind(y, wts), llist(xg), g, stat.name='y')

# Empirically determine how methods used by wtd.quantile match with
# methods used by quantile, when all weights are unity
set.seed(1)
u <-  eval(formals(wtd.quantile)$type)
v <- as.character(1:9)
r <- matrix(0, nrow=length(u), ncol=9, dimnames=list(u,v))

for(n in c(8, 13, 22, 29))
  {
    x <- rnorm(n)
    for(i in 1:5) {
      probs <- sort( runif(9))
      for(wtype in u) {
        wq <- wtd.quantile(x, type=wtype, weights=rep(1,length(x)), probs=probs)
        for(qtype in 1:9) {
          rq <- quantile(x, type=qtype, probs=probs)
          r[wtype, qtype] <- max(r[wtype,qtype], max(abs(wq-rq)))
        }
      }
    }
  }

r

# Restructure data to generate a dichotomous response variable
# from records containing numbers of events and numbers of trials
num   <- c(10,NA,20,0,15)   # data are 10/12 NA/999 20/20 0/25 15/35
denom <- c(12,999,20,25,35)
w     <- num.denom.setup(num, denom)
w
# attach(my.data.frame[w$subs,])
}
\keyword{nonparametric}
\keyword{category}
\keyword{distribution}
\keyword{robust}
\keyword{loess}
\keyword{smooth}
\keyword{manip}
\concept{weighted sampling}
\concept{grouping}
\concept{weights}
