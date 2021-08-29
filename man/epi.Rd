\name{mhgr}
\alias{mhgr}
\alias{print.mhgr}
\alias{lrcum}
\alias{print.lrcum}
\title{Miscellaneous Functions for Epidemiology}
\description{
  The \code{mhgr} function computes the Cochran-Mantel-Haenszel stratified
  risk ratio and its confidence limits using the Greenland-Robins variance
  estimator.

The \code{lrcum} function takes the results of a series of 2x2 tables
representing the relationship between test positivity and diagnosis and
computes positive and negative likelihood ratios (with all their
deficiencies) and the variance of
their logarithms.  Cumulative likelihood ratios and their confidence
intervals (assuming independence of tests) are computed, assuming a
string of all positive tests or a string of all negative tests.  The
method of Simel et al as described in Altman et al is used.
}
\usage{
mhgr(y, group, strata, conf.int = 0.95)
\method{print}{mhgr}(x, \dots)

lrcum(a, b, c, d, conf.int = 0.95)
\method{print}{lrcum}(x, dec=3, \dots)
}
\arguments{
  \item{y}{a binary response variable}
  \item{group}{a variable with two unique values specifying comparison groups}
  \item{strata}{the stratification variable}
  \item{conf.int}{confidence level}
  \item{x}{an object created by \code{mhgr} or \code{lrcum}}
  \item{a}{frequency of true positive tests}
  \item{b}{frequency of false positive tests}
  \item{c}{frequency of false negative tests}
  \item{d}{frequency of true negative tests}
  \item{dec}{number of places to the right of the decimal to print for
    \code{lrcum}}
  \item{\dots}{addtitional arguments to be passed to other print functions}
}
\details{
  Uses equations 4 and 13 from Greenland and Robins.
}
\value{
  a list of class \code{"mhgr"} or of class \code{"lrcum"}.
}
\references{
  Greenland S, Robins JM (1985): Estimation of a common effect parameter
  from sparse follow-up data.  Biometrics 41:55-68.

  Altman DG, Machin D, Bryant TN, Gardner MJ, Eds. (2000): Statistics with
  Confidence, 2nd Ed.  Bristol: BMJ Books, 105-110.

  Simel DL, Samsa GP, Matchar DB (1991): Likelihood ratios with
  confidence: sample size estimation for diagnostic test studies.  J
  Clin Epi 44:763-770.
}
\author{Frank E Harrell Jr \email{fh@fharrell.com}}
\seealso{\code{\link{logrank}}}
\examples{
# Greate Migraine dataset used in Example 28.6 in the SAS PROC FREQ guide
d <- expand.grid(response=c('Better','Same'),
                 treatment=c('Active','Placebo'),
                 sex=c('female','male'))
d$count <- c(16, 11, 5, 20, 12, 16, 7, 19)
d
# Expand data frame to represent raw data
r <- rep(1:8, d$count)
d <- d[r,]
with(d, mhgr(response=='Better', treatment, sex))

# Discrete survival time example, to get Cox-Mantel relative risk and CL
# From Stokes ME, Davis CS, Koch GG, Categorical Data Analysis Using the
# SAS System, 2nd Edition, Sectino 17.3, p. 596-599
#
# Input data in Table 17.5
d <- expand.grid(treatment=c('A','P'), center=1:3)
d$healed2w    <- c(15,15,17,12, 7, 3)
d$healed4w    <- c(17,17,17,13,17,17)
d$notHealed4w <- c( 2, 7,10,15,16,18)
d
# Reformat to the way most people would collect raw data
d1 <- d[rep(1:6, d$healed2w),]
d1$time <- '2'
d1$y <- 1
d2 <- d[rep(1:6, d$healed4w),]
d2$time <- '4'
d2$y <- 1
d3 <- d[rep(1:6, d$notHealed4w),]
d3$time <- '4'
d3$y <- 0
d <- rbind(d1, d2, d3)
d$healed2w <- d$healed4w <- d$notHealed4w <- NULL
d
# Finally, duplicate appropriate observations to create 2 and 4-week
# risk sets.  Healed and not healed at 4w need to be in the 2-week
# risk set as not healed
d2w      <- subset(d, time=='4')
d2w$time <- '2'
d2w$y    <- 0
d24      <- rbind(d, d2w)
with(d24, table(y, treatment, time, center))
# Matches Table 17.6

with(d24, mhgr(y, treatment, interaction(center, time, sep=';')))

# Get cumulative likelihood ratios and their 0.95 confidence intervals
# based on the following two tables
#
#          Disease       Disease
#          +     -       +     -
# Test +   39    3       20    5
# Test -   21   17       22   15

lrcum(c(39,20), c(3,5), c(21,22), c(17,15))
}
\keyword{category}
\keyword{htest}
\concept{epidemiology}
