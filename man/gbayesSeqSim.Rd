% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gbayesSeqSim.r
\name{gbayesSeqSim}
\alias{gbayesSeqSim}
\title{gbayesSeqSim}
\usage{
gbayesSeqSim(est, asserts)
}
\arguments{
\item{est}{data frame created by \code{estSeqSim()}}

\item{asserts}{list of lists.  The first element of each list is the user-specified name for each assertion/prior combination, e.g., \code{"efficacy"}.  The other elements are, in order, a character string equal to "<", ">", or "in", a parameter value \code{cutoff} (for "<" and ">") or a 2-vector specifying an interval for "in", and either a prior distribution mean and standard deviation named \code{mu} and \code{sigma} respectively, or a parameter value (\code{"cutprior"}) and tail area \code{"tailprob"}.  If the latter is used, \code{mu} is assumed to be zero and \code{sigma} is solved for such that P(parameter > 'cutprior') = P(parameter < - 'cutprior') = \code{tailprob}.}
}
\value{
a data frame with number of rows equal to that of \code{est} with a number of new columns equal to the number of assertions added.  The new columns are named \code{p1}, \code{p2}, \code{p3}, ... (posterior probabilities), \code{mean1}, \code{mean2}, ... (posterior means), and \code{sd1}, \code{sd2}, ... (posterior standard deviations).  The returned data frame also has an attribute \code{asserts} added which is the original \code{asserts} augmented with any derived \code{mu} and \code{sigma} and converted to a data frame, and another attribute \code{alabels} which is a named vector used to map \code{p1}, \code{p2}, ... to the user-provided labels in \code{asserts}.
}
\description{
Simulate Bayesian Sequential Treatment Comparisons Using a Gaussian Model
}
\details{
Simulate a sequential trial under a Gaussian model for parameter estimates, and Gaussian priors using simulated estimates and variances returned by \code{estSeqSim}.  For each row of the data frame \code{est} and for each prior/assertion combination, computes the posterior probability of the assertion.
}
\examples{
\dontrun{
# Simulate Bayesian operating characteristics for an unadjusted
# proportional odds comparison (Wilcoxon test)
# For 100 simulations, 5 looks, 2 true parameter values, and
# 2 assertion/prior combinations, compute the posterior probability
# Use a low-level logistic regression call to speed up simuluations
# Use data.table to compute various summary measures
# Total simulation time: 2s
lfit <- function(x, y) {
f <- rms::lrm.fit(x, y)
  k <- length(coef(f))
  c(coef(f)[k], vcov(f)[k, k])
}
gdat <- function(beta, n1, n2) {
  # Cell probabilities for a 7-category ordinal outcome for the control group
  p <- c(2, 1, 2, 7, 8, 38, 42) / 100

  # Compute cell probabilities for the treated group
  p2 <- pomodm(p=p, odds.ratio=exp(beta))
  y1 <- sample(1 : 7, n1, p,  replace=TRUE)
  y2 <- sample(1 : 7, n2, p2, replace=TRUE)
  list(y1=y1, y2=y2)
}

# Assertion 1: log(OR) < 0 under prior with prior mean 0.1 and sigma 1 on log OR scale
# Assertion 2: OR between 0.9 and 1/0.9 with prior mean 0 and sigma computed so that
# P(OR > 2) = 0.05
asserts <- list(list('Efficacy', '<', 0, mu=0.1, sigma=1),
                list('Similarity', 'in', log(c(0.9, 1/0.9)),
                     cutprior=log(2), tailprob=0.05))

set.seed(1)
est <- estSeqSim(c(0, log(0.7)), looks=c(50, 75, 95, 100, 200),
                   gendat=gdat,
                   fitter=lfit, nsim=100)
z <- gbayesSeqSim(est, asserts)
head(z)
attr(z, 'asserts')

# Compute the proportion of simulations that hit targets (different target posterior
# probabilities for efficacy vs. similarity)

# For the efficacy assessment compute the first look at which the target
# was hit (set to infinity if never hit)
require(data.table)
z <- data.table(z)
u <- z[, .(first=min(p1 > 0.95)), by=.(parameter, sim)]
# Compute the proportion of simulations that ever hit the target and
# that hit it by the 100th subject
u[, .(ever=mean(first < Inf)),  by=.(parameter)]
u[, .(by75=mean(first <= 100)), by=.(parameter)]
}
}
\seealso{
\code{gbayes()}, \code{estSeqSim()}, \code{simMarkovOrd()}, \code{estSeqMarkovOrd()}
}
\author{
Frank Harrell
}
