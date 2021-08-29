\name{bootkm}
\alias{bootkm}
\title{
  Bootstrap Kaplan-Meier Estimates
}
\description{
  Bootstraps Kaplan-Meier estimate of the probability of survival to at
  least a fixed time (\code{times} variable) or the estimate of the \code{q}
  quantile of the survival distribution (e.g., median survival time, the
  default).
}
\usage{
bootkm(S, q=0.5, B=500, times, pr=TRUE)
}
\arguments{
  \item{S}{
    a \code{Surv} object for possibly right-censored survival time
  }
  \item{q}{
    quantile of survival time, default is 0.5 for median
  }
  \item{B}{
    number of bootstrap repetitions (default=500)
  }
  \item{times}{
    time vector (currently only a scalar is allowed) at which to compute
    survival estimates.  You may specify only one of \code{q} and
    \code{times}, and if \code{times} is specified \code{q} is ignored. 
  }
  \item{pr}{
    set to \code{FALSE} to suppress printing the iteration number every
    10 iterations
  }
}
\value{
  a vector containing \code{B} bootstrap estimates
}
\section{Side Effects}{
  updates \code{.Random.seed}, and, if \code{pr=TRUE}, prints progress
  of simulations
}
\details{
  \code{bootkm} uses Therneau's \code{survfitKM} function to efficiently
  compute Kaplan-Meier estimates.
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
\references{
  Akritas MG (1986): Bootstrapping the Kaplan-Meier estimator.  JASA
  81:1032--1038.
}
\seealso{
  \code{\link[survival]{survfit}}, \code{\link[survival]{Surv}},
  \code{\link[rms]{Survival.cph}}, \code{\link[rms]{Quantile.cph}}
}
\examples{
# Compute 0.95 nonparametric confidence interval for the difference in
# median survival time between females and males (two-sample problem)
set.seed(1)
library(survival)
S <- Surv(runif(200))      # no censoring
sex <- c(rep('female',100),rep('male',100))
med.female <- bootkm(S[sex=='female',], B=100) # normally B=500
med.male   <- bootkm(S[sex=='male',],   B=100)
describe(med.female-med.male)
quantile(med.female-med.male, c(.025,.975), na.rm=TRUE)
# na.rm needed because some bootstrap estimates of median survival
# time may be missing when a bootstrap sample did not include the
# longer survival times
}
\keyword{survival}
\keyword{nonparametric}
\concept{bootstrap}
