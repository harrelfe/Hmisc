\name{rcorrp.cens}
\alias{rcorrp.cens}
\alias{improveProb}
\alias{print.improveProb}
\title{
  Rank Correlation for Paired Predictors with a Possibly Censored
  Response, and Integrated Discrimination Index
}
\description{
  Computes U-statistics to test for whether predictor X1 is more
  concordant than predictor X2, extending \code{rcorr.cens}.  For
  \code{method=1}, estimates the fraction of pairs for which the
  \code{x1} difference is more impressive than the \code{x2}
  difference. For \code{method=2}, estimates the fraction of pairs for
  which \code{x1} is concordant with \code{S} but \code{x2} is not.

  For binary responses the function \code{improveProb} provides several
  assessments of whether one set of predicted probabilities is better
  than another, using the methods describe in 
  \cite{Pencina et al (2007)}. This involves NRI and IDI to test for
  whether predictions from model \code{x1} are significantly different
  from those obtained from predictions from model \code{x2}. This is a
  distinct improvement over comparing ROC areas, sensitivity, or
  specificity.
}
\usage{
rcorrp.cens(x1, x2, S, outx=FALSE, method=1)

improveProb(x1, x2, y)

\method{print}{improveProb}(x, digits=3, conf.int=.95, \dots)
}
\arguments{
  \item{x1}{
    first predictor (a probability, for \code{improveProb})
  } 
  \item{x2}{
    second predictor (a probability, for \code{improveProb})
  }
  \item{S}{
    a possibly right-censored \code{\link[survival]{Surv}} object.  If
    \code{S} is a vector instead, it is converted to a
    \code{\link[survival]{Surv}} object and it is assumed that no
    observations are censored.
  }
  \item{outx}{
    set to \code{TRUE} to exclude pairs tied on \code{x1} or \code{x2}
    from consideration
  }
  \item{method}{
    see above
  }
  \item{y}{
    a binary 0/1 outcome variable
  }
  \item{x}{
    the result from \code{improveProb}
  }
  \item{digits}{
    number of significant digits for use in printing the result of
    \code{improveProb}
  } 
  \item{conf.int}{
    level for confidence limits
  }
  \item{\dots}{
    unused
  }
}
\details{
  If \code{x1},\code{x2} represent predictions from models, these
  functions assume either that you are using a separate sample from the
  one used to build the model, or that the amount of overfitting in
  \code{x1} equals the amount of overfitting in \code{x2}.  An example
  of the latter is giving both models equal opportunity to be complex so
  that both models have the same number of effective degrees of freedom,
  whether a predictor was included in the model or was screened out by a
  variable selection scheme.
  
  Note that in the first part of their paper, \cite{Pencina et al.}
  presented measures that required binning the predicted probabilities.
  Those measures were then replaced with better continuous measures that
  are implementedhere.
}
\value{
  a vector of statistics for \code{rcorrp.cens}, or a list with class
  \code{improveProb} of statistics for \code{improveProb}:
   \cr
  \item{n}{number of cases}
  \item{na}{number of events}
  \item{nb}{number of non-events}
  \item{pup.ev}{
    mean of pairwise differences in probabilities for those with events
    and a pairwise difference of \eqn{\mbox{probabilities}>0}
  }
  \item{pup.ne}{
    mean of pairwise differences in probabilities for those without
    events and a pairwise difference of \eqn{\mbox{probabilities}>0}
  } 
  \item{pdown.ev}{
    mean of pairwise differences in probabilities for those with events
    and a pairwise difference of \eqn{\mbox{probabilities}>0}
  } 
  \item{pdown.ne}{
    mean of pairwise differences in probabilities for those without
    events and a pairwise difference of \eqn{\mbox{probabilities}>0}
  }
  \item{nri}{
    Net Reclassification Index =
    \eqn{(pup.ev-pdown.ev)-(pup.ne-pdown.ne)}
  }
  \item{se.nri}{standard error of NRI}
  \item{z.nri}{Z score for NRI}
  \item{nri.ev}{Net Reclassification Index = \eqn{pup.ev-pdown.ev}}
  \item{se.nri.ev}{SE of NRI of events}
  \item{z.nri.ev}{Z score for NRI of events}
  \item{nri.ne}{Net Reclassification Index = \eqn{pup.ne-pdown.ne}}
  \item{se.nri.ne}{SE of NRI of non-events}
  \item{z.nri.ne}{Z score for NRI of non-events}
  \item{improveSens}{improvement in sensitivity}
  \item{improveSpec}{improvement in specificity}
  \item{idi}{Integrated Discrimination Index}
  \item{se.idi}{SE of IDI}
  \item{z.idi}{Z score of IDI}
}
\author{
  Frank Harrell  \cr
  Department of Biostatistics, Vanderbilt University  \cr
  \email{fh@fharrell.com}

  Scott Williams  \cr
  Division of Radiation Oncology  \cr
  Peter MacCallum Cancer Centre, Melbourne, Australia  \cr
  \email{scott.williams@petermac.org}
}
\references{
  Pencina MJ, D'Agostino Sr RB, D'Agostino Jr RB, Vasan RS (2008):
  Evaluating the added predictive ability of a new marker: From area
  under the ROC curve to reclassification and beyond.  Stat in Med 27:157-172.
  DOI: 10.1002/sim.2929
  
  Pencina MJ, D'Agostino Sr RB, D'Agostino Jr RB, Vasan RS:
  Rejoinder: Comments on Integrated discrimination and net reclassification
  improvements-Practical advice. Stat in Med 2007; DOI: 10.1002/sim.3106  

  Pencina MJ, D'Agostino RB, Steyerberg EW (2011): Extensions of net
  reclassification improvement calculations to measure usefulness of new
  biomarkers.  Stat in Med 30:11-21; DOI: 10.1002/sim.4085
}
\seealso{
  \code{\link{rcorr.cens}}, \code{\link{somers2}},
  \code{\link[survival]{Surv}}, \code{\link[rms]{val.prob}},
  \code{\link[survival]{concordance}}
}
\examples{
set.seed(1)
library(survival)

x1 <- rnorm(400)
x2 <- x1 + rnorm(400)
d.time <- rexp(400) + (x1 - min(x1))
cens   <- runif(400,.5,2)
death  <- d.time <= cens
d.time <- pmin(d.time, cens)
rcorrp.cens(x1, x2, Surv(d.time, death))
#rcorrp.cens(x1, x2, y) ## no censoring

set.seed(1)
x1 <- runif(1000)
x2 <- runif(1000)
y  <- sample(0:1, 1000, TRUE)
rcorrp.cens(x1, x2, y)
improveProb(x1, x2, y)
}
\keyword{survival}
\keyword{nonparametric}
\keyword{regression}
\concept{logistic regression model}
\concept{predictive accuracy}

