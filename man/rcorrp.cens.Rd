\name{rcorrp.cens}
\alias{rcorrp.cens}
\alias{improveProb}
\alias{print.improveProb}
\title{
Rank Correlation for Paired Predictors with a Possibly Censored Response
}
\description{
  Computes U-statistics to test for whether predictor X1 is more concordant
  than predictor X2, extending rcorr.cens.  For method=1, estimates the fraction of
  pairs for which the x1 difference is more impressive than the x2 difference.
  For method=2, estimates the fraction of pairs for which x1 is concordant with
  S but x2 is not.

  For binary responses the function \code{improveProb} provides several
  assessments of whether one set of predicted probabilities is better
  than another, using the methods describe in Pencina et al (2007).
  This is a distinct improvement over comparing ROC areas, sensitivity,
  or specificity.
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
    a possibly right-censored \code{\link[survival]{Surv}} object.  If \code{S} is a vector instead,
    it is converted to a \code{\link[survival]{Surv}} object and it is assumed that no observations
    are censored.
  }
  \item{outx}{
    set to T to exclude pairs tied on x1 or x2 from consideration
  }
  \item{method}{
    see above
  }
  \item{y}{a binary 0/1 outcome variable}
  \item{x}{the result from \code{improveProb}}
 \item{digits}{number of significant digits for use in printing the
result of \code{improveProb}}
 \item{conf.int}{level for confidence limits}
 \item{\dots}{unused}
}
\details{
  If \code{x1,x2} represent predictions from models, these functions
  assume either that you are using a separate sample from the one used
  to build the model, or that the amount of overfitting in \code{x1}
  equals the amount of overfitting in \code{x2}.  An example of the
  latter is giving both models equal opportunity to be complex so that
  both models have the same number of effective degrees of freedom,
  whether a predictor was included in the model or was screened out by a
  variable selection scheme.
  }
\value{
  a vector of statistics (a list of statistics for \code{improveProb})
}
\author{
  Frank Harrell
  \cr
  Department of Biostatistics, Vanderbilt University
  \cr
  \email{f.harrell@vanderbilt.edu}
}
\references{
  Pencina MJ, D'Agostino Sr RB, D'Agostino Jr RB, Vasan RS (2007):
  Evaluating the added predictive ability of a new marker: From area
  under the ROC curve to reclassification and beyond.  Stat in Med 26.
}
\seealso{
\code{\link[Hmisc]{rcorr.cens}}, \code{\link[Hmisc]{somers2}},
\code{\link[survival]{Surv}}, \code{\link[Design]{val.prob}}
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

