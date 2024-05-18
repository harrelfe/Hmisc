\name{rcorr.cens}
\alias{rcorr.cens}
\alias{rcorrcens}
\alias{rcorrcens.formula}
\title{
  Rank Correlation for Censored Data
}
\description{
  Computes the c index and the corresponding
  generalization of Somers' Dxy rank correlation for a censored response
  variable. Also works for uncensored and binary responses, 
  although its use of all possible pairings
  makes it slow for this purpose.  Dxy and c are related by
  \eqn{Dxy=2(c-0.5)}{Dxy = 2*(c - 0.5)}.

  \code{rcorr.cens} handles one predictor variable.  \code{rcorrcens}
  computes rank correlation measures separately by a series of
  predictors.  In addition, \code{rcorrcens} has a rough way of handling
  categorical predictors.  If a categorical (factor) predictor has two
  levels, it is coverted to a numeric having values 1 and 2.  If it has
  more than 2 levels, an indicator variable is formed for the most
  frequently level vs. all others, and another indicator for the second
  most frequent level and all others.  The correlation is taken as the
  maximum of the two (in absolute value).
}
\usage{
rcorr.cens(x, S, outx=FALSE)

\method{rcorrcens}{formula}(formula, data=NULL, subset=NULL,
          na.action=na.retain, exclude.imputed=TRUE, outx=FALSE,
          \dots)
}
\arguments{
  \item{x}{
    a numeric predictor variable
  }
  \item{S}{
    an \code{Surv} object or a vector.  If a vector, assumes that every
    observation is uncensored.
  }
  \item{outx}{
    set to \code{TRUE} to not count pairs of observations tied on \code{x} as a
    relevant pair.  This results in a Goodman--Kruskal gamma type rank
    correlation.
  }
  \item{formula}{
    a formula with a \code{Surv} object or a numeric vector
    on the left-hand side
  }
  \item{data, subset, na.action}{
    the usual options for models.  Default for \code{na.action} is to retain
    all values, NA or not, so that NAs can be deleted in only a pairwise
    fashion.
  }
  \item{exclude.imputed}{
    set to \code{FALSE} to include imputed values (created by
    \code{impute}) in the calculations.
  }
  \item{\dots}{
    extra arguments passed to \code{\link{biVar}}.
  }
}
\value{
  \code{rcorr.cens} returns a vector with the following named elements:
  \code{C Index}, \code{Dxy}, \code{S.D.}, \code{n}, \code{missing},
  \code{uncensored}, \code{Relevant Pairs}, \code{Concordant}, and
  \code{Uncertain}

  \item{n}{number of observations not missing on any input variables}

  \item{missing}{number of observations missing on \code{x} or \code{S}}

  \item{relevant}{number of pairs of non-missing observations for which
  \code{S} could be ordered}

  \item{concordant}{number of relevant pairs for which \code{x} and \code{S}
  are concordant.}
  
  \item{uncertain}{number of pairs of non-missing observations for which
  censoring prevents classification of concordance of \code{x} and
  \code{S}.
  }

  \code{rcorrcens.formula} returns an object of class \code{biVar}
which is documented with the \code{\link{biVar}} function.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{fh@fharrell.com}
}
\seealso{
\code{\link[survival]{concordance}}, \code{\link{somers2}}, \code{\link{biVar}}, \code{\link{rcorrp.cens}}
}
\references{
Newson R: Confidence intervals for rank statistics: Somers' D and extensions.  Stata Journal 6:309-334; 2006.
}
\examples{
set.seed(1)
x <- round(rnorm(200))
y <- rnorm(200)
rcorr.cens(x, y, outx=TRUE)   # can correlate non-censored variables
library(survival)
age <- rnorm(400, 50, 10)
bp  <- rnorm(400,120, 15)
bp[1]  <- NA
d.time <- rexp(400)
cens   <- runif(400,.5,2)
death  <- d.time <= cens
d.time <- pmin(d.time, cens)
rcorr.cens(age, Surv(d.time, death))
r <- rcorrcens(Surv(d.time, death) ~ age + bp)
r
plot(r)

# Show typical 0.95 confidence limits for ROC areas for a sample size
# with 24 events and 62 non-events, for varying population ROC areas
# Repeat for 138 events and 102 non-events
set.seed(8)
par(mfrow=c(2,1))
for(i in 1:2) {
 n1 <- c(24,138)[i]
 n0 <- c(62,102)[i]
 y <- c(rep(0,n0), rep(1,n1))
 deltas <- seq(-3, 3, by=.25)
 C <- se <- deltas
 j <- 0
 for(d in deltas) {
  j <- j + 1
  x <- c(rnorm(n0, 0), rnorm(n1, d))
  w <- rcorr.cens(x, y)
  C[j]  <- w['C Index']
  se[j] <- w['S.D.']/2
 }
 low <- C-1.96*se; hi <- C+1.96*se
 print(cbind(C, low, hi))
 errbar(deltas, C, C+1.96*se, C-1.96*se,
        xlab='True Difference in Mean X',
        ylab='ROC Area and Approx. 0.95 CI')
 title(paste('n1=',n1,'  n0=',n0,sep=''))
 abline(h=.5, v=0, col='gray')
 true <- 1 - pnorm(0, deltas, sqrt(2))
 lines(deltas, true, col='blue')
}
par(mfrow=c(1,1))
}
\keyword{survival}
\keyword{nonparametric}
\concept{predictive accuracy}
\concept{logistic regression model}
% Converted by Sd2Rd version 1.21.
