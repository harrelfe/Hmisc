\name{rcorrp.cens}
\alias{rcorrp.cens}
\title{
Rank Correlation for Paired Predictors with a Censored Response
}
\description{
Computes U-statistics to test for whether predictor X1 is more concordant
than predictor X2, extending rcorr.cens.  For method=1, estimates the fraction of
pairs for which the x1 difference is more impressive than the x2 difference.
For method=2, estimates the fraction of pairs for which x1 is concordant with
S but x2 is not.
}
\usage{
rcorrp.cens(x1, x2, S, outx=FALSE, method=1)
}
\arguments{
\item{x1}{
first predictor
}
\item{x2}{
second predictor
}
\item{S}{
a possibly right-censored \code{Surv} object.  If \code{S} is a vector instead,
it is converted to a \code{Surv} object and it is assumed that no observations
are censored.
}
\item{outx}{
set to T to exclude pairs tied on x1 or x2 from consideration
}
\item{method}{
see above
}}
\value{
a vector of statistics
}
\author{
Frank Harrell
\cr
Department of Biostatistics, Vanderbilt University
\cr
f.harrell@vanderbilt.edu
}
\seealso{
\code{\link{rcorr.cens}}, \code{\link{somers2}}, \code{\link{Surv}}
}
\examples{
set.seed(1)
if(.R.) library(survival)
x1 <- rnorm(400)
x2 <- x1 + rnorm(400)
d.time <- rexp(400) + (x1 - min(x1))
cens   <- runif(400,.5,2)
death  <- d.time <= cens
d.time <- pmin(d.time, cens)
rcorrp.cens(x1, x2, Surv(d.time, death))
# rcorrp.cens(x1, x2, y) ## no censoring
}
\keyword{survival}
\keyword{nonparametric}
% Converted by Sd2Rd version 1.21.
