\name{rcorr.cens}
\alias{rcorr.cens}
\title{
Rank Correlation for Censored Data
}
\description{
Computes the \code{c} index and the corresponding
generalization of Somers' Dxy rank correlation for a censored response
variable. Also works for uncensored and binary responses, 
although its use of all possible pairings
makes it slow for this purpose.
}
\usage{
rcorr.cens(x, S, outx=FALSE)
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
}}
\value{
 a vector with the following named elements:
\code{C Index, Dxy, S.D., n, missing, uncensored, Relevant Pairs, Concordant},
\code{Uncertain}
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
f.harrell@vanderbilt.edu
}
\seealso{
somers2
}
\examples{
set.seed(1)
x <- round(rnorm(200))
y <- rnorm(200)
rcorr.cens(x, y, outx=TRUE)   # can correlate non-censored variables
if(.R.) library(survival)
age <- rnorm(400, 50, 10)
d.time <- rexp(400)
cens   <- runif(400,.5,2)
death  <- d.time <= cens
d.time <- pmin(d.time, cens)
rcorr.cens(age, Surv(d.time, death))
}
\keyword{survival}
% Converted by Sd2Rd version 1.21.
