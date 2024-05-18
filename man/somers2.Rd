\name{somers2}
\alias{somers2}
\title{
Somers' Dxy Rank Correlation
}
\description{
Computes Somers' Dxy rank correlation between a variable \code{x} and a
binary (0-1) variable \code{y}, and the corresponding receiver operating
characteristic curve area \code{c}. Note that \code{Dxy = 2(c-0.5)}.  
\code{somers} allows for a \code{weights} variable, which specifies frequencies
to associate with each observation.
}
\usage{
somers2(x, y, weights=NULL, normwt=FALSE, na.rm=TRUE)
}
\arguments{
\item{x}{
typically a predictor variable. \code{NA}s are allowed.
}
\item{y}{
a numeric outcome variable coded \code{0-1}. \code{NA}s are allowed.
}
\item{weights}{
a numeric vector of observation weights (usually frequencies).  Omit
or specify a zero-length vector to do an unweighted analysis.
}
\item{normwt}{
set to \code{TRUE} to make \code{weights} sum to the actual number of non-missing
observations.
}
\item{na.rm}{
set to \code{FALSE} to suppress checking for NAs.
}}
\value{
a vector with the named elements \code{C}, \code{Dxy}, \code{n} (number of non-missing
pairs), and \code{Missing}. Uses the formula 
\code{C = (mean(rank(x)[y == 1]) - (n1 + 1)/2)/(n - n1)}, where \code{n1} is the
frequency of \code{y=1}.
}
\details{
The \code{rcorr.cens} function, which although slower than \code{somers2} for large
sample sizes, can also be used to obtain Dxy for non-censored binary
\code{y}, and it has the advantage of computing the standard deviation of
the correlation index.
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
\seealso{
\code{\link[survival]{concordance}}, \code{\link{rcorr.cens}}, \code{\link{rank}}, \code{\link{wtd.rank}}, 
}
\examples{
set.seed(1)
predicted <- runif(200)
dead      <- sample(0:1, 200, TRUE)
roc.area <- somers2(predicted, dead)["C"]

# Check weights
x <- 1:6
y <- c(0,0,1,0,1,1)
f <- c(3,2,2,3,2,1)
somers2(x, y)
somers2(rep(x, f), rep(y, f))
somers2(x, y, f)
}
\keyword{nonparametric}
\concept{logistic regression model}
\concept{predictive accuracy}



