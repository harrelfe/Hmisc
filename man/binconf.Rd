\name{binconf}
\alias{binconf}
\title{
Confidence Intervals for Binomial Probabilities
}
\description{
Produces 1-alpha confidence intervals for binomial probabilities.
}
\usage{
binconf(x, n, alpha=0.05,
        method=c("wilson","exact","asymptotic","all"),
        include.x=FALSE, include.n=FALSE, return.df=FALSE)
}
\arguments{
\item{x}{
vector containing the number of "successes" for binomial variates
}
\item{n}{
vector containing the numbers of corresponding observations 
}
\item{alpha}{
probability of a type I error, so confidence coefficient = 1-alpha
}
\item{method}{
character string specifing which method to use.  The "all" method only
works when 
x and n are length 1.  The "exact" method uses the F distribution
to compute exact (based on the binomial cdf) intervals; the
"wilson" interval is score-test-based; and the "asymptotic" is the
text-book, asymptotic normal interval.  Following Agresti and
Coull, the Wilson interval is to be preferred and so is the
default.
}
\item{include.x}{
logical flag to indicate whether \code{x} should be included in the
returned matrix or data frame 
}
\item{include.n}{
logical flag to indicate whether \code{n} should be included in the
returned matrix or data frame 
}
\item{return.df}{
logical flag to indicate that a data frame rather than a matrix be
returned
}}
\value{
a matrix or data.frame containing the computed intervals and,
optionally, \code{x} and \code{n}.  
}
\author{
Rollin Brant, Modified by Frank Harrell and
\cr
Brad Biggerstaff
\cr
Centers for Disease Control and Prevention
\cr
National Center for Infectious Diseases
\cr
Division of Vector-Borne Infectious Diseases
\cr
P.O. Box 2087, Fort Collins, CO, 80522-2087, USA
\cr
\email{bkb5@cdc.gov}
}
\references{
A. Agresti and B.A. Coull, Approximate is better than "exact" for
interval estimation of binomial proportions,  
\emph{American Statistician,}
\bold{52}:119--126, 1998.


R.G. Newcombe, Logit confidence intervals and the inverse sinh
transformation,
\emph{American Statistician,}
\bold{55}:200--202, 2001.


L.D. Brown, T.T. Cai and A. DasGupta, Interval estimation for
a binomial proportion (with discussion),
\emph{Statistical Science,}
\bold{16}:101--133, 2001.
}
\examples{
binconf(0:10,10,include.x=TRUE,include.n=TRUE)
binconf(46,50,method="all")
}
\keyword{category}
\keyword{htest}
% Converted by Sd2Rd version 1.21.
