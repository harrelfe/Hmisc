\name{cut2}
\alias{cut2}
\title{
Cut a Numeric Variable into Intervals
}
\description{
Function like cut but left endpoints are inclusive and labels are of
the form \code{[lower, upper)}, except that last interval is \code{[lower,upper]}.  
If cuts are given, will by default make sure that cuts include entire
range of \code{x}.
Also, if cuts are not given, will cut \code{x} into quantile groups 
(\code{g} given) or groups
with a given minimum number of observations (\code{m}).  Whereas cut creates a
category object, \code{cut2} creates a factor object.
}
\usage{
cut2(x, cuts, m, g, levels.mean, digits, minmax=TRUE, oneval=TRUE)
}
\arguments{
\item{x}{
numeric vector to classify into intervals
}
\item{cuts}{
cut points
}
\item{m}{
desired minimum number of observations in a group
}
\item{g}{
number of quantile groups
}
\item{levels.mean}{
set to \code{TRUE} to make the new categorical vector have levels attribute that is
the group means of \code{x} instead of interval endpoint labels
}
\item{digits}{
number of significant digits to use in constructing levels.  Default is 3
(5 if \code{levels.mean=TRUE})
}
\item{minmax}{
if cuts is specified but \code{min(x)<min(cuts)} or \code{max(x)>max(cuts)}, augments
cuts to include min and max \code{x}
}
\item{oneval}{
if an interval contains only one unique value, the interval will be
labeled with the formatted version of that value instead of the
interval endpoints, unless \code{oneval=FALSE}
}}
\value{
a factor variable with levels of the form \code{[a,b)} or formatted means
(character strings)
}
\seealso{
\code{\link{cut}}, \code{\link{quantile}}
}
\examples{
set.seed(1)
x <- runif(1000, 0, 100)
z <- cut2(x, c(10,20,30))
table(z)
table(cut2(x, g=10))      # quantile groups
table(cut2(x, m=50))      # group x into intevals with at least 50 obs.
}
\keyword{category}
\keyword{nonparametric}
\concept{grouping}
\concept{categorization}
\concept{discretization}
