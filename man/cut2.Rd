\name{cut2}
\alias{cut2}
\alias{cutGn}
\title{Cut a Numeric Variable into Intervals}
\description{
\code{cut2} is a function like \code{cut} but left endpoints are inclusive and labels are of
the form \code{[lower, upper)}, except that last interval is \code{[lower,upper]}.  
If cuts are given, will by default make sure that cuts include entire
range of \code{x}.
Also, if cuts are not given, will cut \code{x} into quantile groups 
(\code{g} given) or groups
with a given minimum number of observations (\code{m}).  Whereas cut creates a
category object, \code{cut2} creates a factor object.  \code{m} is not guaranteed but is a target.

\code{cutGn} guarantees that the grouped variable will have a minimum of \code{m} observations in any group.  This is done by an exhaustive algorithm that runs fast due to being coded in Fortran.
}
\usage{
cut2(x, cuts, m=150, g, levels.mean=FALSE, digits, minmax=TRUE,
oneval=TRUE, onlycuts=FALSE, formatfun=format, \dots)

cutGn(x, m, what=c('mean', 'factor', 'summary', 'cuts', 'function'), rcode=FALSE) 
}
\arguments{
\item{x}{
numeric vector to classify into intervals
}
\item{cuts}{
cut points
}
\item{m}{
desired minimum number of observations in a group.  The algorithm does
not guarantee that all groups will have at least \code{m} observations.
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
}
\item{onlycuts}{
  set to \code{TRUE} to only return the vector of computed cuts.  This
  consists of the interior values plus outer ranges.
}
\item{formatfun}{
  formatting function, supports formula notation (if \code{rlang} is installed)
}
\item{\dots}{
  additional arguments passed to \code{formatfun}
}
\item{what}{specifies the kind of vector values to return from \code{cutGn}, the default being like \code{'levels.mean'} of \code{cut2}.  Specify \code{'summary'} to return a numeric 3-column matrix that summarizes the intervals satisfying the \code{m} requirement. Use \code{what='cuts'} to only return the vector of computed cutpoints. To create a function that will recode the variable in play using the same intervals as computed by \code{cutGn}, specify \code{what='function'}.  This function will have a \code{what} argument to allow the user to decide later whether to recode into interval means or into a \code{factor} variable.}
\item{rcode}{set to \code{TRUE} to run the \code{cutgn} algorithm in R.  This is useful for speed comparisons with the default compiled code.}
}
\value{
a factor variable with levels of the form \code{[a,b)} or formatted means
(character strings) unless \code{onlycuts} is \code{TRUE} in which case
a numeric vector is returned
}
\seealso{
\code{\link{cut}}, \code{\link{quantile}}, \code{\link{combine.levels}}
}
\examples{
set.seed(1)
x <- runif(1000, 0, 100)
z <- cut2(x, c(10,20,30))
table(z)
table(cut2(x, g=10))      # quantile groups
table(cut2(x, m=50))      # group x into intevals with at least 50 obs.

table(cutGn(x, m=50, what='factor'))
f <- cutGn(x, m=50, what='function')
f
f(c(-1, 2, 10), what='mean')
f(c(-1, 2, 10), what='factor')
\dontrun{
  x <- round(runif(200000), 3)
  system.time(a <- cutGn(x, m=20))              # 0.02s
  system.time(b <- cutGn(x, m=20, rcode=TRUE))  # 1.51s
  identical(a, b)
}
}
\keyword{category}
\keyword{nonparametric}
\concept{grouping}
\concept{categorization}
\concept{discretization}
