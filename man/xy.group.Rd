\name{xy.group}
\alias{xy.group}
\title{
Mean x vs. function of y in groups of x
}
\description{
Compute mean x vs. a function of y (e.g. median) by quantile
groups of x or by x grouped to have a given minimum number of
observations.  Deletes NAs in x and y before doing computations.
}
\usage{
xy.group(x, y, m=150, g, fun=mean, result="list")
}
\arguments{
\item{x}{
a vector, may contain NAs
}
\item{y}{
a vector of same length as x, may contain NAs
}
\item{m}{
number of observations per group
}
\item{g}{
number of quantile groups
}
\item{fun}{
function of y such as median or mean (the default)
}
\item{result}{
"list" (the default), or "matrix"
}}
\value{
if result="list", a list with components x and y suitable for plotting.
if result="matrix", matrix with rows corresponding to x-groups and columns named
n, x, and y.
}
\seealso{
\code{\link{cut2}}, \code{\link{cutGn}}, \code{\link{tapply}}
}
\examples{
  \dontrun{
plot(xy.group(x, y, g=10))	#Plot mean y by deciles of x
xy.group(x, y, m=100, result="matrix")	#Print table, 100 obs/group
  }
}
\keyword{category}
\keyword{nonparametric}
\concept{grouping}
\concept{stratification}
\concept{aggregation}
\concept{discretization}

