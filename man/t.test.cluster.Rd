\name{t.test.cluster}
\alias{t.test.cluster}
\alias{print.t.test.cluster}
\title{t-test for Clustered Data}
\description{
Does a 2-sample t-test for clustered data.
}
\usage{
t.test.cluster(y, cluster, group, conf.int = 0.95)
\method{print}{t.test.cluster}(x, digits, \dots)
}
\arguments{
  \item{y}{normally distributed response variable to test}
  \item{cluster}{cluster identifiers, e.g. subject ID}
  \item{group}{grouping variable with two values}
  \item{conf.int}{confidence coefficient to use for confidence limits}
  \item{x}{an object created by \code{t.test.cluster}}
  \item{digits}{number of significant digits to print}
  \item{\dots}{unused}
}
\value{
  a matrix of statistics of class \code{t.test.cluster}
}
\references{
  Donner A, Birkett N, Buck C, Am J Epi 114:906-914, 1981.

  Donner A, Klar N, J Clin Epi 49:435-439, 1996.
  
  Hsieh FY, Stat in Med 8:1195-1201, 1988.
}
\author{Frank Harrell}
\seealso{\code{\link{t.test}}}
\examples{
set.seed(1)
y <- rnorm(800)
group <- sample(1:2, 800, TRUE)
cluster <- sample(1:40, 800, TRUE)
table(cluster,group)
t.test(y ~ group)   # R only
t.test.cluster(y, cluster, group)
# Note: negate estimates of differences from t.test to
# compare with t.test.cluster
}
\keyword{htest}

