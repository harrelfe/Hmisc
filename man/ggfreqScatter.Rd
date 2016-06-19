\name{ggfreqScatter}
\alias{ggfreqScatter}
\title{Frequency Scatterplot}
\description{
Uses \code{ggplot2} to plot a scatterplot or dot-like chart for the case
where there is a very large number of overlapping values.  This works
for continuous and categorical \code{x} and \code{y}.  For continuous
variables it serves the same purpose as hexagonal binning.  Counts for
overlapping points are grouped into quantile groups and level of
transparency and rainbow colors are used to provide count information.
}
\usage{
ggfreqScatter(x, y, bins=50, g=10,
              xlab=as.character(substitute(x)),
              ylab=as.character(substitute(y)))
}
\arguments{
\item{x}{x-variable}
\item{y}{y-variable}
\item{bins}{for continuous \code{x} or \code{y} is the number of bins to
	create by rounding.  Ignored for categorical variables}
\item{g}{number of quantile groups to make for frequency counts}
\item{xlab}{x-axis label}
\item{ylab}{y-axis label}
}
\value{a \code{ggplot} object}
\author{Frank Harrell}
\seealso{\code{\link[Hmisc]{cut2}}}
\examples{
set.seed(1)
x <- rnorm(1000)
y <- rnorm(1000)
count <- sample(1:100, 1000, TRUE)
x <- rep(x, count)
y <- rep(y, count)
ggfreqScatter(x, y) + ggtitle("Using Deciles of Frequency Counts, 2500 Bins")

# Try with x categorical
x1 <- sample(c('cat', 'dog', 'giraffe'), length(x), TRUE)
ggfreqScatter(x1, y)

# Try with y categorical
y1 <- sample(LETTERS[1:10], length(x), TRUE)
ggfreqScatter(x, y1)

# Both categorical
ggfreqScatter(x1, y1)
}
\keyword{hplot}
\concept{grouping}
\concept{categorization}
\concept{discretization}
