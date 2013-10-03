\name{hist.data.frame}
\alias{hist.data.frame}
\title{Histograms for Variables in a Data Frame}
\description{
This functions tries to compute the maximum number of histograms that
will fit on one page, then it draws a matrix of histograms.  If there
are more qualifying variables than will fit on a page, the function
waits for a mouse click before drawing the next page.
}
\usage{
\method{hist}{data.frame}(x, n.unique = 3, nclass = "compute",
                na.big = FALSE, rugs = FALSE, freq=TRUE, mtitl = FALSE, ...)
}
\arguments{
  \item{x}{a data frame}
  \item{n.unique}{minimum number of unique values a variable must have
	before a histogram is drawn}
  \item{nclass}{number of bins.  Default is
	max(2,trunc(min(n/10,25*log(n,10))/2)), where n is the number of
	non-missing values for a variable.}
  \item{na.big}{set to \code{TRUE} to draw the number of missing values
	on the top of the histogram in addition to in a subtitle.  In the
	subtitle, n is the number of non-missing values and m is the number
	of missing values}
  \item{rugs}{set to \code{TRUE} to add rug plots at the top of each
		histogram}
	\item{freq}{see \code{\link{hist}}.  Default is to show frequencies.}
  \item{mtitl}{set to a character string to set aside extra outside top
	margin and to use the string for an overall title}
  \item{\dots}{arguments passed to \code{scat1d}}
}
\value{the number of pages drawn}
\author{Frank E Harrell Jr}
\seealso{\code{\link{hist}}, \code{\link{scat1d}}}
\examples{
d <- data.frame(a=runif(200), b=rnorm(200),
                w=factor(sample(c('green','red','blue'), 200, TRUE)))
hist.data.frame(d)   # in R, just say hist(d)
}
\keyword{hplot}
\keyword{dplot}
\keyword{distribution}
