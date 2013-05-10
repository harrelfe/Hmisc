\name{format.pval}
\alias{format.pval}
\title{Format P Values}
\description{
  \code{format.pval} is intended for formatting p-values.
}
\usage{
format.pval(x, pv=x, digits = max(1, .Options$digits - 2),
            eps = .Machine$double.eps, na.form = "NA", \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pv}{a numeric vector.}
  \item{x}{argument for method compliance.}
  \item{digits}{how many significant digits are to be used.}
  \item{eps}{a numerical tolerance: see Details.}
  \item{na.form}{character representation of \code{NA}s.}
  \item{\dots}{
    arguments passed to \code{\link{format}} in the \code{format.pval}
    function body.
  }
}
\details{
  \code{format.pval} is mainly an auxiliary function for
  \code{\link{print.summary.lm}} etc., and does separate formatting for
  fixed, floating point and very small values; those less than
  \code{eps} are formatted as \dQuote{\samp{< [eps]}} (where
  \dQuote{\samp{[eps]}} stands for \code{format(eps, digits)}).
}
\value{
  A character vector.
}
\note{This is the base \code{\link[base]{format.pval}} function with the
  ablitiy to pass the \code{nsmall} argument to \code{\link{format}}
}
\examples{
format.pval(c(runif(5), pi^-100, NA))
format.pval(c(0.1, 0.0001, 1e-27))
format.pval(c(0.1, 1e-27), nsmall=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
