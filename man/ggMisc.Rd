\name{colorFacet}
\alias{colorFacet}
\alias{arrGrob}
\alias{print.arrGrob}
\title{Miscellaneous ggplot2 and grid Helper Functions}
\description{
These functions are used on \code{ggplot2} objects or as layers when
building a \code{ggplot2} object, and to facilitate use of
\code{gridExtra}.  \code{colorFacet} colors the thin 
rectangles used to separate panels created by \code{facet_grid} (and
probably by \code{facet_wrap}). 
A better approach may be found at \url{https://stackoverflow.com/questions/28652284/}.
\code{arrGrob} is a front-end to \code{gridExtra::arrangeGrob} that
allows for proper printing.  See
\url{https://stackoverflow.com/questions/29062766/store-output-from-gridextragrid-arrange-into-an-object/}.  The \code{arrGrob} \code{print} method calls \code{grid::grid.draw}.
}
\usage{
colorFacet(g, col = adjustcolor("blue", alpha.f = 0.3))

arrGrob(\dots)

\method{print}{arrGrob}(x, \dots)
}
\arguments{
  \item{g}{a \code{ggplot2} object that used faceting}
	\item{col}{color for facet separator rectanges}
	\item{\dots}{passed to \code{arrangeGrob}}
	\item{x}{an object created by \code{arrGrob}}
}
\author{Sandy Muspratt}
\examples{
\dontrun{
require(ggplot2)
s <- summaryP(age + sex ~ region + treatment)
colorFacet(ggplot(s))   # prints directly
# arrGrob is called by rms::ggplot.Predict and others
}
}
\keyword{hplot}
