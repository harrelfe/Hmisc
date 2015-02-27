\name{colorFacet}
\alias{colorFacet}
\title{Miscellaneous ggplot2 Helper Functions}
\description{
These functions are used on \code{ggplot2} objects or as layers when
building a \code{ggplot2} object.  \code{colorFacet} colors the thin
rectangles used to separate panels created by \code{facet_grid} (and
probably by \code{facet_wrap}).  This is used by \code{ggplot.summaryP}.
}
\usage{
colorFacet(g, col = adjustcolor("blue", alpha.f = 0.3))
}
\arguments{
  \item{g}{a \code{ggplot2} object that used faceting}
	\item{col}{color for facet separator rectanges}
}
\value{
}
\author{Sandy Muspratt}
}
\examples{
\dontrun{
s <- summaryP(age + sex ~ region + treatment)
colorFacet(ggplot(s))   # prints directly
}
}
\keyword{hplot}
