\name{symbol.freq}
\alias{symbol.freq}
\title{Graphic Representation of a Frequency Table}
\description{
This function can be used to represent
contingency tables graphically.  Frequency counts are represented as
the heights of "thermometers" by default; you can also specify
\code{symbol='circle'} to the function.  There is an option to include
marginal frequencies, which are plotted on a halved scale so as to not
overwhelm the plot.   If you do not ask for marginal frequencies to be
plotted using \code{marginals=T}, \code{symbol.freq} will ask you to click
the mouse where a reference symbol is to be drawn to assist in reading
the scale of the frequencies.

\code{label} attributes, if present, are used for x- and y-axis labels.
Otherwise, names of calling arguments are used.
}
\usage{
symbol.freq(x, y, symbol = c("thermometer", "circle"),
            marginals = FALSE, orig.scale = FALSE,
            inches = 0.25, width = 0.15, subset, srtx = 0, ...)
}
\arguments{
  \item{x}{first variable to cross-classify}
  \item{y}{second variable}
  \item{symbol}{specify \code{"thermometer"} (the default) or \code{"circle"}}
  \item{marginals}{set to \code{TRUE} to add marginal frequencies
	(scaled by half) to the plot}
  \item{orig.scale}{set to \code{TRUE} when the first two arguments are
	numeric variables; this uses their original values for x and y
	coordinates)} 
  \item{inches}{see \code{\link{symbols}}}
  \item{width}{see \code{thermometers} option in \code{symbols}}
  \item{subset}{the usual subsetting vector}
  \item{srtx}{rotation angle for x-axis labels}
  \item{\dots}{other arguments to pass to \code{symbols}}
}
\author{Frank Harrell}
\seealso{\code{\link{symbols}}}
\examples{
\dontrun{
getHdata(titanic)
attach(titanic)
age.tertile <- cut2(titanic$age, g=3)
symbol.freq(age.tertile, pclass, marginals=T, srtx=45)
detach(2)
}}
\keyword{hplot}

