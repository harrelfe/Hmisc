\name{latexTabular}
\alias{latexTabular}
\title{Convert a Data Frame or Matrix to a LaTeX Tabular}
\description{
\code{latexTabular} creates a character vector representing a matrix or
data frame in a simple \samp{tabular} environment.
}
\usage{
latexTabular(x, headings=colnames(x),
             align =paste(rep('c',ncol(x)),collapse=''),
             halign=paste(rep('c',ncol(x)),collapse=''),
             helvetica=TRUE, translate=TRUE, hline=0, center=FALSE, \dots)
}
\arguments{
  \item{x}{a matrix or data frame, or a vector that is automatically
		converted to a matrix}
  \item{headings}{a vector of character strings specifying column
		headings for \samp{latexTabular}, defaulting to \code{x}'s
	\code{colnames}.  To make multi-line headers use the newline character
	inside elements of \code{headings}.}
  \item{align}{a character strings specifying column
		alignments for \samp{latexTabular}, defaulting to
		\code{paste(rep('c',ncol(x)),collapse='')} to center.  You may
		specify \code{align='c|c'} and other LaTeX tabular formatting.}
  \item{halign}{a character strings specifying alignment for
		column headings, defaulting to centered.}
  \item{helvetica}{set to \code{FALSE} to use default LaTeX font in
		\samp{latexTabular} instead of helvetica.}
	\item{translate}{set to \code{FALSE} if column headings and table
		entries are already in
		LaTeX format, otherwise \code{latexTabular} will run them through
		\code{latexTranslate}}
	\item{hline}{set to 1 to put \code{hline} after heading, 2 to also put
		\code{hline}s before and after heading and at table end}
	\item{center}{set to \code{TRUE} to enclose the tabular in a LaTeX
		\code{center} environment}
	\item{\dots}{if present, \code{x} is run through \code{format.df} with
		those extra arguments}
  }
\value{a character string containing LaTeX markup}
\author{
  Frank E. Harrell, Jr.,\cr
  Department of Biostatistics,\cr
  Vanderbilt University,\cr
  \email{fh@fharrell.com}
}
\seealso{
\code{\link{latex.default}}, \code{\link{format.df}}
}
\examples{
x <- matrix(1:6, nrow=2, dimnames=list(c('a','b'),c('c','d','this that')))
latexTabular(x)   # a character string with LaTeX markup
}
\keyword{utilities}
\keyword{interface}
\keyword{methods}
\keyword{file}
\keyword{character}
\keyword{manip}
