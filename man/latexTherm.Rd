\name{latexTherm}
\alias{latexTherm}
\title{Create LaTeX Thermometers}
\description{
Creates a LaTeX picture environment for drawing a series of thermometers
whose heights depict the values of a variable \code{y} assumed to be
scaled from 0 to 1.  This is useful for showing fractions of sample
analyzed in any table or plot, intended for a legend.  For example, four
thermometers might be used to depict the fraction of enrolled patients
included in the current analysis, the fraction randomized, the fraction
of patients randomized to treatment A being analyzed, and the fraction
randomized to B being analyzed.  The picture is placed
inside a LaTeX macro definition for macro variable named \code{name}, to
be invoked by the user later in the LaTeX file using \code{name}
preceeded by a backslash.

If \code{y} has an attribute \code{"table"}, it is assumed to contain a
character string with LaTeX code.  This code is used as a tooltip popup
for PDF using the LaTeX \code{ocgtools} package.  Typically the code
will contain a \code{tabular} environment.
}
\usage{
latexTherm(y, name, w = 0.075, h = 0.15, spacefactor = 1/2, extra = 0.07,
           file = "", append = TRUE)
}
\arguments{
  \item{y}{a vector of 0-1 scaled values.  Boxes and their frames are
		omitted for \code{NA} elements}
  \item{name}{name of LaTeX macro variable to be defined}
  \item{w}{width of a single box (thermometer) in inches}
  \item{h}{height of a single box in inches}
  \item{spacefactor}{fraction of \code{w} added for extra space between boxes}
  \item{extra}{extra space in inches to set aside to the right of and
		above the series of boxes}
  \item{file}{name of file to which to write LaTeX code.  Default is the console.}
  \item{append}{set to \code{FALSE} to write over \code{file}}
}
\author{Frank Harrell}
\examples{
\dontrun{
# The following is in the Hmisc tests directory
# For a knitr example see latexTherm.Rnw in that directory
ct <- function(...) cat(..., sep='')
ct('\\documentclass{report}\\begin{document}\n')
latexTherm(c(1, 1, 1, 1), name='lta')
latexTherm(c(.5, .7, .4, .2), name='ltb')
latexTherm(c(.5, NA, .75, 0), w=.3, h=1, name='ltc', extra=0)
latexTherm(c(.5, NA, .75, 0), w=.3, h=1, name='ltcc')
latexTherm(c(0, 0, 0, 0), name='ltd')
ct('This is a the first:\\lta and the second:\\ltb\\\\ and the third
without extra:\\ltc END\\\\\nThird with extra:\\ltcc END\\\\ 
\\vspace{2in}\\\\ 
All data = zero, frame only:\\ltd\\\\
\\end{document}\n')
}}
\keyword{utilities}
\keyword{interface}
\keyword{file}
\keyword{character}
\keyword{manip}
