\name{latexTherm}
\alias{latexTherm}
\alias{latexNeedle}
\alias{pngNeedle}
\title{Create LaTeX Thermometers and Colored Needles}
\description{
\code{latexTherm} creates a LaTeX picture environment for drawing a
series of thermometers 
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
for PDF using the LaTeX \code{ocgtools} package or using style
\code{tooltips}.  Typically the code will contain a \code{tabular}
environment.  The user must define a LaTeX macro \code{tooltipn} that
takes two arguments (original object and pop-up object) that does
the pop-up.

\code{latexNeedle} is similar to \code{latexTherm} except that vertical
needles are produced and each may have its own color.  A grayscale box
is placed around the needles and provides the 0-1 \code{y}-axis
reference.  Horizontal grayscale grid lines may be drawn.

\code{pngNeedle} is similar to \code{latexNeedle} but is for generating
small png graphics.  The full graphics file name is returned invisibly.
}
\usage{
latexTherm(y, name, w = 0.075, h = 0.15, spacefactor = 1/2, extra = 0.07,
           file = "", append = TRUE)

latexNeedle(y, x=NULL, col='black', href=0.5, name, w=.05, h=.15,
            extra=0, file = "", append=TRUE)

pngNeedle(y, x=NULL, col='black', href=0.5, lwd=3.5, w=6, h=18,
          file=tempfile(fileext='.png'))
}
\arguments{
  \item{y}{a vector of 0-1 scaled values.  Boxes and their frames are
		omitted for \code{NA} elements}
	\item{x}{a vector corresponding to \code{y} giving x-coordinates.
          Scaled accordingly, or defaults to equally-spaced values.}
  \item{name}{name of LaTeX macro variable to be defined}
  \item{w}{width of a single box (thermometer) in inches.  For
    \code{latexNeedle} and \code{pngNeedle} is the spacing between
          needles, the latter being in pixels.}
  \item{h}{height of a single box in inches.  For \code{latexNeedle} and
          \code{pngNeedle} is the height of the frame, the latter in pixels.}
  \item{spacefactor}{fraction of \code{w} added for extra space between
    boxes for \code{latexTherm}}
  \item{extra}{extra space in inches to set aside to the right of and
		above the series of boxes or frame}
  \item{file}{name of file to which to write LaTeX code.  Default is the
    console.  Also used as base file name for png graphic.  Default for
          that is from \code{tempfile}.}
  \item{append}{set to \code{FALSE} to write over \code{file}}
	\item{col}{a vector of colors corresponding to positions in \code{y}.
    \code{col} is repeated if too short.}
	\item{href}{values of \code{y} (0-1) for which horizontal grayscale
    reference lines are drawn for \code{latexNeedle} and
          \code{pngNeedle}.  Set to 
					\code{NULL} to not draw any reference lines}
  \item{lwd}{line width of needles for \code{pngNeedle}}
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
w <- pngNeedle(c(.2, .5, .7))
cat(tobase64image(w))  # can insert this directly into an html file
}}
\keyword{utilities}
\keyword{interface}
\keyword{file}
\keyword{character}
\keyword{manip}
