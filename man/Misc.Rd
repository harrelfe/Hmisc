\name{Misc}
\alias{clowess}
\alias{confbar}
\alias{getLatestSource}
\alias{james.stein}
\alias{km.quick}
\alias{lm.fit.qr.bare}
\alias{matxv}
\alias{nomiss}
\alias{outerText}
\alias{sepUnitsTrans}
\alias{trap.rule}
\alias{trellis.strip.blank}
\alias{under.unix}
\alias{.R.}
\alias{.SV4.}
\alias{unPaste}
\alias{whichClosest}
\alias{whichClosePW}
\alias{xless}
\title{Miscellaneous Functions}
\description{
  This documents miscellaneous small functions in Hmisc that may be of
  interest to users.

  \code{clowess} runs \code{lowess} but if the \code{iter} argument
  exceeds zero, sometimes wild values can result, in which case
  \code{lowess} is re-run with \code{iter=0}.
  
  \code{confbar} draws multi-level confidence bars using small rectangles
  that may be of different colors.

  \code{getLatestSource} fetches and \code{source}s the most recent
  source code for functions in packages in the Vanderbilty University
  CVS repository.

  \code{james.stein} computes James-Stein shrunken estimates of cell
  means given a response variable (which may be binary) and a grouping
  indicator.

  \code{km.quick} provides a fast way to invoke \code{survfit.km} in the
  \code{survival} package to get Kaplan-Meier estimates for a
  single stratum for a vector of time points (if \code{times} is given) or to
  get a vector of survival time quantiles (if \code{q} is given).

  \code{lm.fit.qr.bare} is a fast stripped-down function for computing
  regression coefficients, residuals, \eqn{R^2}, and fitted values.  It
  uses the Fortran routines \code{dqrls}. 

  \code{matxv} multiplies a matrix by a vector, handling automatic
  addition of intercepts if the matrix does not have a column of ones.
  If the first argument is not a matrix, it will be converted to one.

  \code{nomiss} returns a data frame (if its argument is one) with rows
  corresponding to \code{NA}s removed, or it returns a matrix with rows
  with any element missing removed.

  \code{outerText} uses \code{text()} to put test strings in left or
  right margins. It temporarily sets \code{par(xpd=NA)} if using \R.

  \code{sepUnitsTrans} converts character vectors containing values such
  as \code{c("3 days","3day","4month","2 years","2weeks","7")} to
  numeric vectors 
  (here \code{c(3,3,122,730,14,7)}) in a flexible fashion.  The user can
  specify a 
  vector of units of measurements and conversion factors.  The units
  with a conversion factor of \code{1} are taken as the target units,
  and if those units are present in the character strings they are
  ignored.  The target units are added to the resulting vector as the
  \code{"units"} attribute.
  
  \code{trap.rule} computes the area under a curve using the trapezoidal
  rule, assuming \code{x} is sorted.

  \code{trellis.strip.blank} sets up Trellis or Lattice graphs to have a
  clear background on the strips for panel labels.
  
  \code{under.unix} is a scalar logical value that is \code{TRUE} if you
  are running Linux or Unix.

  \code{.R.} is a logical value set to \code{TRUE} if running \R,
  \code{FALSE} for S-Plus.

  \code{.SV4.} is a logical value set to \code{TRUE} if running version
  4 of the S language under S-Plus, \code{FALSE} otherwise.
  
  \code{unPaste} provides a version of the S-Plus \code{unpaste} that
  works for \R and S-Plus.

  \code{whichClosePW} is a very fast function using weighted multinomial
  sampling to determine which element of a vector is "closest" to each
  element of another vector.  \code{whichClosest} quickly finds the closest
  element without any randomness.

  \code{xless} is a function for Linux/Unix users to invoke the system
  \code{xless} command to pop up a window to display the result of
  \code{print}ing an object.  For Windows, \code{xless} uses the builtin
  \code{page} function but with better defaults.
}
\usage{
confbar(at, est, se, width, q = c(0.7, 0.8, 0.9, 0.95, 0.99), 
        col = if (.R.) gray(c(0, 0.25, 0.5, 0.75, 1)) else
              if (under.unix) c(1, 0.8, 0.5, 0.2, 0.065) else
              c(1, 4, 3, 2, 5),
        type = c("v", "h"), labels = TRUE, ticks = FALSE,
        cex = 0.5, side = "l", lwd = 5, clip = c(-1e+30, 1e+30),
        fun = function(x) x,
        qfun = function(x) ifelse(x == 0.5, qnorm(x),
                            ifelse(x < 0.5, qnorm(x/2),
                            qnorm((1 +  x)/2))))
getLatestSource(x=NULL, package='Hmisc', recent=NULL, avail=FALSE) 
james.stein(y, group)
km.quick(S, times, q)
lm.fit.qr.bare(x, y, tolerance, intercept=TRUE, xpxi=FALSE)
matxv(a, b, kint=1)
nomiss(x)
outerText(string, y, setAside=string[1], side=4, space=1,
          adj=1, cex=par('cex'))
sepUnitsTrans(x, conversion=c(day=1, month=365.25/12, year=365.25, week=7),
              round=FALSE, digits=0)
trap.rule(x, y)
trellis.strip.blank()
unPaste(str, sep="/", extended=FALSE)
whichClosest(x, w)
whichClosePW(x, w, f=0.2)
xless(x, \dots, title)
}
\arguments{
  \item{a}{a numeric matrix or vector}
  \item{adj}{0 for left justification, 0.5 for centered, 1 for right}
  \item{at}{x-coordinate for vertical confidence intervals, y-coordinate
	for horizontal}
  \item{avail}{set to \code{TRUE} to have \code{getLatestSource} return
	a data frame of available files and latest versions instead of
	fetching any}
  \item{b}{a numeric vector}
  \item{cex}{character expansion factor}
  \item{clip}{interval to truncate limits}
  \item{col}{vector of colors}
  \item{conversion}{a named numeric vector}
  \item{digits}{number of digits used for \code{round}}
  \item{est}{vector of point estimates for confidence limits}
  \item{extended}{see \code{\link{strsplit}} in \R}
  \item{f}{a scaling constant}
  \item{fun}{function to transform scale}
  \item{group}{a categorical grouping variable}
  \item{intercept}{set to \code{FALSE} to not automatically add a column
	of ones to the \code{x} matrix}
  \item{kint}{which element of \code{b} to add to the result if \code{a}
	does not contain a column for intercepts}
  \item{labels}{set to \code{FALSE} to omit drawing confidence
	coefficients}
  \item{lwd}{line widths}
  \item{package}{name of package for \code{getLatestSource}, default is
	\code{'Hmisc'}}
  \item{q}{vector of confidence coefficients or quantiles}
  \item{qfun}{quantiles on transformed scale}
  \item{recent}{an integer telling \code{getLatestSource} to get the
	\code{recent} most recently modified files from the package}
  \item{round}{set to \code{TRUE} to round converted values}
  \item{S}{a \code{\link[survival]{Surv}} object}
  \item{se}{vector of standard errors}
  \item{sep}{a single character string specifying the delimiter}
  \item{setAside}{for \code{adj=1 side=4}, is a character string used to
	determine the space to set aside for all strings.}
  \item{side}{for \code{confbar} is \code{"b","l","t","r"} for bottom,
	left, top, right.  For \code{outText} is the using integers 1-4
	corresponding to these.}
  \item{space}{the number of extra characters to leave to the left of
	the string(s) (\code{adj=0}) or to the right (\code{adj=1})}
  \item{str}{a character string vector}
  \item{string}{a character string vector}
  \item{ticks}{set to \code{TRUE} to draw lines between rectangles}
  \item{times}{a numeric vector of times}
  \item{title}{a character string to title a window or plot}
  \item{tolerance}{tolerance for judging singularity in matrix}
  \item{type}{\code{"v"} for vertical, \code{"h"} for horizontal}
  \item{w}{a numeric vector}
  \item{width}{width of confidence rectanges in user units}
  \item{x}{a numeric vector (matrix for \code{lm.fit.qr.bare}) or data
	frame.  For \code{xless} may be any object that is sensible to
	\code{print}.  For \code{sepUnitsTrans} is a character or factor
	variable.  For \code{getLatestSource} is a character string or
	vector of character strings containing base file names to retrieve
	from CVS.  Set \code{x='all'} to retrieve all source files.  For
	\code{clowess}, \code{x} may also be a list with x and y components.}
  \item{xpxi}{set to \code{TRUE} to add an element to the result
	containing the inverse of \eqn{X'X}}
  \item{y}{a numeric vector}
  \item{\dots}{arguments passed through to another function}
}
\author{Frank Harrell}
\examples{
trap.rule(1:100,1:100)

unPaste(c('a;b or c','ab;d','qr;s'), ';')

sepUnitsTrans(c('3 days','4 months','2 years','7'))

set.seed(1)
whichClosest(1:100, 3:5)
whichClosest(1:100, rep(3,20))

whichClosePW(1:100, rep(3,20))
whichClosePW(1:100, rep(3,20), f=.05)
whichClosePW(1:100, rep(3,20), f=1e-10)

\dontrun{
getLatestSource(recent=5)  # source() most recent 5 revised files in Hmisc
getLatestSource('cut2')    # fetch and source latest cut2.s
getLatestSource('all')     # get everything
getLatestSource(avail=TRUE) # list available files and latest versions
}
}
\keyword{programming}
\keyword{utilities}
