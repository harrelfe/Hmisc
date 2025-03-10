\name{Misc}
\alias{clowess}
\alias{confbar}
\alias{getLatestSource}
\alias{grType}
\alias{prType}
\alias{htmlSpecialType}
\alias{inverseFunction}
\alias{james.stein}
\alias{keepHattrib}
\alias{km.quick}
\alias{latexBuild}
\alias{lm.fit.qr.bare}
\alias{matxv}
\alias{makeSteps}
\alias{nomiss}
\alias{outerText}
\alias{plotlyParm}
\alias{plotp}
\alias{rendHTML}
\alias{restoreHattrib}
\alias{sepUnitsTrans}
\alias{strgraphwrap}
\alias{tobase64image}
\alias{trap.rule}
\alias{trellis.strip.blank}
\alias{unPaste}
\alias{whichClosest}
\alias{whichClosePW}
\alias{whichClosek}
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
  source code for functions in GitHub.

  \code{grType} retrieves the system option \code{grType}, which is
  forced to be \code{"base"} if the \code{plotly} package is not
  installed.

	\code{prType} retrieves the system option \code{prType}, which is
	set to \code{"plain"} if the option is not set.  \code{print} methods
	that allow for markdown/html/latex can be automatically invoked by
	setting \code{options(prType="html")} or
	\code{options(prType='latex')}.

	\code{htmlSpecialType} retrieves the system option
	\code{htmlSpecialType}, which is set to \code{"unicode"} if the option
	is not set.  \code{htmlSpecialType='unicode'} cause html-generating
	functions in \code{Hmisc} and \code{rms} to use unicode for special
	characters, and \code{htmlSpecialType='&'} uses the older ampersand
	3-digit format. 

  \code{inverseFunction} generates a function to find all inverses of a
  monotonic or nonmonotonic function that is tabulated at vectors (x,y),
  typically 1000 points.  If the original function is monotonic, simple linear
  interpolation is used and the result is a vector, otherwise linear
  interpolation is used within each interval in which the function is
  monotonic and the result is a matrix with number of columns equal to the
  number of monotonic intervals.  If a requested y is not within any
  interval, the extreme x that pertains to the nearest extreme y is
  returned. Specifying what='sample' to the returned function will cause a
  vector to be returned instead of a matrix, with elements taken as a
  random choice of the possible inverses.

  \code{james.stein} computes James-Stein shrunken estimates of cell
  means given a response variable (which may be binary) and a grouping
  indicator.

	\code{keepHattrib} for an input variable or a data frame, creates a
	list object saving special Hmisc attributes such as \code{label} and
	\code{units} that might be lost during certain operations such as
	running \code{data.table}.  \code{restoreHattrib} restores these attributes.

  \code{km.quick} provides a fast way to invoke \code{survfitKM} in the
  \code{survival} package to efficiently get Kaplan-Meier or Fleming-Harrington estimates for a
  single stratum for a vector of time points (if \code{times} is given) or to
  get a vector of survival time quantiles (if \code{q} is given).  If neither is given,
  the whole curve is returned in a list with objects \code{time} and \code{surv}, and
  there is an option to consider an interval as pertaining to greater than or equal
  to a specific time instead of the traditional greater than.  If the censoring is not 
  right censoring, the more general \code{survfit} is called by \code{km.quick}.

	\code{latexBuild} takes pairs of character strings and produces a
	single character string containing concatenation of all of them, plus
	an attribute \code{"close"} which is a character string containing the
	LaTeX closure that will balance LaTeX code with respect to
	parentheses, braces, brackets, or \code{begin} vs. \code{end}.  When
	an even-numbered element of the vector is not a left parenthesis,
	brace, or bracket, the element is taken as a word that was surrounded
	by \code{begin} and braces, for which the corresponding \code{end} is
	constructed in the returned attribute.

  \code{lm.fit.qr.bare} is a fast stripped-down function for computing
  regression coefficients, residuals, \eqn{R^2}, and fitted values.  It
  uses \code{lm.fit}. 

  \code{matxv} multiplies a matrix by a vector, handling automatic
  addition of intercepts if the matrix does not have a column of ones.
  If the first argument is not a matrix, it will be converted to one.
  An optional argument allows the second argument to be treated as a
  matrix, useful when its rows represent bootstrap reps of
  coefficients.  Then ab' is computed.  \code{matxv} respects the
  \code{"intercepts"} attribute if it is stored on \code{b} by the
  \code{rms} package.  This is used by \code{\link[rms]{orm}}
  fits that are bootstrap-repeated by \code{\link[rms]{bootcov}} where
  only the intercept corresponding to the median is retained.  If
  \code{kint} has nonzero length, it is checked for consistency with the
  attribute.

  \code{makeSteps} is a copy of the dostep function inside the
  \code{survival} package's \code{plot.survfit} function.  It expands a
  series of points to include all the segments needed to plot step
  functions.  This is useful for drawing polygons to shade confidence
  bands for step functions.

  \code{nomiss} returns a data frame (if its argument is one) with rows
  corresponding to \code{NA}s removed, or it returns a matrix with rows
  with any element missing removed.

  \code{outerText} uses \code{axis()} to put right-justified text
  strings in the right margin.  Placement depends on
  \code{par('mar')[4]}

	\code{plotlyParm} is a list of functions useful for specifying
	parameters to \code{plotly} graphics.

	\code{plotp} is a generic to handle \code{plotp} methods to make
	\code{plotly} graphics.

	\code{rendHTML} renders HTML in a character vector, first converting
	to one character string with newline delimeters.  If \code{knitr} is
	currently running, runs this string through \code{knitr::asis_output}
	so that the user need not include \code{results='asis'} in the chunk
	header for R Markdown or Quarto.  If \code{knitr} is not running, uses
	\code{htmltools::browsable} and \code{htmltools::HTML} and prints the
	result so that an RStudio viewer (if running inside RStudio) or
	separate browser window displays the rendered HTML.  The HTML code is
	surrounded by yaml markup to make Pandoc not fiddle with the HTML.
	Set the argument \code{html=FALSE} to not add this, in case you are
	really rendering markdown.  \code{html=FALSE} also invokes
	\code{rmarkdown::render} to convert the character vector to HTML
	before using \code{htmltools} to view, assuming the characters
	represent RMarkdown/Quarto text other than the YAML header.  If
	\code{options(rawmarkup=TRUE)} is in effect, \code{rendHTML} will just
	\code{cat()} its first argument.  This is useful when rendering is
	happening inside a Quarto margin, for example.
	
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

  \code{strgraphwrap} is like \code{strwrap} but is for the current
  graphics environment.

	\code{tobase64image} is a function written by Dirk Eddelbuettel that
  uses the \code{base64enc} package to convert a png graphic file to
  base64 encoding to include as an inline image in an html file.
	
  \code{trap.rule} computes the area under a curve using the trapezoidal
  rule, assuming \code{x} is sorted.

  \code{trellis.strip.blank} sets up Trellis or Lattice graphs to have a
  clear background on the strips for panel labels.
  
  \code{unPaste} provides a version of the S-Plus \code{unpaste} that
  works for \R and S-Plus.

  \code{whichClosePW} is a very fast function using weighted multinomial
  sampling to determine which element of a vector is "closest" to each
  element of another vector.  \code{whichClosest} quickly finds the closest
  element without any randomness.

  \code{whichClosek} is a slow function that finds, after jittering the
  lookup table, the \code{k} closest matchest to each element of the
  other vector, and chooses from among these one at random.

  \code{xless} is a function for Linux/Unix users to invoke the system
  \code{xless} command to pop up a window to display the result of
  \code{print}ing an object.  For MacOS \code{xless} uses the system \code{open} command to pop up a \code{TextEdit} window.
}
\usage{
confbar(at, est, se, width, q = c(0.7, 0.8, 0.9, 0.95, 0.99), 
        col = gray(c(0, 0.25, 0.5, 0.75, 1)),
        type = c("v", "h"), labels = TRUE, ticks = FALSE,
        cex = 0.5, side = "l", lwd = 5, clip = c(-1e+30, 1e+30),
        fun = function(x) x,
        qfun = function(x) ifelse(x == 0.5, qnorm(x),
                            ifelse(x < 0.5, qnorm(x/2),
                            qnorm((1 +  x)/2))))
getLatestSource(x=NULL, package='Hmisc', recent=NULL, avail=FALSE)
grType()
prType()
htmlSpecialType()
inverseFunction(x, y)
james.stein(y, group)
keepHattrib(obj)
km.quick(S, times, q,
        type = c("kaplan-meier", "fleming-harrington", "fh2"),
        interval = c(">", ">="), method=c('constant', 'linear'), fapprox=0, n.risk=FALSE)
latexBuild(\dots, insert, sep='')
lm.fit.qr.bare(x, y, tolerance, intercept=TRUE, xpxi=FALSE, singzero=FALSE)
matxv(a, b, kint=1, bmat=FALSE)
nomiss(x)
outerText(string, y, cex=par('cex'), \dots)
plotlyParm
plotp(data, ...)
rendHTML(x, html=TRUE)
restoreHattrib(obj, attribs)
sepUnitsTrans(x, conversion=c(day=1, month=365.25/12, year=365.25, week=7),
              round=FALSE, digits=0)
strgraphwrap(x, width = 0.9 * getOption("width"),
             indent = 0, exdent = 0,
             prefix = "", simplify = TRUE, units='user', cex=NULL)
tobase64image(file, Rd = FALSE, alt = "image")
trap.rule(x, y)
trellis.strip.blank()
unPaste(str, sep="/")
whichClosest(x, w)
whichClosePW(x, w, f=0.2)
whichClosek(x, w, k)
xless(x, \dots, title)
}
\arguments{
  \item{a}{a numeric matrix or vector}
	\item{alt,Rd}{see \code{base64::img}}
  \item{at}{x-coordinate for vertical confidence intervals, y-coordinate
		for horizontal}
	\item{attribs}{an object returned by \code{keepHattrib}}
  \item{avail}{set to \code{TRUE} to have \code{getLatestSource} return
	a data frame of available files and latest versions instead of
	fetching any}
  \item{b}{a numeric vector}
  \item{cex}{character expansion factor}
  \item{clip}{interval to truncate limits}
  \item{col}{vector of colors}
  \item{conversion}{a named numeric vector}
	\item{data}{an object having a \code{plotp} method}
  \item{digits}{number of digits used for \code{round}}
  \item{est}{vector of point estimates for confidence limits}
  \item{f}{a scaling constant}
	\item{file}{a file name}
  \item{fun}{function to transform scale}
  \item{group}{a categorical grouping variable}
	\item{html}{set to \code{FALSE} to tell \code{rendHTML} to not
  surround HTML code with yaml}
	\item{insert}{a list of 3-element lists for \code{latexBuild}.
		The first of each 3-element
		list is a character string with an environment name.  The second
		specifies the order: \code{"before"} or \code{"after"}, the former
		indicating that when the environment is found, the third element of
		the list is inserted before or after it, according to the second
		element.}
  \item{intercept}{set to \code{FALSE} to not automatically add a column
	of ones to the \code{x} matrix}
  \item{k}{get the \code{k} closest matches}
  \item{kint}{which element of \code{b} to add to the result if \code{a}
		does not contain a column for intercepts}
	\item{bmat}{set to \code{TRUE} to consider \code{b} a matrix of
		repeated coefficients, usually resampled estimates with rows
		corresponding to resamples}
  \item{labels}{set to \code{FALSE} to omit drawing confidence
	coefficients}
  \item{lwd}{line widths}
  \item{package}{name of package for \code{getLatestSource}, default is
		\code{'Hmisc'}}
	\item{obj}{a variable, data frame, or data table}
  \item{q}{vector of confidence coefficients or quantiles}
  \item{qfun}{quantiles on transformed scale}
  \item{recent}{an integer telling \code{getLatestSource} to get the
	\code{recent} most recently modified files from the package}
  \item{round}{set to \code{TRUE} to round converted values}
  \item{S}{a \code{\link[survival]{Surv}} object}
  \item{se}{vector of standard errors}
  \item{sep}{a single character string specifying the delimiter.  For
		\code{latexBuild} the default is \code{""}.}
  \item{side}{for \code{confbar} is \code{"b","l","t","r"} for bottom,
	left, top, right.}
  \item{str}{a character string vector}
  \item{string}{a character string vector}
  \item{ticks}{set to \code{TRUE} to draw lines between rectangles}
  \item{times}{a numeric vector of times}
  \item{title}{a character string to title a window or plot.  Ignored for \code{xless} under MacOs.}
  \item{tolerance}{tolerance for judging singularity in matrix}
  \item{type}{\code{"v"} for vertical, \code{"h"} for horizontal. 
  For \code{km.quick} specifies the type of survival estimator.}
  \item{w}{a numeric vector}
  \item{width}{width of confidence rectanges in user units, or see
	\code{\link{strwrap}}} 
  \item{x}{a numeric vector (matrix for \code{lm.fit.qr.bare}) or data
	frame.  For \code{xless} may be any object that is sensible to
	\code{print}.  For \code{sepUnitsTrans} is a character or factor
	variable.  For \code{getLatestSource} is a character string or
	vector of character strings containing base file names to retrieve
	from CVS.  Set \code{x='all'} to retrieve all source files.  For
	\code{clowess}, \code{x} may also be a list with x and y
	components.  For \code{inverseFunction}, \code{x} and \code{y}
	contain evaluations of the function whose inverse is needed.
	\code{x} is typically an equally-spaced grid of 1000 points.  For
	\code{strgraphwrap} is a character vector.  For \code{rendHTML}
  \code{x} is a character vector.}
  \item{xpxi}{set to \code{TRUE} to add an element to the result
		containing the inverse of \eqn{X'X}}
	\item{singzero}{set to \code{TRUE} to set coefficients corresponding
		to singular variables to zero instead of \code{NA}.}
  \item{y}{a numeric vector.  For \code{inverseFunction} \code{y} is the
	evaluated function values at \code{x}.}
  \item{indent, exdent, prefix}{see \code{\link{strwrap}}}
  \item{simplify}{see \code{\link{sapply}}}
  \item{units}{see \code{\link{par}}}
  \item{interval}{specifies whether to deal with probabilities of exceeding a value
  (the default) or of exceeding or equalling the value}
  \item{method,fapprox}{see \code{approx}}
  \item{n.risk}{set to \code{TRUE} to include the number at risk in the result}
  \item{\dots}{arguments passed through to another function.  For
		\code{latexBuild} represents pairs, with odd numbered elements being
		character strings containing LaTeX code or a zero-length object to
		ignore, and even-numbered elements representing LaTeX left
		parenthesis, left brace, or left bracket, or environment name.}
}
\author{Frank Harrell and Charles Dupont}
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

x <- seq(-1, 1, by=.01)
y <- x^2
h <- inverseFunction(x,y)
formals(h)$turns   # vertex
a <- seq(0, 1, by=.01)
plot(0, 0, type='n', xlim=c(-.5,1.5))
lines(a, h(a)[,1])            ## first inverse
lines(a, h(a)[,2], col='red') ## second inverse
a <- c(-.1, 1.01, 1.1, 1.2)
points(a, h(a)[,1])

d <- data.frame(x=1:2, y=3:4, z=5:6)
d <- upData(d, labels=c(x='X', z='Z lab'), units=c(z='mm'))
a <- keepHattrib(d)

d <- data.frame(x=1:2, y=3:4, z=5:6)
d2 <- restoreHattrib(d, a)
sapply(d2, attributes)

\dontrun{
getLatestSource(recent=5)  # source() most recent 5 revised files in Hmisc
getLatestSource('cut2')    # fetch and source latest cut2.s
getLatestSource('all')     # get everything
getLatestSource(avail=TRUE) # list available files and latest versions
}
}
\keyword{programming}
\keyword{utilities}
\keyword{iplot}
