\name{labcurve}
\alias{labcurve}
\alias{putKey}
\alias{putKeyEmpty}
\alias{largest.empty}
\alias{drawPlot}
\alias{plot.drawPlot}
\alias{bezier}
\title{Label Curves, Make Keys, and Interactively Draw Points and Curves}
\description{
\code{labcurve} optionally draws a set of curves then labels the curves.
A variety of methods for drawing labels are implemented, ranging from
positioning using the mouse to automatic labeling to automatic placement
of key symbols with manual placement of key legends to automatic
placement of legends.  For automatic positioning of labels or keys, a
curve is labeled at a point that is maximally separated from all of the
other curves.  Gaps occurring when curves do not start or end at the
same x-coordinates are given preference for positioning labels. If
labels are offset from the curves (the default behaviour), if the
closest curve to curve i is above curve i, curve i is labeled below its
line.  If the closest curve is below curve i, curve i is labeled above
its line.  These directions are reversed if the resulting labels would
appear outside the plot region.

Both ordinary lines and step functions are handled, and there is an
option to draw the labels at the same angle as the curve within a
local window.

Unless the mouse is used to position labels or plotting symbols are
placed along the curves to distinguish them, curves are examined at 100
(by default) equally spaced points over the range of x-coordinates in
the current plot area.  Linear interpolation is used to get
y-coordinates to line up (step function or constant interpolation is
used for step functions).  There is an option to instead examine all
curves at the set of unique x-coordinates found by unioning the
x-coordinates of all the curves.  This option is especially useful when
plotting step functions.  By setting \code{adj="auto"} you can have
\code{labcurve} try to optimally left- or right-justify labels depending
on the slope of the curves at the points at which labels would be
centered (plus a vertical offset).  This is especially useful when
labels must be placed on steep curve sections.

You can use the \code{on top} method to write (short) curve names
directly on the curves (centered on the y-coordinate).  This is
especially useful when there are many curves whose full labels would run
into each other.  You can plot letters or numbers on the curves, for
example (using the \code{keys} option), and have \code{labcurve} use the
\code{key} function to provide long labels for these short ones (see the
end of the example).  There is another option for connecting labels to
curves using arrows.  When \code{keys} is a vector of integers, it is
taken to represent plotting symbols (\code{pch}s), and these symbols are
plotted at equally-spaced x-coordinates on each curve (by default, using
5 points per curve).  The points are offset in the x-direction between
curves so as to minimize the chance of collisions.

To add a legend defining line types, colors, or line widths with no
symbols, specify \code{keys="lines"}, e.g., \code{labcurve(curves,
keys="lines", lty=1:2)}.

\code{putKey} provides a different way to use \code{key()} by allowing
the user to specify vectors for labels, line types, plotting characters,
etc.  Elements that do not apply (e.g., \code{pch} for lines
(\code{type="l"})) may be \code{NA}.  When a series of points is
represented by both a symbol and a line, the corresponding elements of
both \code{pch} and \code{lty}, \code{col.}, or \code{lwd} will be
non-missing.

\code{putKeyEmpty}, given vectors of all the x-y coordinates that have been
plotted, uses \code{largest.empty} to find the largest empty rectangle large
enough to hold the key, and draws the key using \code{putKey}.

\code{drawPlot} is a simple mouse-driven function for drawing series of
lines, step functions, polynomials, Bezier curves, and points, and
automatically labeling the point groups using \code{labcurve} or
\code{putKeyEmpty}.  When \code{drawPlot} is invoked it creates
temporary functions \code{Points}, \code{Curve}, and \code{Abline}.
The user calls these functions inside
the call to \code{drawPlot} to define groups of points in the order they
are defined with the mouse.  \code{Abline} is used to call \code{abline}
and not actually great a group of points. For some curve types, the
curve generated to represent the corresponding series of points is drawn
after all points are entered for that series, and this curve may be
different than the simple curve obtained by connecting points at the
mouse clicks.  For example, to draw a general smooth Bezier curve the
user need only click on a few points, and she must overshoot the final
curve coordinates to define the curve.  The originally entered points
are not erased once the curve is drawn.  The same goes for step
functions and polynomials.  If you \code{plot()} the object returned by
\code{drawPlot}, however, only final curves will be shown.  The last
examples show how to use \code{drawPlot}.

The \code{largest.empty} function finds the largest rectangle that is large
enough to hold a rectangle of a given height and width, such that the
rectangle does not contain any of a given set of points.  This is
used by \code{labcurve} and \code{putKeyEmpty} to position keys at the most
empty part of an existing plot.  The default method was created by Hans
Borchers.
}
\usage{
labcurve(curves, labels=names(curves),
         method=NULL, keys=NULL, keyloc=c("auto","none"),
         type="l", step.type=c("left", "right"), 
         xmethod=if(any(type=="s")) "unique" else "grid", 
         offset=NULL, xlim=NULL,
         tilt=FALSE, window=NULL, npts=100, cex=NULL, 
         adj="auto", angle.adj.auto=30,
         lty=pr$lty, lwd=pr$lwd, col.=pr$col, transparent=TRUE,
         arrow.factor=1, point.inc=NULL, opts=NULL, key.opts=NULL,
         empty.method=c('area','maxdim'), numbins=25, 
         pl=!missing(add), add=FALSE, 
         ylim=NULL, xlab="", ylab="",
         whichLabel=1:length(curves),
         grid=FALSE, xrestrict=NULL, \dots)

putKey(z, labels, type, pch, lty, lwd,
       cex=par('cex'), col=rep(par('col'),nc),
       transparent=TRUE, plot=TRUE, key.opts=NULL, grid=FALSE)

putKeyEmpty(x, y, labels, type=NULL,
            pch=NULL, lty=NULL, lwd=NULL,
            cex=par('cex'), col=rep(par('col'),nc),
            transparent=TRUE, plot=TRUE, key.opts=NULL,
            empty.method=c('area','maxdim'), 
            numbins=25, 
            xlim=pr$usr[1:2], ylim=pr$usr[3:4], grid=FALSE)

drawPlot(\dots, xlim=c(0,1), ylim=c(0,1), xlab='', ylab='',
         ticks=c('none','x','y','xy'),
         key=FALSE, opts=NULL)

# Points(label=' ', type=c('p','r'),
#        n, pch=pch.to.use[1], cex=par('cex'), col=par('col'),
#        rug = c('none','x','y','xy'), ymean)

# Curve(label=' ',
#       type=c('bezier','polygon','linear','pol','loess','step','gauss'),
#       n=NULL, lty=1, lwd=par('lwd'), col=par('col'), degree=2,
#      evaluation=100, ask=FALSE)

# Abline(\dots)

\method{plot}{drawPlot}(x, xlab, ylab, ticks,
     key=x$key, keyloc=x$keyloc, \dots)

largest.empty(x, y, width=0, height=0, 
              numbins=25, method=c('exhaustive','rexhaustive','area','maxdim'),
              xlim=pr$usr[1:2], ylim=pr$usr[3:4],
              pl=FALSE, grid=FALSE)
}
\arguments{
\item{curves}{
a list of lists, each of which have at least two components: a vector of
\code{x} values and a vector of corresponding \code{y} values.  \code{curves} is
mandatory except when \code{method="mouse"} or \code{"locator"}, in which 
case \code{labels} is mandatory.  Each list in \code{curves} may optionally have
any of the parameters \code{type}, \code{lty}, \code{lwd}, or \code{col}
for that curve, as defined below (see one of the last examples).
}
\item{z}{
a two-element list specifying the coordinate of the center of the key,
e.g. \code{locator(1)} to use the mouse for positioning
}
\item{labels}{
For \code{labcurve}, a vector of character strings used to label curves 
(which may contain newline characters to stack labels vertically).  The
default labels are taken from the names of the \code{curves} list.
Setting \code{labels=FALSE} will suppress drawing any labels (for
\code{labcurve} only). 
For \code{putKey} and \code{putKeyEmpty} is a vector of character strings
specifying group labels
}
\item{x}{see below}
\item{y}{
for \code{putKeyEmpty} and \code{largest.empty}, \code{x} and \code{y}
are same-length vectors specifying points that have been plotted.
\code{x} can also be an object created by \code{drawPlot}.
}
\item{\dots}{
For \code{drawPlot} is a series of invocations of \code{Points} and
\code{Curve} (see example).  Any number of point groups can be defined
in this way.  For \code{Abline} these may be any arguments to
\code{abline}. 
For \code{labcurve}, other parameters to pass to \code{text}.
}
\item{width}{see below}
\item{height}{
for \code{largest.empty}, specifies the minimum allowable width in
\code{x} units and the minimum allowable height in \code{y} units
}
\item{method}{
\code{"offset"} (the default) offsets labels at largest gaps between
curves, and draws labels beside curves.  
\code{"on top"} draws labels on top of the curves (especially
good when using keys).  
\code{"arrow"} draws arrows connecting labels to the curves.
\code{"mouse"} or \code{"locator"} positions labels according to mouse clicks.
If \code{keys} is specified and is an integer vector or is \code{"lines"}, 
\code{method} defaults to \code{"on top"}.  If \code{keys} is character,
\code{method} defaults to \code{"offset"}.  Set \code{method="none"} to
suppress all curve labeling and key drawing, which is useful when
\code{pl=TRUE} and you only need \code{labcurve} to draw the curves and the
rest of the basic graph.

For \code{largest.empty} specifies the method a rectangle that does not
collide with any of the (\code{x}, \code{y}) points.  The default
method, \code{'exhaustive'}, uses a Fortran translation of an R function
and algorithm developed by Hans Borchers.  The same result, more slowly,
may be obtained by using pure R code by specifying
\code{method='rexhaustive'}.  The original algorithms using binning (and
the only methods supported for S-Plus) are
still available.  For all methods, screening of candidate rectangles
having at least a given width in \code{x}-units of \code{width} or
having at least a given height in \code{y}-units of \code{height} is possible.
Use \code{method="area"} to use the binning method to find the rectangle
having the largest area, or \code{method="maxdim"} to use the binning
method to return with last rectangle searched that had both
the largest width and largest height over all previous rectangles.
}
\item{keys}{
This causes keys (symbols or short text) to be drawn on or beside
curves, and if \code{keyloc} is not equal to \code{"none"}, a legend to be
automatically drawn.  The legend links keys with full curve labels
and optionally with colors and line types.
Set \code{keys} to a vector of character strings, or a
vector of integers specifying plotting character (\code{pch} values -
see \code{points}).  For the latter case, the default behavior is to
plot the symbols periodically, at equally spaced x-coordinates.
}
\item{keyloc}{
When \code{keys} is specified, \code{keyloc} specifies how the legend
is to be positioned for drawing using the \code{key} function in
\code{trellis}.  The default is \code{"auto"}, for which the
\code{largest.empty} function to used to find the most empty part of the
plot.  If no empty rectangle large enough to hold the key is found, no
key will be drawn. Specify \code{keyloc="none"} to suppress drawing a
legend, or set \code{keyloc} to a 2-element list containing the x and y
coordinates for the center of the legend.  For example, use
\code{keyloc=locator(1)} to click the mouse at the center.
\code{keyloc} specifies the coordinates of the center of the
key to be drawn with \code{plot.drawPlot} when \code{key=TRUE}.
}
\item{type}{
for \code{labcurve}, a scalar or vector of character strings specifying the
method that the points in the curves were connected. \code{"l"} means
ordinary connections between points and \code{"s"} means step functions.
For \code{putKey} and \code{putKeyEmpty} is a vector of plotting types, \code{"l"}
for regular line, \code{"p"} for point, \code{"b"} for both point and line, and
\code{"n"} for none.  For \code{Points} is either \code{"p"} (the default) for
regular points, or \code{"r"} for rugplot (one-dimensional scatter diagram
to be drawn using the \code{scat1d} function).  For \code{Curve}, \code{type} is
\code{"bezier"} (the default) for drawing a smooth Bezier curves (which can
represent a non-1-to-1 function such as a circle), \code{"polygon"} for
orginary line segments, \code{"linear"} for a straight line defined by two
endpoints, \code{"pol"} for a \code{degree}-degree polynomial to be fitted to
the mouse-clicked points, \code{"step"} for a left-step-function, \code{"gauss"}
to plot a Gaussian density fitted to 3 clicked points, \code{"loess"} to
use the \code{lowess} function to smooth the clicked points, or a function
to draw a user-specified function, evaluated at \code{evaluation} points
spanning the whole x-axis.  For the density the user must click in the
left tail, at the highest value (at the mean), and in the right tail,
with the two tail values being approximately equidistant from the
mean.  The density is scaled to fit in the highest value regardless of
its area.
}
\item{step.type}{
type of step functions used (default is \code{"left"})
}
\item{xmethod}{
method for generating the unique set of x-coordinates to examine (see above).  Default is \code{"grid"} for \code{type="l"} or \code{"unique"} for 
\code{type="s"}.
}
\item{offset}{
distance in y-units between the center of the label and the line being
labeled.  Default is 0.75 times the height of an "m" that would be
drawn in a label.  For R grid/lattice you must specify offset using
the \code{grid} \code{unit} function, e.g., \code{offset=unit(2,"native")} or
\code{offset=unit(.25,"cm")} (\code{"native"} means data units)
}
\item{xlim}{
limits for searching for label positions, and is also used to set up
plots when \code{pl=TRUE} and \code{add=FALSE}.  Default is total x-axis
range for current plot (\code{par("usr")[1:2]}).  For
\code{largest.empty}, \code{xlim} limits the search for largest
rectanges, but it has the same default as above. For
\code{pl=TRUE,add=FALSE} you may want to extend \code{xlim} somewhat to
allow large keys to fit, when using \code{keyloc="auto"}.  For
\code{drawPlot} default is \code{c(0,1)}.  When using
\code{largest.empty} with \code{ggplot2}, \code{xlim} and \code{ylim}
are mandatory.
}
\item{tilt}{
set to \code{TRUE} to tilt labels to follow the curves, for \code{method="offset"}
when \code{keys} is not given.
}
\item{window}{
width of a window, in x-units, to use in determining the local slope
for tilting labels.  Default is 0.5 times number of characters in the
label times the x-width of an "m" in the current character size and font.
}
\item{npts}{
number of points to use if \code{xmethod="grid"}
}
\item{cex}{
character size to pass to \code{text} and \code{key}.  Default is current
\code{par("cex")}.  For \code{putKey}, \code{putKeyEmpty}, and \code{Points} is the size of the
plotting symbol.
}
\item{adj}{
Default is \code{"auto"} which has \code{labcurve} figure justification
automatically when \code{method="offset"}.  This will cause centering to be used when the local angle
of the curve is less than \code{angle.adj.auto} in absolute value, left
justification if the angle is larger and either the label is under a
curve of positive slope or over a curve of negative slope, and right
justification otherwise.  For step functions, left justification is used
when the label is above the curve and right justifcation otherwise.
Set \code{adj=.5} to center labels at computed coordinates.  Set to 0 for
left-justification, 1 for right.  Set \code{adj} to a vector to vary adjustments
over the curves.
}
\item{angle.adj.auto}{
see \code{adj}.  Does not apply to step functions.
}
\item{lty}{
vector of line types which were used to draw the curves.
This is only used when keys are drawn. If all of the
line types, line widths, and line colors are the same, 
lines are not drawn in the key.
}
\item{lwd}{
vector of line widths which were used to draw the curves.
This is only used when keys are drawn.  See \code{lty} also.
}
\item{col.}{vector of integer color numbers}
\item{col}{
vector of integer color numbers for use in curve labels, symbols,
lines, and legends.  Default is \code{par("col")} for all curves.
See \code{lty} also.
}
\item{transparent}{
Default is \code{TRUE} to make \code{key} draw transparent legends, i.e., to
suppress drawing a solid rectangle background for the legend.
Set to \code{FALSE} otherwise.
}
\item{arrow.factor}{
factor by which to multiply default arrow lengths
}
\item{point.inc}{
When \code{keys} is a vector of integers, \code{point.inc} specifies the x-increment
between the point symbols that are overlaid periodically on the curves.  
By default, \code{point.inc} is equal
to the range for the x-axis divided by 5.
}
\item{opts}{
an optional list which can be used to specify any of the options
to \code{labcurve}, with the usual element name abbreviations allowed.
This is useful when \code{labcurve} is being called from another
function.  Example: \code{opts=list(method="arrow", cex=.8, np=200)}.
For \code{drawPlot} a list of \code{labcurve} options to pass as
\code{labcurve(\dots, opts=)}.
}
\item{key.opts}{
a list of extra arguments you wish to pass to \code{key()}, e.g.,
\code{key.opts=list(background=1, between=3)}.  
The argument names must be spelled out in full.
}
\item{empty.method}{see below}
\item{numbins}{
These two arguments are passed to the \code{largest.empty} function's
\code{method} and \code{numbins} arguments (see below).
For \code{largest.empty} specifies the number of bins in which to
discretize both the \code{x} and \code{y} directions for searching for
rectangles.  Default is 25.
}
\item{pl}{
set to \code{TRUE} (or specify \code{add}) to cause the curves in \code{curves} to be
drawn, under the control of \code{type},\code{lty},\code{lwd},\code{col} parameters defined
either in the \code{curves} lists or in the separate arguments given to
\code{labcurve} or through \code{opts}.
For \code{largest.empty}, set \code{pl=TRUE} to show the rectangle the function 
found by drawing it with a solid color.  May not be used under \code{ggplot2}.
}
\item{add}{
By default, when curves are actually drawn by \code{labcurve} a new plot is
started.  To add to an existing plot, set \code{add=TRUE}.
}
\item{ylim}{
When a plot has already been started, \code{ylim} defaults to \code{par("usr")[3:4]}.
When \code{pl=TRUE}, \code{ylim} and \code{xlim} are determined from the ranges of the data.
Specify \code{ylim} yourself to take control of the plot construction.  
In some cases it is advisable to
make \code{ylim} larger than usual to allow for automatically-positioned keys.
For \code{largest.empty}, \code{ylim} specifies the limits on the y-axis to limit
the search for rectangle.  
Here \code{ylim} defaults to the same as above, i.e., the range
of the y-axis of an open plot from \code{par}.  For \code{drawPlot} the default
is \code{c(0,1)}.
}
\item{xlab}{see below}
\item{ylab}{
x-axis and y-axis labels when \code{pl=TRUE} and \code{add=FALSE} or for
\code{drawPlot}.
Defaults to \code{""} unless the first curve has names for its first two
elements, in which case the names of these elements are taken as
\code{xlab} and \code{ylab}.
}
\item{whichLabel}{
  integer vector corresponding to \code{curves} specifying which curves
  are to be labelled or have a legend
  }
\item{grid}{
set to \code{TRUE} if the R \code{grid} package was used to draw the
current plot.  This prevents \code{labcurve} from using
\code{par("usr")} etc.  If using R \code{grid} you can pass coordinates
and lengths having arbitrary units, as documented in the \code{unit}
function.  This is especially useful for \code{offset}.
}
\item{xrestrict}{
  When having \code{labcurve} label curves where they are most
  separated, you can restrict the search for this separation point to a
  range of the x-axis, specified as a 2-vector \code{xrestrict}.  This
  is useful when one part of the curve is very steep.  Even though
  steep regions may have maximum separation, the labels will collide
  when curves are steep.
  }
\item{pch}{
vector of plotting characters for \code{putKey} and \code{putKeyEmpty}.  Can be
any value including \code{NA} when only a line is used to indentify the
group.  Is a single plotting character for \code{Points}, with the default
being the next unused value from among 1, 2, 3, 4, 16, 17, 5, 6, 15,
18, 19.
}
\item{plot}{
set to \code{FALSE} to keep \code{putKey} or \code{putKeyEmpty} from actually drawing the
key.  Instead, the size of the key will be return by \code{putKey}, or the
coordinates of the key by \code{putKeyEmpty}.
}
\item{ticks}{
tells \code{drawPlot} which axes to draw tick marks and tick labels.
Default is \code{"none"}.
}
\item{key}{
for \code{drawPlot} and \code{plot.drawPlot}.  Default is \code{FALSE} so that \code{labcurve}
is used to label points or curves.  Set to \code{TRUE} to use
\code{putKeyEmpty}.}
}
\value{
\code{labcurve} returns an invisible list with components \code{x, y, offset, adj, cex, col}, and if \code{tilt=TRUE}, 
\code{angle}. \code{offset} is the amount to add to \code{y} to draw a label.
\code{offset} is negative if the label is drawn below the line.
\code{adj} is a vector containing the values 0, .5, 1.


\code{largest.empty} returns a list with elements \code{x} and \code{y}
specifying the coordinates of the center of the rectangle which was
found, and element \code{rect} containing the 4 \code{x} and \code{y}
coordinates of the corners of the found empty rectangle.  The
\code{area} of the rectangle is also returned.
}
\details{
  The internal functions \code{Points}, \code{Curve}, \code{Abline} have
  unique arguments as follows.
  \describe{
	\item{\code{label}:}{for \code{Points} and \code{Curve} is a single
	  character string to label that group of points}
	\item{\code{n}:}{number of points to accept from the mouse.  Default
	  is to input points until a right mouse click.}
	\item{\code{rug}:}{for \code{Points}.  Default is \code{"none"} to
	  not show the  marginal x or y distributions as rug plots, for the
	  points entered. Other possibilities are used to execute
	  \code{scat1d} to show the marginal distribution of x, y, or both
	  as rug plots.} 
	\item{\code{ymean}:}{for \code{Points}, subtracts a constant from
	  each y-coordinate entered to make the overall mean \code{ymean}}
	\item{\code{degree}:}{degree of polynomial to fit to points by
	  \code{Curve}} 
	\item{\code{evaluation}:}{number of points at which to evaluate
	  Bezier curves, polynomials, and other functions in \code{Curve}}
	\item{\code{ask}:}{set \code{ask=TRUE} to give the user the
	  opportunity to try again at specifying points for Bezier curves,
	  step functions, and polynomials}
  }
  
The \code{labcurve} function used some code from the function \code{plot.multicurve} written
by Rod Tjoelker of The Boeing Company (\email{tjoelker@espresso.rt.cs.boeing.com}).

If there is only one curve, a label is placed at the middle x-value,
and no fancy features such as \code{angle} or positive/negative offsets are
used.

\code{key} is called once (with the argument \code{plot=FALSE}) to find the key
dimensions.  Then an empty rectangle with at least these dimensions is
searched for using \code{largest.empty}.  Then \code{key} is called again to draw
the key there, using the argument \code{corner=c(.5,.5)} so that the center
of the rectangle can be specified to \code{key}.

If you want to plot the data, an easier way to use \code{labcurve} is
through \code{xYplot} as shown in some of its examples.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{fh@fharrell.com}
}
\seealso{
	\code{\link{approx}}, \code{\link{text}}, \code{\link{legend}},
	\code{\link{scat1d}}, \code{\link{xYplot}}, \code{\link{abline}}
}
\examples{
n <- 2:8
m <-  length(n)
type <- c('l','l','l','l','s','l','l')
# s=step function l=ordinary line (polygon)
curves <- vector('list', m)


plot(0,1,xlim=c(0,1),ylim=c(-2.5,4),type='n')


set.seed(39)


for(i in 1:m) {
  x <- sort(runif(n[i]))
  y <- rnorm(n[i])
  lines(x, y, lty=i, type=type[i], col=i)
  curves[[i]] <- list(x=x,y=y)
}


labels <- paste('Label for',letters[1:m])
labcurve(curves, labels, tilt=TRUE, type=type, col=1:m)


# Put only single letters on curves at points of 
# maximum space, and use key() to define the letters,
# with automatic positioning of the key in the most empty
# part of the plot
# Have labcurve do the plotting, leaving extra space for key


names(curves) <- labels
labcurve(curves, keys=letters[1:m], type=type, col=1:m,
         pl=TRUE, ylim=c(-2.5,4))


# Put plotting symbols at equally-spaced points,
# with a key for the symbols, ignoring line types


labcurve(curves, keys=1:m, lty=1, type=type, col=1:m,
         pl=TRUE, ylim=c(-2.5,4))




# Plot and label two curves, with line parameters specified with data
set.seed(191)
ages.f <- sort(rnorm(50,20,7))
ages.m <- sort(rnorm(40,19,7))
height.f <- pmin(ages.f,21)*.2+60
height.m <- pmin(ages.m,21)*.16+63


labcurve(list(Female=list(ages.f,height.f,col=2),
              Male  =list(ages.m,height.m,col=3,lty='dashed')),
         xlab='Age', ylab='Height', pl=TRUE)
# add ,keys=c('f','m') to label curves with single letters
# For S-Plus use lty=2


# Plot power for testing two proportions vs. n for various odds ratios, 
# using 0.1 as the probability of the event in the control group.  
# A separate curve is plotted for each odds ratio, and the curves are
# labeled at points of maximum separation


n  <- seq(10, 1000, by=10)
OR <- seq(.2,.9,by=.1)
pow <- lapply(OR, function(or,n)list(x=n,y=bpower(p1=.1,odds.ratio=or,n=n)),
              n=n)
names(pow) <- format(OR)
labcurve(pow, pl=TRUE, xlab='n', ylab='Power')


# Plot some random data and find the largest empty rectangle
# that is at least .1 wide and .1 tall


x <- runif(50)
y <- runif(50)
plot(x, y)
z <- largest.empty(x, y, .1, .1)
z
points(z,pch=3)  # mark center of rectangle, or
polygon(z$rect, col='blue')  # to draw the rectangle, or
#key(z$x, z$y, \dots stuff for legend)




# Use the mouse to draw a series of points using one symbol, and
# two smooth curves or straight lines (if two points are clicked), 
# none of these being labeled


# d <- drawPlot(Points(), Curve(), Curve())
# plot(d)


\dontrun{
# Use the mouse to draw a Gaussian density, two series of points
# using 2 symbols, one Bezier curve, a step function, and raw data
# along the x-axis as a 1-d scatter plot (rug plot).  Draw a key.
# The density function is fit to 3 mouse clicks
# Abline draws a dotted horizontal reference line
d <- drawPlot(Curve('Normal',type='gauss'),
              Points('female'), Points('male'), 
              Curve('smooth',ask=TRUE,lty=2), Curve('step',type='s',lty=3), 
              Points(type='r'), Abline(h=.5, lty=2),
              xlab='X', ylab='y', xlim=c(0,100), key=TRUE)
plot(d, ylab='Y')
plot(d, key=FALSE)  # label groups using labcurve
}
}
\keyword{hplot}
\keyword{aplot}
\keyword{dplot}
\keyword{iplot}



