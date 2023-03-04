\name{xYplot}
\alias{xYplot}
\alias{panel.xYplot}
\alias{prepanel.xYplot}
\alias{Dotplot}
\alias{panel.Dotplot}
\alias{prepanel.Dotplot}
\alias{Cbind}
\alias{[.Cbind}
\alias{setTrellis}
\alias{numericScale}
\title{xyplot and dotplot with Matrix Variables to Plot Error Bars and Bands}
\description{
A utility function \code{Cbind} returns the first argument as a vector and
combines all other arguments into a matrix stored as an attribute called
\code{"other"}.  The arguments can be named (e.g.,
\code{Cbind(pressure=y,ylow,yhigh)}) or a \code{label} attribute may be pre-attached
to the first argument. In either case, the name or label of the first
argument is stored as an attribute \code{"label"} of the object returned by
\code{Cbind}.  Storing other vectors as a matrix attribute facilitates plotting
error bars, etc., as \code{trellis} really wants the x- and y-variables to be
vectors, not matrices. If a single argument is given to \code{Cbind} and that
argument is a matrix with column dimnames, the first column is taken as the
main vector and remaining columns are taken as \code{"other"}. A subscript
method for \code{Cbind} objects subscripts the \code{other} matrix along
with the main \code{y} vector.

The \code{xYplot} function is a substitute for \code{xyplot} that allows for
simulated multi-column \code{y}. It uses by default the \code{panel.xYplot} and
\code{prepanel.xYplot} functions to do the actual work. The \code{method} argument
passed to \code{panel.xYplot} from \code{xYplot} allows you to make error bars, the
upper-only or lower-only portions of error bars, alternating lower-only and
upper-only bars, bands, or filled bands.  \code{panel.xYplot} decides how to
alternate upper and lower bars according to whether the median \code{y} value of
the current main data line is above the median \code{y} for all \code{groups} of
lines or not.  If the median is above the overall median, only the upper
bar is drawn. For \code{bands} (but not 'filled bands'), any number of other
columns of \code{y} will be drawn as lines having the same thickness, color, and
type as the main data line.  If plotting bars, bands, or filled bands and
only one additional column is specified for the response variable, that
column is taken as the half width of a precision interval for \code{y}, and the
lower and upper values are computed automatically as \code{y} plus or minus the
value of the additional column variable.


When a \code{groups} variable is present, \code{panel.xYplot} will create a function
in frame 0 (\code{.GlobalEnv} in \R) called \code{Key} that when
invoked will draw a key describing the 
\code{groups} labels, point symbols, and colors. By default, the key is outside
the graph.  For S-Plus, if \code{Key(locator(1))} is specified, the key will appear so that
its upper left corner is at the coordinates of the mouse click.  For
R/Lattice the first two arguments of \code{Key} (\code{x} and \code{y}) are fractions
of the page, measured from the lower left corner, and the default
placement is at \code{x=0.05, y=0.95}.  For \R, an optional argument
to \code{sKey}, \code{other}, may contain a list of arguments to pass to \code{draw.key} (see
\code{\link[lattice]{xyplot}} for a list of possible arguments, under
the \code{key} option).  


When \code{method="quantile"} is specified, \code{xYplot} automatically groups the
\code{x} variable into intervals containing a target of \code{nx} observations each,
and within each \code{x} group computes three quantiles of \code{y} and plots these
as three lines. The mean \code{x} within each \code{x} group is taken as the
\code{x}-coordinate. This will make a useful empirical display for large
datasets in which scatterdiagrams are too busy to see patterns of central
tendency and variability.  You can also specify a general function of a
data vector that returns a matrix of statistics for the \code{method} argument.
Arguments can be passed to that function via a list \code{methodArgs}.  The
statistic in the first column should be the measure of central tendency.
Examples of useful \code{method} functions are those listed under the help file
for \code{summary.formula} such as \code{smean.cl.normal}.

\code{xYplot} can also produce bubble plots.  This is done when
\code{size} is specified to \code{xYplot}.  When \code{size} is used, a
function \code{sKey} is generated for drawing a key to the character
sizes.  See the bubble plot example.  \code{size} can also specify a
vector where the first character of each observation is used as the
plotting symbol, if \code{rangeCex} is set to a single \code{cex}
value.  An optional argument to \code{sKey}, \code{other}, may contain
a list of arguments to pass to \code{draw.key} (see
\code{\link[lattice]{xyplot}} for a list of possible arguments, under
the \code{key} option).  See the bubble plot example.

\code{Dotplot} is a substitute for \code{dotplot} allowing for a matrix x-variable,
automatic superpositioning when \code{groups} is present, and creation of a
\code{Key} function.  When the x-variable (created by \code{Cbind} to simulate a
matrix) contains a total of 3 columns, the first column specifies where the
dot is positioned, and the last 2 columns specify starting and ending
points for intervals.  The intervals are shown using line type, width, and
color from the trellis \code{plot.line} list. By default, you will usually see a
darker line segment for the low and high values, with the dotted reference
line elsewhere. A good choice of the \code{pch} argument for such plots is \code{3}
(plus sign) if you want to emphasize the interval more than the point
estimate.  When the x-variable contains a total of 5 columns, the 2nd and
5th columns are treated as the 2nd and 3rd are treated above, and the 3rd
and 4th columns define an inner line segment that will have twice the
thickness of the outer segments. In addition, tick marks separate the outer
and inner segments.  This type of display (an example of which appeared in
\emph{The Elements of Graphing Data} by Cleveland) is very suitable for
displaying two confidence levels (e.g., 0.9 and 0.99) or the 0.05, 0.25,
0.75, 0.95 sample quantiles, for example.  For this display, the central
point displays well with a default circle symbol.


\code{setTrellis} sets nice defaults for Trellis graphics, assuming that the
graphics device has already been opened if using postscript, etc. By
default, it sets panel strips to blank and reference dot lines to thickness
1 instead of the Trellis default of 2.


\code{numericScale} is a utility function that facilitates using
\code{xYplot} to 
plot variables that are not considered to be numeric but which can readily
be converted to numeric using \code{as.numeric()}.  \code{numericScale}
by default will keep the name of the input variable as a \code{label}
attribute for the new numeric variable.
}
\usage{
Cbind(\dots)

xYplot(formula, data = sys.frame(sys.parent()), groups,
       subset, xlab=NULL, ylab=NULL, ylim=NULL,
       panel=panel.xYplot, prepanel=prepanel.xYplot, scales=NULL,
       minor.ticks=NULL, sub=NULL, \dots)

panel.xYplot(x, y, subscripts, groups=NULL, 
             type=if(is.function(method) || method=='quantiles') 
               'b' else 'p',
             method=c("bars", "bands", "upper bars", "lower bars", 
                      "alt bars", "quantiles", "filled bands"), 
             methodArgs=NULL, label.curves=TRUE, abline,
             probs=c(.5,.25,.75), nx=NULL,
             cap=0.015, lty.bar=1, 
             lwd=plot.line$lwd, lty=plot.line$lty, pch=plot.symbol$pch, 
             cex=plot.symbol$cex, font=plot.symbol$font, col=NULL, 
             lwd.bands=NULL, lty.bands=NULL, col.bands=NULL, 
             minor.ticks=NULL, col.fill=NULL,
             size=NULL, rangeCex=c(.5,3), \dots)

prepanel.xYplot(x, y, \dots)

Dotplot(formula, data = sys.frame(sys.parent()), groups, subset, 
        xlab = NULL, ylab = NULL, ylim = NULL,
        panel=panel.Dotplot, prepanel=prepanel.Dotplot,
        scales=NULL, xscale=NULL, \dots)

prepanel.Dotplot(x, y, \dots)

panel.Dotplot(x, y, groups = NULL,
              pch  = dot.symbol$pch, 
              col  = dot.symbol$col, cex = dot.symbol$cex, 
              font = dot.symbol$font, abline, \dots)

setTrellis(strip.blank=TRUE, lty.dot.line=2, lwd.dot.line=1)

numericScale(x, label=NULL, \dots)
}
\arguments{
\item{\dots}{
for \code{Cbind} \code{\dots} is any number of additional numeric
vectors. Unless you are using \code{Dotplot} (which allows for either 2
or 4 "other" variables) or \code{xYplot} with \code{method="bands"},
vectors after the first two are ignored.  If drawing bars and only one
extra variable is given in \code{\dots}, upper and lower values are
computed as described above. If the second argument to \code{Cbind} is a
matrix, that matrix is stored in the \code{"other"} attribute and
arguments after the second are ignored.  For bubble plots, name an
argument \code{cex}.

Also can be other arguments to pass to \code{labcurve}.
}
\item{formula}{
a \code{trellis} formula consistent with \code{xyplot} or \code{dotplot} 
}
\item{x}{
\code{x}-axis variable.  For \code{numericScale} \code{x} is any vector
such as \code{as.numeric(x)} returns a numeric vector suitable for x- or
y-coordinates.
}
\item{y}{
  a vector, or an object created by \code{Cbind} for \code{xYplot}.
\code{y} represents the main variable to plot, i.e., the variable used to
draw the main lines. For \code{Dotplot} the first argument to
\code{Cbind} will be the main \code{x}-axis variable.  
}
\item{data,subset,ylim,subscripts,groups,type,scales,panel,prepanel,xlab,ylab}{
  see \code{trellis.args}.  \code{xlab} and \code{ylab} get default values from
  \code{"label"} attributes.
}
\item{xscale}{allows one to use the default \code{scales} but specify
  only the \code{x} component of it for \code{Dotplot}}
\item{method}{
defaults to \code{"bars"} to draw error-bar type plots.  See meaning of other
values above.  \code{method} can be a function.  Specifying \code{method=quantile},
\code{methodArgs=list(probs=c(.5,.25,.75))} is the same as specifying
\code{method="quantile"} without specifying \code{probs}.
}
\item{methodArgs}{
a list containing optional arguments to be passed to the function specified
in \code{method}
}
\item{label.curves}{
set to \code{FALSE} to suppress invocation of \code{labcurve} to label primary curves
where they are most separated or to draw a legend in an empty spot on the
panel.  You can also set \code{label.curves} to a list of options to pass to
\code{labcurve}.  These options can also be passed as \code{\dots} to \code{xYplot}. See the
examples below.
}
\item{abline}{
a list of arguments to pass to \code{panel.abline} for each panel, e.g.
\code{list(a=0, b=1, col=3)} to draw the line of identity using color
3.  To make multiple calls to \code{panel.abline}, pass a list of
unnamed lists as \code{abline}, e.g., \code{abline=list(list(h=0),list(v=1))}.
}
\item{probs}{
a vector of three quantiles with the quantile corresponding to the central
line listed first. By default \code{probs=c(.5, .25, .75)}. You can also specify
\code{probs} through \code{methodArgs=list(probs=\dots)}.
}
\item{nx}{
number of target observations for each \code{x} group (see \code{cut2} \code{m} argument).
\code{nx} defaults to the minimum of 40 and the number of points in the current
stratum divided by 4. Set \code{nx=FALSE} or \code{nx=0} if \code{x} is already discrete and
requires no grouping.
}
\item{cap}{
the half-width of horizontal end pieces for error bars, as a fraction of
the length of the \code{x}-axis
}
\item{lty.bar}{
line type for bars
}
\item{lwd, lty, pch, cex, font, col}{
see \code{trellis.args}.  These are vectors when \code{groups} is present, and the
order of their elements corresponds to the different \code{groups}, regardless
of how many bands or bars are drawn. If you don't specify \code{lty.bands}, for
example, all band lines within each group will have the same \code{lty}.
}
\item{lty.bands, lwd.bands, col.bands}{
used to allow \code{lty}, \code{lwd}, \code{col} to vary across the different band lines
for different \code{groups}. These parameters are vectors or lists whose
elements correspond to the added band lines (i.e., they ignore the central
line, whose line characteristics are defined by \code{lty}, \code{lwd}, \code{col}). For
example, suppose that 4 lines are drawn in addition to the central line.
Specifying \code{lwd.bands=1:4} will cause line widths of 1:4 to be used for
every group, regardless of the value of \code{lwd}.  To vary characteristics
over the \code{groups} use e.g. \code{lwd.bands=list(rep(1,4), rep(2,4))} or
\code{list(c(1,2,1,2), c(3,4,3,4))}.
}
\item{minor.ticks}{
a list with elements \code{at} and \code{labels} specifying positions
and labels for minor tick marks to be used on the x-axis of each panel,
if any.
}
\item{sub}{an optional subtitle}
\item{col.fill}{
used to override default colors used for the bands in method='filled
bands'. This is a vector when \code{groups} is present, and the order of the
elements corresponds to the different \code{groups}, regardless of how many
bands are drawn.  The default colors for 'filled bands' are pastel colors
matching the default colors superpose.line$col (plot.line$col)
}
\item{size}{
  a vector the same length as \code{x} giving a variable whose values
  are a linear function of the size of the symbol drawn.  This is used
  for example for bubble plots.
}
\item{rangeCex}{
  a vector of two values specifying the range in character sizes to use
  for the \code{size} variable (lowest first, highest second).
  \code{size} values are linearly translated to this range, based on the
  observed range of \code{size} when \code{x} and \code{y} coordinates
  are not missing.  Specify a single numeric \code{cex} value for
  \code{rangeCex} to use the first character of each observations's
  \code{size} as the plotting symbol.
  }
\item{strip.blank}{
set to \code{FALSE} to not make the panel strip backgrounds blank 
}
\item{lty.dot.line}{
line type for dot plot reference lines (default = 1 for dotted; use 2 for
dotted)
}
\item{lwd.dot.line}{
line thickness for reference lines for dot plots (default = 1) 
}
\item{label}{
a scalar character string to be used as a variable label after
\code{numericScale} converts the 
variable to numeric form 
}
}
\value{
\code{Cbind} returns a matrix with attributes.  Other functions return standard
\code{trellis} results.
}
\section{Side Effects}{
plots, and \code{panel.xYplot} may create temporary \code{Key} and
\code{sKey} functions in the session frame. 
}
\details{
Unlike \code{xyplot}, \code{xYplot} senses the presence of a \code{groups} variable and
automatically invokes \code{panel.superpose} instead of \code{panel.xyplot}. The same
is true for \code{Dotplot} vs. \code{dotplot}.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{fh@fharrell.com}
\cr
Madeline Bauer
\cr
Department of Infectious Diseases
\cr
University of Southern California School of Medicine
\cr
\email{mbauer@usc.edu}
}
\seealso{
\code{\link[lattice]{xyplot}}, \code{\link[lattice]{panel.xyplot}}, \code{\link{summarize}}, \code{\link{label}}, \code{\link{labcurve}},
\code{\link{errbar}}, \code{\link[lattice:xyplot]{dotplot}}, 
\code{\link{reShape}}, \code{\link{cut2}}, \code{\link[lattice:panel.functions]{panel.abline}}
}
\examples{
# Plot 6 smooth functions.  Superpose 3, panel 2.
# Label curves with p=1,2,3 where most separated 
d <- expand.grid(x=seq(0,2*pi,length=150), p=1:3, shift=c(0,pi)) 
xYplot(sin(x+shift)^p ~ x | shift, groups=p, data=d, type='l') 
# Use a key instead, use 3 line widths instead of 3 colors 
# Put key in most empty portion of each panel
xYplot(sin(x+shift)^p ~ x | shift, groups=p, data=d, 
       type='l', keys='lines', lwd=1:3, col=1) 
# Instead of implicitly using labcurve(), put a 
# single key outside of panels at lower left corner
xYplot(sin(x+shift)^p ~ x | shift, groups=p, data=d, 
       type='l', label.curves=FALSE, lwd=1:3, col=1, lty=1:3) 
Key()

# Bubble plots
x <- y <- 1:8
x[2] <- NA
units(x) <- 'cm^2'
z <- 101:108
p <- factor(rep(c('a','b'),4))
g <- c(rep(1,7),2)
data.frame(p, x, y, z, g)
xYplot(y ~ x | p, groups=g, size=z)
 Key(other=list(title='g', cex.title=1.2))  # draw key for colors
sKey(.2,.85,other=list(title='Z Values', cex.title=1.2))
# draw key for character sizes

# Show the median and quartiles of height given age, stratified 
# by sex and race.  Draws 2 sets (male, female) of 3 lines per panel.
# xYplot(height ~ age | race, groups=sex, method='quantiles')


# Examples of plotting raw data
dfr <- expand.grid(month=1:12, continent=c('Europe','USA'), 
                   sex=c('female','male'))
set.seed(1)
dfr <- upData(dfr,
              y=month/10 + 1*(sex=='female') + 2*(continent=='Europe') + 
                runif(48,-.15,.15),
              lower=y - runif(48,.05,.15),
              upper=y + runif(48,.05,.15))


xYplot(Cbind(y,lower,upper) ~ month,subset=sex=='male' & continent=='USA',
       data=dfr)
xYplot(Cbind(y,lower,upper) ~ month|continent, subset=sex=='male',data=dfr)
xYplot(Cbind(y,lower,upper) ~ month|continent, groups=sex, data=dfr); Key() 
# add ,label.curves=FALSE to suppress use of labcurve to label curves where
# farthest apart


xYplot(Cbind(y,lower,upper) ~ month,groups=sex,
                              subset=continent=='Europe', data=dfr) 
xYplot(Cbind(y,lower,upper) ~ month,groups=sex, type='b',
                              subset=continent=='Europe', keys='lines',
                              data=dfr)
# keys='lines' causes labcurve to draw a legend where the panel is most empty


xYplot(Cbind(y,lower,upper) ~ month,groups=sex, type='b', data=dfr,
                              subset=continent=='Europe',method='bands') 
xYplot(Cbind(y,lower,upper) ~ month,groups=sex, type='b', data=dfr,
                              subset=continent=='Europe',method='upper')


label(dfr$y) <- 'Quality of Life Score'   
# label is in Hmisc library = attr(y,'label') <- 'Quality\dots'; will be
# y-axis label 
# can also specify Cbind('Quality of Life Score'=y,lower,upper) 
xYplot(Cbind(y,lower,upper) ~ month, groups=sex,
       subset=continent=='Europe', method='alt bars',
        offset=grid::unit(.1,'inches'), type='b', data=dfr)   
# offset passed to labcurve to label .4 y units away from curve
# for R (using grid/lattice), offset is specified using the grid
# unit function, e.g., offset=grid::unit(.4,'native') or
# offset=grid::unit(.1,'inches') or grid::unit(.05,'npc')


# The following example uses the summarize function in Hmisc to 
# compute the median and outer quartiles.  The outer quartiles are 
# displayed using "error bars"
set.seed(111)
dfr <- expand.grid(month=1:12, year=c(1997,1998), reps=1:100)
month <- dfr$month; year <- dfr$year
y <- abs(month-6.5) + 2*runif(length(month)) + year-1997
s <- summarize(y, llist(month,year), smedian.hilow, conf.int=.5) 
xYplot(Cbind(y,Lower,Upper) ~ month, groups=year, data=s, 
       keys='lines', method='alt', type='b')
# Can also do:
s <- summarize(y, llist(month,year), quantile, probs=c(.5,.25,.75),
               stat.name=c('y','Q1','Q3')) 
xYplot(Cbind(y, Q1, Q3) ~ month, groups=year, data=s, 
       type='b', keys='lines') 
# Or:
xYplot(y ~ month, groups=year, keys='lines', nx=FALSE, method='quantile',
       type='b') 
# nx=FALSE means to treat month as a discrete variable


# To display means and bootstrapped nonparametric confidence intervals 
# use:
s <- summarize(y, llist(month,year), smean.cl.boot) 
s
xYplot(Cbind(y, Lower, Upper) ~ month | year, data=s, type='b')
# Can also use Y <- cbind(y, Lower, Upper); xYplot(Cbind(Y) ~ ...) 
# Or:
xYplot(y ~ month | year, nx=FALSE, method=smean.cl.boot, type='b')


# This example uses the summarize function in Hmisc to 
# compute the median and outer quartiles.  The outer quartiles are 
# displayed using "filled bands"


s <- summarize(y, llist(month,year), smedian.hilow, conf.int=.5) 


# filled bands: default fill = pastel colors matching solid colors
# in superpose.line (this works differently in R)
xYplot ( Cbind ( y, Lower, Upper ) ~ month, groups=year, 
     method="filled bands" , data=s, type="l")


# note colors based on levels of selected subgroups, not first two colors
xYplot ( Cbind ( y, Lower, Upper ) ~ month, groups=year, 
     method="filled bands" , data=s, type="l",
     subset=(year == 1998 | year == 2000), label.curves=FALSE )


# filled bands using black lines with selected solid colors for fill
xYplot ( Cbind ( y, Lower, Upper ) ~ month, groups=year, 
     method="filled bands" , data=s, label.curves=FALSE,
     type="l", col=1, col.fill = 2:3)
Key(.5,.8,col = 2:3) #use fill colors in key


# A good way to check for stable variance of residuals from ols 
# xYplot(resid(fit) ~ fitted(fit), method=smean.sdl) 
# smean.sdl is defined with summary.formula in Hmisc


# Plot y vs. a special variable x
# xYplot(y ~ numericScale(x, label='Label for X') | country) 
# For this example could omit label= and specify 
#    y ~ numericScale(x) | country, xlab='Label for X'


# Here is an example of using xYplot with several options
# to change various Trellis parameters,
# xYplot(y ~ x | z, groups=v, pch=c('1','2','3'),
#        layout=c(3,1),     # 3 panels side by side
#        ylab='Y Label', xlab='X Label',
#        main=list('Main Title', cex=1.5),
#        par.strip.text=list(cex=1.2),
#        strip=function(\dots) strip.default(\dots, style=1),
#        scales=list(alternating=FALSE))


#
# Dotplot examples
#


s <- summarize(y, llist(month,year), smedian.hilow, conf.int=.5) 


setTrellis()            # blank conditioning panel backgrounds 
Dotplot(month ~ Cbind(y, Lower, Upper) | year, data=s) 
# or Cbind(\dots), groups=year, data=s


# Display a 5-number (5-quantile) summary (2 intervals, dot=median) 
# Note that summarize produces a matrix for y, and Cbind(y) trusts the 
# first column to be the point estimate (here the median) 
s <- summarize(y, llist(month,year), quantile,
               probs=c(.5,.05,.25,.75,.95), type='matrix') 
Dotplot(month ~ Cbind(y) | year, data=s) 
# Use factor(year) to make actual years appear in conditioning title strips

# Plot proportions and their Wilson confidence limits
set.seed(3)
d <- expand.grid(continent=c('USA','Europe'), year=1999:2001,
                 reps=1:100)
# Generate binary events from a population probability of 0.2
# of the event, same for all years and continents
d$y <- ifelse(runif(6*100) <= .2, 1, 0)
s <- with(d,
          summarize(y, llist(continent,year),
                    function(y) {
                     n <- sum(!is.na(y))
                     s <- sum(y, na.rm=TRUE)
                     binconf(s, n)
                    }, type='matrix')
)

Dotplot(year ~ Cbind(y) | continent,  data=s, ylab='Year',
        xlab='Probability')


# Dotplot(z ~ x | g1*g2)                 
# 2-way conditioning 
# Dotplot(z ~ x | g1, groups=g2); Key()  
# Key defines symbols for g2


# If the data are organized so that the mean, lower, and upper 
# confidence limits are in separate records, the Hmisc reShape 
# function is useful for assembling these 3 values as 3 variables 
# a single observation, e.g., assuming type has values such as 
# c('Mean','Lower','Upper'):
# a <- reShape(y, id=month, colvar=type) 
# This will make a matrix with 3 columns named Mean Lower Upper 
# and with 1/3 as many rows as the original data 
}
\keyword{hplot}
\concept{trellis}
\concept{lattice}
