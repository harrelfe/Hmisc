\name{plsmo}
\alias{plsmo}
\alias{panel.plsmo}
\title{
Plot smoothed estimates
}
\description{

  Plot smoothed estimates of x vs. y, handling missing data for lowess
  or supsmu, and adding axis labels.  Optionally suppresses plotting
  extrapolated estimates.  An optional \code{group} variable can be
  specified to compute and plot the smooth curves by levels of
  \code{group}.  When \code{group} is present, the \code{datadensity}
  option will draw tick marks showing the location of the raw
  \code{x}-values, separately for each curve.  \code{plsmo} has an
  option to plot connected points for raw data, with no smoothing.  The
  non-panel version of \code{plsmo} allows \code{y} to be a matrix, for
  which smoothing is done separately over its columns.  If both
  \code{group} and multi-column \code{y} are used, the number of curves
  plotted is the product of the number of groups and the number of
  \code{y} columns.

	\code{method='intervals'} is often used when y is binary, as it may be
	tricky to specify a reasonable smoothing parameter to \code{lowess} or
	\code{supsmu} in this case.  The \code{'intervals'} method uses the
	\code{cutGn} function to form intervals of x containing a minimum of
	\code{mobs} observations.  For each interval the \code{ifun} function
	summarizes y, with the default being the mean (proportions for binary
	y).  The results are plotted as step functions, with vertical
	discontinuities drawn with a saturation of 0.15 of the original color.
	A plus sign is drawn at the mean x within each interval.
	For this approach, the default x-range is the entire raw data range,
	and \code{trim} and \code{evaluate} are ignored.  For
	\code{panel.plsmo} it is best to specify \code{type='l'} when using
	\code{'intervals'}. 

\code{panel.plsmo} is a \code{panel} function for \code{trellis} for the
\code{xyplot} function that uses \code{plsmo} and its options to draw
one or more nonparametric function estimates on each panel.  This has
advantages over using \code{xyplot} with \code{panel.xyplot} and
\code{panel.loess}: (1) by default it will invoke \code{labcurve} to
label the curves where they are most separated, (2) the
\code{datadensity} option will put rug plots on each curve (instead of a
single rug plot at the bottom of the graph), and (3) when
\code{panel.plsmo} invokes \code{plsmo} it can use the "super smoother"
(\code{supsmu} function) instead of \code{lowess}, or pass
\code{method='intervals'}.  \code{panel.plsmo} 
senses when a \code{group} variable is specified to \code{xyplot} so
that it can invoke \code{\link[lattice]{panel.superpose}} instead of
\code{panel.xyplot}.  Using \code{panel.plsmo} through \code{trellis}
has some advantages over calling \code{plsmo} directly in that
conditioning variables are allowed and \code{trellis} uses nicer fonts
etc.

When a \code{group} variable was used, \code{panel.plsmo} creates a function
\code{Key} in the session frame that the user can invoke to draw a key for
individual data point symbols used for the \code{group}s.  
By default, the key is positioned at the upper right
corner of the graph.  If \code{Key(locator(1))} is specified, the key will
appear so that its upper left corner is at the coordinates of the
mouse click.

For \code{ggplot2} graphics the counterparts are
\code{\link{stat_plsmo}} and \code{\link{histSpikeg}}.
}
\usage{
plsmo(x, y, method=c("lowess","supsmu","raw","intervals"), xlab, ylab, 
      add=FALSE, lty=1 : lc, col=par("col"), lwd=par("lwd"),
      iter=if(length(unique(y))>2) 3 else 0, bass=0, f=2/3, mobs=30, trim, 
      fun, ifun=mean, group, prefix, xlim, ylim, 
      label.curves=TRUE, datadensity=FALSE, scat1d.opts=NULL,
      lines.=TRUE, subset=TRUE,
      grid=FALSE, evaluate=NULL, \dots)


#To use panel function:
#xyplot(formula=y ~ x | conditioningvars, groups,
#       panel=panel.plsmo, type='b', 
#       label.curves=TRUE,
#       lwd = superpose.line$lwd, 
#       lty = superpose.line$lty, 
#       pch = superpose.symbol$pch, 
#       cex = superpose.symbol$cex, 
#       font = superpose.symbol$font, 
#       col = NULL, scat1d.opts=NULL, \dots)
}
\arguments{
\item{x}{
vector of x-values, NAs allowed
}
\item{y}{
vector or matrix of y-values, NAs allowed
}
\item{method}{
\code{"lowess"} (the default), \code{"supsmu"}, \code{"raw"} to not
smooth at all, or \code{"intervals"} to use intervals (see above)
}
\item{xlab}{
x-axis label iff add=F.  Defaults of label(x) or argument name.
}
\item{ylab}{
y-axis label, like xlab.
}
\item{add}{
Set to T to call lines instead of plot.  Assumes axes already labeled.
}
\item{lty}{
line type, default=1,2,3,\dots, corresponding to columns of \code{y} and
\code{group} combinations
}
\item{col}{
color for each curve, corresponding to \code{group}.  Default is
current \code{par("col")}. 
}
\item{lwd}{
vector of line widths for the curves, corresponding to \code{group}.
Default is current \code{par("lwd")}. 
\code{lwd} can also be specified as an element of \code{label.curves} if
\code{label.curves} is a list.
}
\item{iter}{
iter parameter if \code{method="lowess"}, default=0 if \code{y} is binary, and 3 otherwise.
}
\item{bass}{
bass parameter if \code{method="supsmu"}, default=0.
}
\item{f}{passed to the \code{lowess} function, for \code{method="lowess"}}
\item{mobs}{for \code{method='intervals'}, the minimum number of
            observations per interval}
\item{trim}{
only plots smoothed estimates between trim and 1-trim quantiles
of x.  Default is to use 10th smallest to 10th largest x in the group if the number of observations in the group exceeds 200 (0 otherwise).
Specify trim=0 to plot over entire range.
}
\item{fun}{
after computing the smoothed estimates, if \code{fun} is given the y-values
are transformed by \code{fun()}
}
\item{ifun}{a summary statistic function to apply to the
            \code{y}-variable for \code{method='intervals'}.  Default is \code{mean}.}
\item{group}{
a variable, either a \code{factor} vector or one that will be converted to
\code{factor} by \code{plsmo}, that is used to stratify the data so that separate
smooths may be computed
}
\item{prefix}{
a character string to appear in group of group labels.  The presence of
\code{prefix} ensures that \code{labcurve} will be called even when \code{add=TRUE}.
}
\item{xlim}{
a vector of 2 x-axis limits.  Default is observed range.
}
\item{ylim}{
a vector of 2 y-axis limits.  Default is observed range.
}
\item{label.curves}{
set to \code{FALSE} to prevent \code{labcurve} from being called to label multiple
curves corresponding to \code{group}s.  Set to a list to pass options to
\code{labcurve}.  \code{lty} and \code{col} are passed to \code{labcurve} automatically.
}
\item{datadensity}{
set to \code{TRUE} to draw tick marks on each curve, using x-coordinates
of the raw data \code{x} values.  This is done using \code{scat1d}.
}
\item{scat1d.opts}{a list of options to hand to \code{scat1d}}
\item{lines.}{
set to \code{FALSE} to suppress smoothed curves from being drawn.  This can
make sense if \code{datadensity=TRUE}.
}
\item{subset}{
a logical or integer vector specifying a subset to use for processing,
with respect too all variables being analyzed
}
\item{grid}{
  set to \code{TRUE} if the \R \code{grid} package drew the current	plot}
\item{evaluate}{
	number of points to keep from smoother.  If specified, an
equally-spaced grid of \code{evaluate} \code{x} values will be obtained from the
smoother using linear interpolation.  This will keep from plotting an
enormous number of points if the dataset contains a very large number
of unique \code{x} values.}
\item{\dots}{
  optional arguments that are passed to \code{scat1d},
  or optional parameters to pass to \code{plsmo} from
  \code{panel.plsmo}.  See optional arguments for \code{plsmo} above.
}
\item{type}{
set to \code{p} to have \code{panel.plsmo} plot points (and not call \code{plsmo}), 
\code{l} to call \code{plsmo} and not plot points, or use the default \code{b} to plot both.
}
\item{pch,cex,font}{
vectors of graphical parameters corresponding to the \code{group}s (scalars
if \code{group} is absent).  By default, the parameters set up by
\code{trellis} will be used.
}
}
\value{
\code{plsmo} returns a list of curves (x and y coordinates) that was passed to \code{labcurve}
}
\section{Side Effects}{
plots, and \code{panel.plsmo} creates the \code{Key} function in the session frame.
}
\seealso{
	\code{\link{lowess}}, \code{\link{supsmu}}, \code{\link{label}},
	\code{\link{quantile}}, \code{\link{labcurve}}, \code{\link{scat1d}},
	\code{\link[lattice]{xyplot}}, \code{\link[lattice]{panel.superpose}},
	\code{\link[lattice]{panel.xyplot}}, \code{\link{stat_plsmo}},
	\code{\link{histSpikeg}}, \code{\link{cutGn}}
}
\examples{
set.seed(1)
x <- 1:100
y <- x + runif(100, -10, 10)
plsmo(x, y, "supsmu", xlab="Time of Entry") 
#Use label(y) or "y" for ylab


plsmo(x, y, add=TRUE, lty=2)
#Add lowess smooth to existing plot, with different line type


age <- rnorm(500, 50, 15)
survival.time <- rexp(500)
sex <- sample(c('female','male'), 500, TRUE)
race <- sample(c('black','non-black'), 500, TRUE)
plsmo(age, survival.time < 1, fun=qlogis, group=sex) # plot logit by sex

#Bivariate Y
sbp <- 120 + (age - 50)/10 + rnorm(500, 0, 8) + 5 * (sex == 'male')
dbp <-  80 + (age - 50)/10 + rnorm(500, 0, 8) - 5 * (sex == 'male')
Y <- cbind(sbp, dbp)
plsmo(age, Y)
plsmo(age, Y, group=sex)


#Plot points and smooth trend line using trellis 
# (add type='l' to suppress points or type='p' to suppress trend lines)
require(lattice)
xyplot(survival.time ~ age, panel=panel.plsmo)


#Do this for multiple panels
xyplot(survival.time ~ age | sex, panel=panel.plsmo)

#Repeat this using equal sample size intervals (n=25 each) summarized by
#the median, then a proportion (mean of binary y)
xyplot(survival.time ~ age | sex, panel=panel.plsmo, type='l',
       method='intervals', mobs=25, ifun=median)
ybinary <- ifelse(runif(length(sex)) < 0.5, 1, 0)
xyplot(ybinary ~ age, groups=sex, panel=panel.plsmo, type='l',
       method='intervals', mobs=75, ifun=mean, xlim=c(0, 120))


#Do this for subgroups of points on each panel, show the data
#density on each curve, and draw a key at the default location
xyplot(survival.time ~ age | sex, groups=race, panel=panel.plsmo,
       datadensity=TRUE)
Key()


#Use wloess.noiter to do a fast weighted smooth
plot(x, y)
lines(wtd.loess.noiter(x, y))
lines(wtd.loess.noiter(x, y, weights=c(rep(1,50), 100, rep(1,49))), col=2)
points(51, y[51], pch=18)   # show overly weighted point
#Try to duplicate this smooth by replicating 51st observation 100 times
lines(wtd.loess.noiter(c(x,rep(x[51],99)),c(y,rep(y[51],99)),
      type='ordered all'), col=3)
#Note: These two don't agree exactly
}
\keyword{smooth}
\keyword{nonparametric}
\keyword{hplot}
\concept{trellis}
\concept{lattice}


