\name{scat1d}
\alias{scat1d}
\alias{jitter2}
\alias{jitter2.default}
\alias{jitter2.data.frame}
\alias{datadensity}
\alias{datadensity.data.frame}
\alias{histSpike}
\alias{histSpikeg}
\alias{ecdfpM}
\title{One-Dimensional Scatter Diagram, Spike Histogram, or Density}
\description{
  \code{scat1d} adds tick marks (bar codes. rug plot) on any of the four
  sides of an existing plot, corresponding with non-missing values of a
  vector \code{x}.  This is used to show the data density.  Can also
  place the tick marks along a curve by specifying y-coordinates to go
  along with the \code{x} values.
  
  If any two values of \code{x} are within \eqn{\code{eps}*w} of
  each other, where \code{eps} defaults to .001 and w is the span
  of the intended axis, values of \code{x} are jittered by adding a
  value uniformly distributed in \eqn{[-\code{jitfrac}*w,
  \code{jitfrac}*w]}, where \code{jitfrac} defaults to
  .008. Specifying \code{preserve=TRUE} invokes \code{jitter2} with a
  different logic of jittering. Allows plotting random sub-segments to
  handle very large \code{x} vectors (see\code{tfrac}).
  
  \code{jitter2} is a generic method for jittering, which does not add
  random noise. It retains unique values and ranks, and randomly spreads
  duplicate values at equidistant positions within limits of enclosing
  values. \code{jitter2} is especially useful for numeric variables with
  discrete values, like rating scales. Missing values are allowed and
  are returned. Currently implemented methods are \code{jitter2.default}
  for vectors and \code{jitter2.data.frame} which returns a data.frame
  with each numeric column jittered.
  
  \code{datadensity} is a generic method used to show data densities in
  more complex situations.  Here, another \code{datadensity} method is
  defined for data frames. Depending on the \code{which} argument, some
  or all of the variables in a data frame will be displayed, with
  \code{scat1d} used to display continuous variables and, by default,
  bars used to display frequencies of categorical, character, or
  discrete numeric variables.  For such variables, when the total length
  of value labels exceeds 200, only the first few characters from each
  level are used. By default, \code{datadensity.data.frame} will
  construct one axis (i.e., one strip) per variable in the data frame.
  Variable names appear to the left of the axes, and the number of
  missing values (if greater than zero) appear to the right of the axes.
  An optional \code{group} variable can be used for stratification,
  where the different strata are depicted using different colors.  If
  the \code{q} vector is specified, the desired quantiles (over all
  \code{group}s) are displayed with solid triangles below each axis.
  
  When the sample size exceeds 2000 (this value may be modified using
  the \code{nhistSpike} argument, \code{datadensity} calls
  \code{histSpike} instead of \code{scat1d} to show the data density for
  numeric variables.  This results in a histogram-like display that
  makes the resulting graphics file much smaller.  In this case,
  \code{datadensity} uses the \code{minf} argument (see below) so that
  very infrequent data values will not be lost on the variable's axis,
  although this will slightly distortthe histogram.
  
  \code{histSpike} is another method for showing a high-resolution data
  distribution that is particularly good for very large datasets (say
  \eqn{\code{n} > 1000}).  By default, \code{histSpike} bins the
  continuous \code{x} variable into 100 equal-width bins and then
  computes the frequency counts within bins (if \code{n} does not exceed
  10, no binning is done). If \code{add=FALSE} (the default), the
  function displays either proportions or frequencies as in a vertical
  histogram.  Instead of bars, spikes are used to depict the
  frequencies.  If \code{add=FALSE}, the function assumes you are adding
  small density displays that are intended to take up a small amount of
  space in the margins of the overall plot.  The \code{frac} argument is
  used as with \code{scat1d} to determine the relative length of the
  whole plot that is used to represent the maximum frequency.  No
  jittering is done by \code{histSpike}.
  
  \code{histSpike} can also graph a kernel density estimate for
  \code{x}, or add a small density curve to any of 4 sides of an
  existing plot.  When \code{y} or \code{curve} is specified, the
  density or spikes are drawn with respect to the curve rather than the
  x-axis.

	\code{histSpikeg} is similar to \code{histSpike} but is for adding layers
	to a \code{ggplot2} graphics object or traces to a \code{plotly}
	object.
	\code{histSpikeg} can also add \code{lowess} curves to the plot.

	\code{ecdfpM} makes a \code{plotly} graph or series of graphs showing
	possibly superposed empirical cumulative distribution functions.
}
\usage{
scat1d(x, side=3, frac=0.02, jitfrac=0.008, tfrac,
       eps=ifelse(preserve,0,.001),
       lwd=0.1, col=par("col"),
       y=NULL, curve=NULL,
       bottom.align=FALSE,
       preserve=FALSE, fill=1/3, limit=TRUE, nhistSpike=2000, nint=100,
       type=c('proportion','count','density'), grid=FALSE, \dots)

jitter2(x, \dots)

\method{jitter2}{default}(x, fill=1/3, limit=TRUE, eps=0,
        presorted=FALSE, \dots)

\method{jitter2}{data.frame}(x, \dots)

datadensity(object, \dots)

\method{datadensity}{data.frame}(object, group,
            which=c("all","continuous","categorical"),
            method.cat=c("bar","freq"),
            col.group=1:10,
            n.unique=10, show.na=TRUE, nint=1, naxes,
            q, bottom.align=nint>1,
            cex.axis=sc(.5,.3), cex.var=sc(.8,.3),
            lmgp=NULL, tck=sc(-.009,-.002),
            ranges=NULL, labels=NULL, \dots)
# sc(a,b) means default to a if number of axes <= 3, b if >=50, use
# linear interpolation within 3-50

histSpike(x, side=1, nint=100, bins=NULL, frac=.05, minf=NULL, mult.width=1,
          type=c('proportion','count','density'),
          xlim=range(x), ylim=c(0,max(f)), xlab=deparse(substitute(x)), 
          ylab=switch(type,proportion='Proportion',
                           count     ='Frequency',
                           density   ='Density'),
          y=NULL, curve=NULL, add=FALSE, minimal=FALSE,
          bottom.align=type=='density', col=par('col'), lwd=par('lwd'),
          grid=FALSE, \dots)

histSpikeg(formula=NULL, predictions=NULL, data, plotly=NULL,
           lowess=FALSE, xlim=NULL, ylim=NULL,
           side=1, nint=100,
           frac=function(f) 0.01 + 0.02*sqrt(f-1)/sqrt(max(f,2)-1),
           span=3/4, histcol='black', showlegend=TRUE)

ecdfpM(x, group=NULL, what=c('F','1-F','f','1-f'), q=NULL,
       extra=c(0.025, 0.025), xlab=NULL, ylab=NULL, height=NULL, width=NULL,
       colors=NULL, nrows=NULL, ncols=NULL, ...)
}
\arguments{
  \item{x}{
    a vector of numeric data, or a data frame (for \code{jitter2} or
       \code{ecdfpM}) 
  }
  \item{object}{
    a data frame or list (even with unequal number of observations per
    variable, as long as \code{group} is notspecified)
  }
  \item{side}{
    axis side to use (1=bottom (default for \code{histSpike}), 2=left,
    3=top (default for \code{scat1d}), 4=right)
  }
  \item{frac}{
    fraction of smaller of vertical and horizontal axes for tick mark
    lengths. Can be negative to move tick marks outside of plot. For
    \code{histSpike}, this is the relative y-direction length to be used for the
    largest frequency. When \code{scat1d} calls \code{histSpike}, it
    multiplies its \code{frac} argument by 2.5.  For \code{histSpikeg},
    \code{frac} is a function of \code{f}, the vector of all frequencies.  The
		default function scales tick marks so that they are between 0.01 and
    0.03 of the y range, linearly scaled in the square root of the
    frequency less one.
  }
  \item{jitfrac}{
    fraction of axis for jittering.  If
    \eqn{\code{jitfrac} \le 0}{\code{jitfrac} <= 0}, no
    jittering is done. If \code{preserve=TRUE}, the amount of
    jittering is independent of jitfrac.
  }
  \item{tfrac}{
    Fraction of tick mark to actually draw.  If \eqn{\code{tfrac}<1},
    will draw a random fraction \code{tfrac}  of the line segment at
    each point. This is useful for very large samples or ones with some
    very dense points. The default value is 1 if the number of
    non-missing observations \code{n}  is less than 125, and
    \eqn{\max{(.1, 125/n)}} otherwise.
  }
  \item{eps}{
    fraction of axis for determining overlapping points in \code{x}. For
    \code{preserve=TRUE} the default is 0 and original unique values are
    retained, bigger values of eps tends to bias observations from dense
    to sparse regions, but ranks are still preserved.
  }
  \item{lwd}{
    line width for tick marks, passed to \code{segments}
  }
  \item{col}{
    color for tick marks, passed to \code{segments}
  }
  \item{y}{
    specify a vector the same length as \code{x} to draw tick marks
    along a curve instead of by one of the axes.  The \code{y} values
    are often predicted values from a model.  The \code{side} argument
    is ignored when \code{y} is given.  If the curve is already
    represented as a table look-up, you may specify it using the
    \code{curve} argument instead.  \code{y} may be a scalar to use a
    constant verticalplacement.
  }
  \item{curve}{
    a list containing elements \code{x} and \code{y} for which linear
    interpolation is used to derive \code{y} values corresponding to
    values of \code{x}.  This results in tick marks being drawn along
    the curve.  For \code{histSpike}, interpolated \code{y} values are
    derived for binmidpoints.
  }
	\item{minimal}{for \code{histSpike} set \code{minimal=TRUE} to draw a
       minimalist spike histogram with no y-axis.  This works best when
       produce graphics images that are short, e.g., have a height of
       two inches.  \code{add} is forced to be \code{FALSE} in this case
       so that a standalone graph is produced.  Only base graphics are
       used.}
  \item{bottom.align}{
    set to \code{TRUE} to have the bottoms of tick marks (for
    \code{side=1} or \code{side=3}) aligned at the y-coordinate.  The
    default behavior is to center the tick marks.  For
    \code{datadensity.data.frame}, \code{bottom.align} defaults to
    \code{TRUE} if \code{nint>1}.  In other words, if you are only
    labeling the first and last axis tick mark, the \code{scat1d} tick
    marks are centered on the variable's axis.
  }
  \item{preserve}{
    set to \code{TRUE} to invoke \code{jitter2}
  }
  \item{fill}{
    maximum fraction of the axis filled by jittered values. If \code{d}
    are duplicated values between a lower value l and upper value
    u, then d will be spread within
    \eqn{\pm \code{fill}*\min{(u-d,d-l)}/2}{
      +/- \code{fill}*min(u-d,d-l)/2}.
  }
  \item{limit}{
    specifies a limit for maximum shift in jittered values. Duplicate
    values will be spread within
    \eqn{\pm\code{fill}*\min{(u-d,d-l)}/2}{
      +/- \code{fill}*min(u-d,d-l)/2}. The
    default \code{TRUE} restricts jittering to the smallest
    \eqn{\min{(u-d,d-l)}/2} observed and results
    in equal amount of jittering for all d. Setting to
    \code{FALSE} allows for locally different amount of jittering, using
    maximum space available.
  }
  \item{nhistSpike}{
    If the number of observations exceeds or equals \code{nhistSpike},
    \code{scat1d} will automatically call \code{histSpike} to draw the
    data density, to prevent the graphics file from being too large.
  }
  \item{type}{
    used by or passed to \code{histSpike}.  Set to \code{"count"} to
    display frequency counts rather than relative frequencies, or
    \code{"density"} to display a kernel density estimate computed using
    the \code{density} function.
  }
  \item{grid}{
    set to \code{TRUE} if the \R \code{grid} package is in effect for
    the current plot
  }
  \item{nint}{
    number of intervals to divide each continuous variable's axis for
    \code{datadensity}. For \code{histSpike}, is the number of
    equal-width intervals for which to bin \code{x}, and if instead
    \code{nint} is a character string (e.g.,\code{nint="all"}), the
    frequency tabulation is done with no binning.  In other words,
    frequencies for all unique values of \code{x} are derived and
    plotted.  For \code{histSpikeg}, if \code{x} has no more than
    \code{nint} unique values, all observed values are used, otherwise
    the data are rounded before tabulation so that there are no more
    than \code{nint} intervals.  For \code{histSpike}, \code{nint} is
       ignored if \code{bins} is given.
		 }
  \item{bins}{for \code{histSpike} specifies the actual cutpoints to use
       for binning \code{x}.  The default is to use \code{nint} in
       conjunction with \code{xlim}.}
  \item{\dots}{
    optional arguments passed to \code{scat1d} from \code{datadensity}
    or to \code{histSpike} from \code{scat1d}.  For \code{histSpikep}
           are passed to the \code{lines} list to \code{add_trace}.  For
					 \code{ecdfpM} these arguments are passed to \code{add_lines}.
  }
  \item{presorted}{
    set to \code{TRUE} to prevent from sorting for determining the order
    \eqn{l<d<u}. This is usefull if an existing
    meaningfull local order would be destroyed by sorting, as in
    \eqn{\sin{(\pi*\code{sort}(\code{round}(\code{runif}(1000,0,10),1)))}}.
  }
  \item{group}{
    an optional stratification variable, which is converted to a
    \code{factor} vector if it is not one already
  }
  \item{which}{
    set \code{which="continuous"} to only plot continuous variables, or
    \code{which="categorical"} to only plot categorical, character, or
    discrete numeric ones.  By default, all types of variables are
    depicted.
  }
  \item{method.cat}{
    set \code{method.cat="freq"} to depict frequencies of categorical
    variables with digits representing the cell frequencies, with size
    proportional to the square root of the frequency.  By default,
    vertical bars are used.
  }
  \item{col.group}{
    colors representing the \code{group} strata.  The vector of colors
    is recycled to be the same length as the levels of \code{group}.
  }
  \item{n.unique}{
    number of unique values a numeric variable must have before it is
    considered to be a continuous variable
  }
  \item{show.na}{
    set to \code{FALSE} to suppress drawing the number of \code{NA}s to
    the right of each axis
  }
  \item{naxes}{
    number of axes to draw on each page before starting a new plot.  You
    can set \code{naxes} larger than the number of variables in the data
    frame if you want to compress the plot vertically.
  }
  \item{q}{
    a vector of quantiles to display.  By default, quantiles are not
    shown.
  }
	\item{extra}{a two-vector specifying the fraction of the x
       range to add on the left and the fraction to add on the right}
  \item{cex.axis}{
    character size for draw labels for axis tick marks
  }
  \item{cex.var}{
    character size for variable names and frequence of \code{NA}s
  }
  \item{lmgp}{
    spacing between numeric axis labels and axis (see \code{par} for
    \code{mgp})
  }
  \item{tck}{
    see \code{tck} under \code{\link{par}}
  }
  \item{ranges}{
    a list containing ranges for some or all of the numeric variables.
    If \code{ranges} is not given or if a certain variable is not found
    in the list, the empirical range, modified by \code{pretty}, is
    used.  Example:
    \code{ranges=list(age=c(10,100), pressure=c(50,150))}.
  }
  \item{labels}{
    a vector of labels to use in labeling the axes for
    \code{datadensity.data.frame}.  Default is to use the names of the
    variable in the input data frame.  Note: margin widths computed for
    setting aside names of variables use the names, and not these
    labels.
  }
  \item{minf}{
    For \code{histSpike}, if \code{minf} is specified low bin
    frequencies are set to a minimum value of \code{minf} times the
    maximum bin frequency, so that rare data points will remain visible.
    A good choice of \code{minf} is 0.075.
    \code{datadensity.data.frame} passes \code{minf=0.075} to
    \code{scat1d} to pass to \code{histSpike}.  Note that specifying
    \code{minf} will cause the shape of the histogram to be distorted
    somewhat.
  }
  \item{mult.width}{
    multiplier for the smoothing window width computed by
    \code{histSpike} when \code{type="density"}
  }
  \item{xlim}{
    a 2-vector specifying the outer limits of \code{x} for binning (and
    plotting, if \code{add=FALSE} and \code{nint} is a number).  For
    \code{histSpikeg}, observations outside the \code{xlim} range are ignored.
  }
  \item{ylim}{
    y-axis range for plotting (if \code{add=FALSE}).  Often needed for
    \code{histSpikeg} to help scale the tick mark line segments.
  }
  \item{xlab}{
    x-axis label (\code{add=FALSE} or for \code{ecdfpM}); default is
       name of input argument, or for \code{ecdfpM} comes from
       \code{label} and \code{units} attributes of the analysis
       variable.  For \code{ecdfpM} \code{xlab} may be a vector if there
       is more than one analysis variable.
		 }
  \item{ylab}{
    y-axis label (\code{add=FALSE} or for \code{ecdfpM})
  }
  \item{add}{
    set to \code{TRUE} to add the spike-histogram to an existing plot,
    to show marginal data densities
  }
	\item{formula}{
		a formula of the form \code{y ~ x1} or \code{y ~ x1 + \dots} where
    \code{y} is the name of the \code{y}-axis variable being plotted
    with \code{ggplot}, \code{x1} is the name of the \code{x}-axis
    variable, and optional \dots are variables used by
    \code{ggplot} to produce multiple curves on a panel and/or facets.
	}
	\item{predictions}{
		the data frame being plotted by \code{ggplot}, containing \code{x}
    and \code{y} coordinates of curves.  If omitted, spike histograms
    are drawn at the bottom (default) or top of the plot according to
    \code{side}.
  }
	\item{data}{
		for \code{histSpikeg} is a mandatory data frame containing raw data whose
    frequency distribution is to be summarized, using variables in
    \code{formula}. 
	}
	\item{plotly}{
		an existing \code{plotly} object.  If not \code{NULL},
           \code{histSpikeg} uses \code{plotly} instead of \code{ggplot}.}
	\item{lowess}{
		set to \code{TRUE} to have \code{histSpikeg} add a \code{geom_line}
           layer to the \code{ggplot2} graphic, containing
           \code{lowess()} nonparametric smoothers.  This causes the
           returned value of \code{histSpikeg} to be a list with two
           components: \code{"hist"} and \code{"lowess"} each containing
           a layer.  Fortunately, \code{ggplot2} plots both layers
           automatically.  If the dependent variable is binary,
           \code{iter=0} is passed to \code{lowess} so that outlier
           detection is turned off; otherwise \code{iter=3} is passed.
				 }
\item{span}{passed to \code{lowess} as the \code{f} argument}
\item{histcol}{color of line segments (tick marks) for
 \code{histSpikeg}.  Default is black.  Set to any color or to
   \code{"default"} to use the prevailing colors for the
   graphic.}
\item{showlegend}{set to \code{FALSE} too have the added \code{plotly}
  traces not have entries in the plot legend}
\item{what}{set to \code{"1-F"} to plot 1 minus the ECDF instead of the
       ECDF, \code{"f"} to plot cumulative frequency, or \code{"1-f"} to
       plot the inverse cumulative frequency}
\item{height,width}{passed to \code{plot_ly}}
\item{colors}{a vector of colors to pas to \code{add_lines}}
\item{nrows,ncols}{passed to \code{plotly::subplot}}
}
\value{
  \code{histSpike} returns the actual range of \code{x} used in its binning. 
  \code{histSpikeg} returns a list of \code{ggplot2} layers that \code{ggplot2}
  will easily add with \code{+}.
}
\section{Side Effects}{
  \code{scat1d} adds line segments to plot.
  \code{datadensity.data.frame} draws a complete plot.  \code{histSpike}
  draws a complete plot or adds to an existing plot.
}
\details{
  For \code{scat1d} the length of line segments used is
  \code{frac*min(par()$pin)/par()$uin[opp]} data units, where
  opp is the index of the opposite axis and \code{frac} defaults
  to .02.  Assumes that \code{plot} has already been called.  Current
  \code{par("usr")} is used to determine the range of data for the axis
  of the current plot.  This range is used in jittering and in
  constructing line segments.
}
\author{
Frank Harrell\cr
Department of Biostatistics\cr
Vanderbilt University\cr
Nashville TN, USA\cr
\email{fh@fharrell.com}


Martin Maechler (improved \code{scat1d})\cr
Seminar fuer Statistik\cr
ETH Zurich SWITZERLAND\cr
\email{maechler@stat.math.ethz.ch}


Jens Oehlschlaegel-Akiyoshi (wrote \code{jitter2})\cr
Center for Psychotherapy Research\cr
Christian-Belser-Strasse 79a\cr
D-70597 Stuttgart Germany\cr
\email{oehl@psyres-stuttgart.de}
}
\seealso{
  \code{\link{segments}}, \code{\link{jitter}}, \code{\link{rug}},
  \code{\link{plsmo}}, \code{\link{lowess}}, \code{\link{stripplot}},
  \code{\link{hist.data.frame}},\code{\link{Ecdf}}, \code{\link{hist}},
  \code{\link[lattice]{histogram}}, \code{\link{table}},
  \code{\link{density}}, \code{\link{stat_plsmo}}, \code{\link{histboxp}}
}
\examples{
plot(x <- rnorm(50), y <- 3*x + rnorm(50)/2 )
scat1d(x)                 # density bars on top of graph
scat1d(y, 4)              # density bars at right
histSpike(x, add=TRUE)       # histogram instead, 100 bins
histSpike(y, 4, add=TRUE)
histSpike(x, type='density', add=TRUE)  # smooth density at bottom
histSpike(y, 4, type='density', add=TRUE)


smooth <- lowess(x, y)    # add nonparametric regression curve
lines(smooth)             # Note: plsmo() does this
scat1d(x, y=approx(smooth, xout=x)$y) # data density on curve
scat1d(x, curve=smooth)   # same effect as previous command
histSpike(x, curve=smooth, add=TRUE) # same as previous but with histogram
histSpike(x, curve=smooth, type='density', add=TRUE)  
# same but smooth density over curve


plot(x <- rnorm(250), y <- 3*x + rnorm(250)/2)
scat1d(x, tfrac=0)        # dots randomly spaced from axis
scat1d(y, 4, frac=-.03)   # bars outside axis
scat1d(y, 2, tfrac=.2)    # same bars with smaller random fraction


x <- c(0:3,rep(4,3),5,rep(7,10),9)
plot(x, jitter2(x))       # original versus jittered values
abline(0,1)               # unique values unjittered on abline
points(x+0.1, jitter2(x, limit=FALSE), col=2)
                          # allow locally maximum jittering
points(x+0.2, jitter2(x, fill=1), col=3); abline(h=seq(0.5,9,1), lty=2)
                          # fill 3/3 instead of 1/3
x <- rnorm(200,0,2)+1; y <- x^2
x2 <- round((x+rnorm(200))/2)*2
x3 <- round((x+rnorm(200))/4)*4
dfram <- data.frame(y,x,x2,x3)
plot(dfram$x2, dfram$y)   # jitter2 via scat1d
scat1d(dfram$x2, y=dfram$y, preserve=TRUE, col=2)
scat1d(dfram$x2, preserve=TRUE, frac=-0.02, col=2)
scat1d(dfram$y, 4, preserve=TRUE, frac=-0.02, col=2)


pairs(jitter2(dfram))     # pairs for jittered data.frame
# This gets reasonable pairwise scatter plots for all combinations of
# variables where
#
# - continuous variables (with unique values) are not jittered at all, thus
#   all relations between continuous variables are shown as they are,
#   extreme values have exact positions.
#
# - discrete variables get a reasonable amount of jittering, whether they
#   have 2, 3, 5, 10, 20 \dots levels
#
# - different from adding noise, jitter2() will use the available space
#   optimally and no value will randomly mask another
#
# If you want a scatterplot with lowess smooths on the *exact* values and
# the point clouds shown jittered, you just need
#
pairs( dfram ,panel=function(x,y) { points(jitter2(x),jitter2(y))
                                    lines(lowess(x,y)) } )




datadensity(dfram)     # graphical snapshot of entire data frame
datadensity(dfram, group=cut2(dfram$x2,g=3))
                          # stratify points and frequencies by
                          # x2 tertiles and use 3 colors


# datadensity.data.frame(split(x, grouping.variable))
# need to explicitly invoke datadensity.data.frame when the
# first argument is a list

\dontrun{
require(rms)
require(ggplot2)
f <- lrm(y ~ blood.pressure + sex * (age + rcs(cholesterol,4)),
         data=d)
p <- Predict(f, cholesterol, sex)
g <- ggplot(p, aes(x=cholesterol, y=yhat, color=sex)) + geom_line() +
  xlab(xl2) + ylim(-1, 1)
g <- g + geom_ribbon(data=p, aes(ymin=lower, ymax=upper), alpha=0.2,
                linetype=0, show_guide=FALSE)
g + histSpikeg(yhat ~ cholesterol + sex, p, d)

# colors <- c('red', 'blue')
# p <- plot_ly(x=x, y=y, color=g, colors=colors, mode='markers')
# histSpikep(p, x, y, z, color=g, colors=colors)

w <- data.frame(x1=rnorm(100), x2=exp(rnorm(100)))
g <- c(rep('a', 50), rep('b', 50))
ecdfpM(w, group=g, ncols=2)
}
}
\keyword{dplot}
\keyword{aplot}
\keyword{hplot}
\keyword{distribution}
