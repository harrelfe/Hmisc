\name{summaryS}
\alias{summaryS}
\alias{plot.summaryS}
\alias{plotp.summaryS}
\alias{mbarclPanel}
\alias{medvPanel}
\alias{mbarclpl}
\alias{medvpl}
\title{Summarize Multiple Response Variables and Make Multipanel Scatter
	or Dot Plot}
\description{
Multiple left-hand formula variables along with right-hand side
conditioning variables are reshaped into a "tall and thin" data frame if
\code{fun} is not specified.  The resulting raw data can be plotted with
the \code{plot} method using user-specified \code{panel} functions for
\code{lattice} graphics, typically to make a scatterplot or \code{loess}
smooths, or both.  The \code{Hmisc} \code{panel.plsmo} function is handy
in this context.  Instead, if \code{fun} is specified, this function
takes individual response variables (which may be matrices, as in
\code{\link[survival]{Surv}} objects) and creates one or more summary
statistics that will be computed while the resulting data frame is being
collapsed to one row per condition.  The \code{plot} method in this case
plots a multi-panel dot chart using the \code{lattice}
\code{\link[lattice]{dotplot}} function if \code{panel} is not specified
to \code{plot}.  There is an option to print
selected statistics as text on the panels.  \code{summaryS} pays special
attention to \code{Hmisc} variable annotations: \code{label, units}.
When \code{panel} is specified in addition to \code{fun}, a special
\code{x-y} plot is made that assumes that the \code{x}-axis variable
(typically time) is discrete.  This is used for example to plot multiple
quantile intervals as vertical lines next to the main point.  A special
panel function \code{mvarclPanel} is provided for this purpose.

The \code{plotp} method produces corresponding \code{plotly} graphics.

When \code{fun} is given and \code{panel} is omitted, and the result of
\code{fun} is a vector of more than one 
statistic, the first statistic is taken as the main one.  Any columns
with names not in \code{textonly} will figure into the calculation of
axis limits.  Those in \code{textonly} will be printed right under the
dot lines in the dot chart.  Statistics with names in \code{textplot}
will figure into limits, be plotted, and printed.  \code{pch.stats} can
be used to specify symbols for statistics after the first column.  When
\code{fun} computed three columns that are plotted, columns two and
three are taken as confidence limits for which horizontal "error bars"
are drawn.  Two levels with different thicknesses are drawn if there are
four plotted summary statistics beyond the first.

\code{mbarclPanel} is used to draw multiple vertical lines around the
main points, such as a series of quantile intervals stratified by
\code{x} and paneling variables.  If \code{mbarclPanel} finds a column
of an arument \code{yother} that is named \code{"se"}, and if there are
exactly two levels to a superpositioning variable, the half-height of
the approximate 0.95 confidence interval for the difference between two
point estimates is shown, positioned at the midpoint of the two point
estimates at an \code{x} value.  This assume normality of point
estimates, and the standard error of the difference is the square root
of the sum of squares of the two standard errors.  By positioning the
intervals in this fashion, a failure of the two point estimates to touch
the half-confidence interval is consistent with rejecting the null
hypothesis of no difference at the 0.05 level.

\code{mbarclpl} is the \code{sfun} function corresponding to
\code{mbarclPanel} for \code{plotp}, and \code{medvpl} is the
\code{sfun} replacement for \code{medvPanel}.

\code{medvPanel} takes raw data and plots median \code{y} vs. \code{x},
along with confidence intervals and half-interval for the difference in
medians as with \code{mbarclPanel}.  Quantile intervals are optional.
Very transparent vertical violin plots are added by default.  Unlike
\code{panel.violin}, only half of the violin is plotted, and when there
are two superpose groups they are side-by-side in different colors.

For \code{plotp}, the function corresponding to \code{medvPanel} is
\code{medvpl}, which draws back-to-back spike histograms, optional Gini
mean difference, optional SD, quantiles (thin line version of box
plot with 0.05 0.25 0.5 0.75 0.95 quantiles), and half-width confidence
interval for differences in medians.  For quantiles, the Harrell-Davis
estimator is used.
}
\usage{
summaryS(formula, fun = NULL, data = NULL, subset = NULL,
         na.action = na.retain, continuous=10, \dots)

\method{plot}{summaryS}(x, formula=NULL, groups=NULL, panel=NULL,
           paneldoesgroups=FALSE, datadensity=NULL, ylab='',
           funlabel=NULL, textonly='n', textplot=NULL,
           digits=3, custom=NULL,
           xlim=NULL, ylim=NULL, cex.strip=1, cex.values=0.5, pch.stats=NULL,
           key=list(columns=length(groupslevels),
             x=.75, y=-.04, cex=.9,
             col=lattice::trellis.par.get('superpose.symbol')$col,
             corner=c(0,1)),
           outerlabels=TRUE, autoarrange=TRUE, scat1d.opts=NULL, \dots)

\method{plotp}{summaryS}(data, formula=NULL, groups=NULL, sfun=NULL,
           fitter=NULL, showpts=! length(fitter), funlabel=NULL,
           digits=5, xlim=NULL, ylim=NULL,
           shareX=TRUE, shareY=FALSE, autoarrange=TRUE, \dots)

mbarclPanel(x, y, subscripts, groups=NULL, yother, \dots)

medvPanel(x, y, subscripts, groups=NULL, violin=TRUE, quantiles=FALSE, \dots)

mbarclpl(x, y, groups=NULL, yother, yvar=NULL, maintracename='y',
         xlim=NULL, ylim=NULL, xname='x', alphaSegments=0.45, \dots)

medvpl(x, y, groups=NULL, yvar=NULL, maintracename='y',
       xlim=NULL, ylim=NULL, xlab=xname, ylab=NULL, xname='x',
       zeroline=FALSE, yother=NULL, alphaSegments=0.45,
       dhistboxp.opts=NULL, \dots)
}
\arguments{
  \item{formula}{a formula with possibly multiple left and right-side
		variables separated by \code{+}.  Analysis (response) variables are
		on the left and are typically numeric.  For \code{plot},
           \code{formula} is optional and overrides the default formula
           inferred for the reshaped data frame.}
  \item{fun}{an optional summarization function, e.g., \code{\link{smean.sd}}}
  \item{data}{optional input data frame.  For \code{plotp} is the object
       produced by \code{summaryS}.}
  \item{subset}{optional subsetting criteria}
  \item{na.action}{function for dealing with \code{NA}s when
		constructing the model data frame}
	\item{continuous}{minimum number of unique values for a numeric
           variable to have to be considered continuous}
  \item{\dots}{ignored for \code{summaryS} and \code{mbarclPanel},
		passed to \code{strip} and \code{panel} for \code{plot}.  Passed to
		the \code{\link{density}} function by \code{medvPanel}.  For
       \code{plotp}, are passed to \code{plotlyM} and \code{sfun}.  For
       \code{mbarclpl}, passed to \code{plotlyM}.}
	\item{x}{an object created by \code{summaryS}.  For \code{mbarclPanel}
		is an \code{x}-axis argument provided by \code{lattice}}
	\item{groups}{a character string or factor specifying that one of the
           conditioning variables is used for superpositioning and not
           paneling}
  \item{panel}{optional \code{lattice} \code{panel} function}
  \item{paneldoesgroups}{set to \code{TRUE} if, like
           \code{\link{panel.plsmo}}, the paneling function internally
           handles superpositioning for \code{groups}}
  \item{datadensity}{set to \code{TRUE} to add rug plots etc. using
    \code{\link{scat1d}}}
	\item{ylab}{optional \code{y}-axis label}
	\item{funlabel}{optional axis label for when \code{fun} is given}
	\item{textonly}{names of statistics to print and not plot.  By
           default, any statistic named \code{"n"} is only printed.}
	\item{textplot}{names of statistics to print and plot}
	\item{digits}{used if any statistics are printed as text (including
       \code{plotly} hovertext), to specify
           the number of significant digits to render}
	\item{custom}{a function that customizes formatting of statistics that
           are printed as text.  This is useful for generating plotmath
           notation.  See the example in the tests directory.}
	\item{xlim}{optional \code{x}-axis limits}
	\item{ylim}{optional \code{y}-axis limits}			 
	\item{cex.strip}{size of strip labels}
	\item{cex.values}{size of statistics printed as text}
	\item{pch.stats}{symbols to use for statistics (not included the one
           one in columne one) that are plotted.  This is a named
           vectors, with names exactly matching those created by
           \code{fun}.  When a column does not have an entry in
           \code{pch.stats}, no point is drawn for that column.}
	\item{key}{\code{lattice} \code{key} specification}
	\item{outerlabels}{set to \code{FALSE} to not pass two-way charts
    through \code{\link[latticeExtra]{useOuterStrips}}}
	\item{autoarrange}{set to \code{FALSE} to prevent \code{plot} from
    trying to optimize which conditioning variable is vertical}
	\item{scat1d.opts}{a list of options to specify to \code{\link{scat1d}}}
	\item{y, subscripts}{provided by \code{lattice}}
	\item{yother}{passed to the panel function from the \code{plot} method
		based on multiple statistics computed}
	\item{violin}{controls whether violin plots are included}
	\item{quantiles}{controls whether quantile intervals are included}
	\item{sfun}{a function called by \code{plotp.summaryS} to compute and
       plot user-specified summary measures.  Two functions for doing
       this are provided here: \code{mbarclpl, medvpl}.}
  \item{fitter}{a fitting function such as \code{loess} to smooth
       points.  The smoothed values over a systematic grid will be
       evaluated and plotted as curves.}
  \item{showpts}{set to \code{TRUE} to show raw data points in additon
    to smoothed curves}
	\item{shareX}{\code{TRUE} to cause \code{plotly} to share a single
    x-axis when graphs are aligned vertically}
	\item{shareY}{\code{TRUE} to cause \code{plotly} to share a single
    y-axis when graphs are aligned horizontally}
	\item{yvar}{a character or factor variable used to stratify the
    analysis into multiple y-variables}
	\item{maintracename}{a default trace name when it can't be inferred}
	\item{xname}{x-axis variable name for hover text when it can't be
    inferred}
	\item{xlab}{x-axis label when it can't be inferred}
	\item{alphaSegments}{alpha saturation to draw line segments for
    \code{plotly}}
	\item{dhistboxp.opts}{\code{list} of options to pass to \code{dhistboxp}}
	\item{zeroline}{set to \code{FALSE} to suppress \code{plotly} zero
    line at x=0}
}
\value{a data frame with added attributes for \code{summaryS} or a
           \code{lattice} object ready to render for \code{plot}}
\author{Frank Harrell}
\seealso{\code{\link{summary}}, \code{\link{summarize}}}
\examples{
# See tests directory file summaryS.r for more examples, and summarySp.r
# for plotp examples
require(survival)
n <- 100
set.seed(1)
d <- data.frame(sbp=rnorm(n, 120, 10),
                dbp=rnorm(n, 80, 10),
                age=rnorm(n, 50, 10),
                days=sample(1:n, n, TRUE),
                S1=Surv(2*runif(n)), S2=Surv(runif(n)),
                race=sample(c('Asian', 'Black/AA', 'White'), n, TRUE),
                sex=sample(c('Female', 'Male'), n, TRUE),
                treat=sample(c('A', 'B'), n, TRUE),
                region=sample(c('North America','Europe'), n, TRUE),
                meda=sample(0:1, n, TRUE), medb=sample(0:1, n, TRUE))

d <- upData(d, labels=c(sbp='Systolic BP', dbp='Diastolic BP',
            race='Race', sex='Sex', treat='Treatment',
            days='Time Since Randomization',
            S1='Hospitalization', S2='Re-Operation',
            meda='Medication A', medb='Medication B'),
            units=c(sbp='mmHg', dbp='mmHg', age='Year', days='Days'))

s <- summaryS(age + sbp + dbp ~ days + region + treat,  data=d)
# plot(s)   # 3 pages
plot(s, groups='treat', datadensity=TRUE,
     scat1d.opts=list(lwd=.5, nhistSpike=0))
plot(s, groups='treat', panel=lattice::panel.loess,
     key=list(space='bottom', columns=2),
     datadensity=TRUE, scat1d.opts=list(lwd=.5))

# To make a plotly graph when the stratification variable region is not
# present, run the following (showpts adds raw data points):
# plotp(s, groups='treat', fitter=loess, showpts=TRUE)

# Make your own plot using data frame created by summaryP
# xyplot(y ~ days | yvar * region, groups=treat, data=s,
#        scales=list(y='free', rot=0))

# Use loess to estimate the probability of two different types of events as
# a function of time
s <- summaryS(meda + medb ~ days + treat + region, data=d)
pan <- function(...)
   panel.plsmo(..., type='l', label.curves=max(which.packet()) == 1,
               datadensity=TRUE)
plot(s, groups='treat', panel=pan, paneldoesgroups=TRUE,
     scat1d.opts=list(lwd=.7), cex.strip=.8)

# Repeat using intervals instead of nonparametric smoother
pan <- function(...)  # really need mobs > 96 to est. proportion
  panel.plsmo(..., type='l', label.curves=max(which.packet()) == 1,
              method='intervals', mobs=5)

plot(s, groups='treat', panel=pan, paneldoesgroups=TRUE, xlim=c(0, 150))


# Demonstrate dot charts of summary statistics
s <- summaryS(age + sbp + dbp ~ region + treat, data=d, fun=mean)
plot(s)
plot(s, groups='treat', funlabel=expression(bar(X)))
# Compute parametric confidence limits for mean, and include sample
# sizes by naming a column "n"

f <- function(x) {
  x <- x[! is.na(x)]
  c(smean.cl.normal(x, na.rm=FALSE), n=length(x))
}
s <- summaryS(age + sbp + dbp ~ region + treat, data=d, fun=f)
plot(s, funlabel=expression(bar(X) \%+-\% t[0.975] \%*\% s))
plot(s, groups='treat', cex.values=.65,
     key=list(space='bottom', columns=2,
       text=c('Treatment A:','Treatment B:')))

# For discrete time, plot Harrell-Davis quantiles of y variables across
# time using different line characteristics to distinguish quantiles
d <- upData(d, days=round(days / 30) * 30)
g <- function(y) {
  probs <- c(0.05, 0.125, 0.25, 0.375)
  probs <- sort(c(probs, 1 - probs))
  y <- y[! is.na(y)]
  w <- hdquantile(y, probs)
  m <- hdquantile(y, 0.5, se=TRUE)
  se <- as.numeric(attr(m, 'se'))
  c(Median=as.numeric(m), w, se=se, n=length(y))
}
s <- summaryS(sbp + dbp ~ days + region, fun=g, data=d)
plot(s, panel=mbarclPanel)
plot(s, groups='region', panel=mbarclPanel, paneldoesgroups=TRUE)

# For discrete time, plot median y vs x along with CL for difference,
# using Harrell-Davis median estimator and its s.e., and use violin
# plots

s <- summaryS(sbp + dbp ~ days + region, data=d)
plot(s, groups='region', panel=medvPanel, paneldoesgroups=TRUE)

# Proportions and Wilson confidence limits, plus approx. Gaussian
# based half/width confidence limits for difference in probabilities
g <- function(y) {
  y <- y[!is.na(y)]
  n <- length(y)
  p <- mean(y)
  se <- sqrt(p * (1. - p) / n)
  structure(c(binconf(sum(y), n), se=se, n=n),
            names=c('Proportion', 'Lower', 'Upper', 'se', 'n'))
}
s <- summaryS(meda + medb ~ days + region, fun=g, data=d)
plot(s, groups='region', panel=mbarclPanel, paneldoesgroups=TRUE)
}
\keyword{category}
\keyword{hplot}
\keyword{manip}
\keyword{grouping}
\concept{stratification}
\concept{aggregation}
