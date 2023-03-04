\name{Ecdf}
\alias{Ecdf}
\alias{Ecdf.default}
\alias{Ecdf.data.frame}
\alias{Ecdf.formula}
\alias{panel.Ecdf}
\alias{prepanel.Ecdf}
\title{Empirical Cumulative Distribution Plot}
\description{
Computes coordinates of cumulative distribution function of x, and by defaults
plots it as a step function.  A grouping variable may be specified so that
stratified estimates are computed and (by default) plotted.  If there is
more than one group, the \code{labcurve} function is used (by default) to label
the multiple step functions or to draw a legend defining line types, colors,
or symbols by linking them with group labels.  A \code{weights} vector may
be specified to get weighted estimates.  Specify \code{normwt} to make
\code{weights} sum to the length of \code{x} (after removing NAs).  Other wise
the total sample size is taken to be the sum of the weights.

\code{Ecdf} is actually a method, and \code{Ecdf.default} is what's
called for a vector argument.  \code{Ecdf.data.frame} is called when the
first argument is a data frame.  This function can automatically set up
a matrix of ECDFs and wait for a mouse click if the matrix requires more
than one page.  Categorical variables, character variables, and
variables having fewer than a set number of unique values are ignored.
If \code{par(mfrow=..)} is not set up before \code{Ecdf.data.frame} is
called, the function will try to figure the best layout depending on the
number of variables in the data frame.  Upon return the original
\code{mfrow} is left intact.

When the first argument to \code{Ecdf} is a formula, a Trellis/Lattice function
\code{Ecdf.formula} is called.  This allows for multi-panel
conditioning, superposition using a \code{groups} variable, and other
Trellis features, along with the ability to easily plot transformed
ECDFs using the \code{fun} argument.  For example, if \code{fun=qnorm},
the inverse normal transformation will be used for the y-axis.  If the
transformed curves are linear this indicates normality.  Like the
\code{xYplot} function, \code{Ecdf} will create a function \code{Key} if
the \code{groups} variable is used.  This function can be invoked by the
user to define the keys for the groups.
}

\usage{
Ecdf(x, \dots)

\method{Ecdf}{default}(x, what=c('F','1-F','f','1-f'),
     weights=rep(1, length(x)), normwt=FALSE,
     xlab, ylab, q, pl=TRUE, add=FALSE, lty=1, 
     col=1, group=rep(1,length(x)), label.curves=TRUE, xlim, 
     subtitles=TRUE, datadensity=c('none','rug','hist','density'),
     side=1, 
     frac=switch(datadensity,none=NA,rug=.03,hist=.1,density=.1),
     dens.opts=NULL, lwd=1, log='', \dots)


\method{Ecdf}{data.frame}(x, group=rep(1,nrows),
     weights=rep(1, nrows), normwt=FALSE,
     label.curves=TRUE, n.unique=10, na.big=FALSE, subtitles=TRUE, 
     vnames=c('labels','names'),\dots)

\method{Ecdf}{formula}(x, data=sys.frame(sys.parent()), groups=NULL,
     prepanel=prepanel.Ecdf, panel=panel.Ecdf, \dots, xlab,
     ylab, fun=function(x)x, what=c('F','1-F','f','1-f'), subset=TRUE)
}
\arguments{
\item{x}{a numeric vector, data frame, or Trellis/Lattice formula}
\item{what}{
The default is \code{"F"} which results in plotting the fraction of values
<= x.  Set to \code{"1-F"} to plot the fraction > x or \code{"f"} to plot the
cumulative frequency of values <= x.  Use \code{"1-f"} to plot the
cumulative frequency of values >= x.
}
\item{weights}{
numeric vector of weights.  Omit or specify a zero-length vector or
NULL to get unweighted estimates.
}
\item{normwt}{see above}
\item{xlab}{
x-axis label.  Default is label(x) or name of calling argument.  For
\code{Ecdf.formula}, \code{xlab} defaults to the \code{label} attribute
of the x-axis variable.
}
\item{ylab}{
y-axis label.  Default is \code{"Proportion <= x"}, \code{"Proportion > x"}, 
or "Frequency <= x" depending on value of \code{what}.
}
\item{q}{
a vector for quantiles for which to draw reference lines on the plot.
Default is not to draw any.
}
\item{pl}{set to F to omit the plot, to just return estimates}
\item{add}{
set to TRUE to add the cdf to an existing plot.  Does not apply if using
     lattice graphics (i.e., if a formula is given as the first argument).
}
\item{lty}{
integer line type for plot.  If \code{group} is specified, this can be a vector.
}
\item{lwd}{
  line width for plot.  Can be a vector corresponding to \code{group}s.
}
\item{log}{
	see \code{\link{plot}}.  Set \code{log='x'} to use log scale for
  \code{x}-axis.
	}
\item{col}{
color for step function.  Can be a vector.
}
\item{group}{
a numeric, character, or \code{factor} categorical variable used for stratifying
estimates.  If \code{group} is present, as many ECDFs are drawn as there are
non--missing group levels.
}
\item{label.curves}{
applies if more than one \code{group} exists.
Default is \code{TRUE} to use \code{labcurve} to label curves where they are farthest
apart.  Set \code{label.curves} to a \code{list} to specify options to
\code{labcurve}, e.g., \code{label.curves=list(method="arrow", cex=.8)}.
These option names may be abbreviated in the usual way arguments
are abbreviated.  Use for example \code{label.curves=list(keys=1:5)}
to draw symbols periodically (as in \code{pch=1:5} - see \code{points})
on the curves and automatically position a legend
in the most empty part of the plot.  Set \code{label.curves=FALSE} to
suppress drawing curve labels.  The \code{col}, \code{lty}, and \code{type}
parameters are automatically passed to \code{labcurve}, although you
can override them here.  You can set \code{label.curves=list(keys="lines")} to
have different line types defined in an automatically positioned key.
}
\item{xlim}{
x-axis limits.  Default is entire range of \code{x}.
}
\item{subtitles}{
set to \code{FALSE} to suppress putting a subtitle at the bottom left of each
plot.  The subtitle indicates the numbers of
non-missing and missing observations, which are labeled \code{n}, \code{m}.
}
\item{datadensity}{
If \code{datadensity} is not \code{"none"}, either \code{scat1d} or \code{histSpike} is called to
add a rug plot (\code{datadensity="rug"}), spike histogram
(\code{datadensity="hist"}), or smooth density estimate (\code{"density"}) to
the bottom or top of the ECDF.
}
\item{side}{
If \code{datadensity} is not \code{"none"}, the default is to place the additional
information on top of the x-axis (\code{side=1}).  Use \code{side=3} to place at
the top of the graph.
}
\item{frac}{
passed to \code{histSpike}
}
\item{dens.opts}{
a list of optional arguments for \code{histSpike}
}
\item{...}{
other parameters passed to plot if add=F.  For data frames, other
parameters to pass to \code{Ecdf.default}.
For \code{Ecdf.formula}, if \code{groups} is not used, you can also add
data density information to each panel's ECDF by specifying the
\code{datadensity} and optional \code{frac}, \code{side},
\code{dens.opts} arguments. 
}
\item{n.unique}{
minimum number of unique values before an ECDF is drawn for a variable
in a data frame.  Default is 10.
}
\item{na.big}{
set to \code{TRUE} to draw the number of NAs in larger letters in the middle of
the plot for \code{Ecdf.data.frame}
}
\item{vnames}{
By default, variable labels are used to label x-axes.  Set \code{vnames="names"}
to instead use variable names.
}
\item{method}{
method for computing the empirical cumulative distribution.  See
\code{wtd.Ecdf}.  The default is to use the standard \code{"i/n"} method as is
used by the non-Trellis versions of \code{Ecdf}.
}
\item{fun}{
a function to transform the cumulative proportions, for the
Trellis-type usage of \code{Ecdf}
}
\item{data, groups, subset,prepanel, panel}{the usual Trellis/Lattice parameters, with \code{groups}
  causing \code{Ecdf.formula} to overlay multiple ECDFs on one panel.}
}
\value{
for \code{Ecdf.default} an invisible list with elements x and y giving the
coordinates of the cdf.  If there is more than one \code{group}, a list of
such lists is returned.  An attribute, \code{N}, is in the returned
object.  It contains the elements \code{n} and \code{m}, the number of
non-missing and missing observations, respectively.
}
\author{
Frank Harrell
\cr
Department of Biostatistics, Vanderbilt University
\cr
\email{fh@fharrell.com}
}
\section{Side Effects}{
plots
}
\seealso{
\code{\link{wtd.Ecdf}}, \code{\link{label}}, \code{\link{table}}, \code{\link{cumsum}}, \code{\link{labcurve}}, \code{\link{xYplot}}, \code{\link{histSpike}}
}
\examples{
set.seed(1)
ch <- rnorm(1000, 200, 40)
Ecdf(ch, xlab="Serum Cholesterol")
scat1d(ch)                       # add rug plot
histSpike(ch, add=TRUE, frac=.15)   # add spike histogram
# Better: add a data density display automatically:
Ecdf(ch, datadensity='density')


label(ch) <- "Serum Cholesterol"
Ecdf(ch)
other.ch <- rnorm(500, 220, 20)
Ecdf(other.ch,add=TRUE,lty=2)


sex <- factor(sample(c('female','male'), 1000, TRUE))
Ecdf(ch, q=c(.25,.5,.75))  # show quartiles
Ecdf(ch, group=sex,
     label.curves=list(method='arrow'))


# Example showing how to draw multiple ECDFs from paired data
pre.test <- rnorm(100,50,10)
post.test <- rnorm(100,55,10)
x <- c(pre.test, post.test)
g <- c(rep('Pre',length(pre.test)),rep('Post',length(post.test)))
Ecdf(x, group=g, xlab='Test Results', label.curves=list(keys=1:2))
# keys=1:2 causes symbols to be drawn periodically on top of curves


# Draw a matrix of ECDFs for a data frame
m <- data.frame(pre.test, post.test, 
                sex=sample(c('male','female'),100,TRUE))
Ecdf(m, group=m$sex, datadensity='rug')


freqs <- sample(1:10, 1000, TRUE)
Ecdf(ch, weights=freqs)  # weighted estimates


# Trellis/Lattice examples:


region <- factor(sample(c('Europe','USA','Australia'),100,TRUE))
year <- factor(sample(2001:2002,1000,TRUE))
Ecdf(~ch | region*year, groups=sex)
Key()           # draw a key for sex at the default location
# Key(locator(1)) # user-specified positioning of key
age <- rnorm(1000, 50, 10)
Ecdf(~ch | lattice::equal.count(age), groups=sex)  # use overlapping shingles
Ecdf(~ch | sex, datadensity='hist', side=3)  # add spike histogram at top
}
\keyword{nonparametric}
\keyword{hplot}
\keyword{methods}
\keyword{distribution}
\concept{trellis}
\concept{lattice}
