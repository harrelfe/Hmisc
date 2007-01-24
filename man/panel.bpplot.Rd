\name{panel.bpplot}
\alias{panel.bpplot}
\alias{bpplt}
\title{
Box-Percentile Panel Function for Trellis
}
\description{
For all their good points, box plots have a high ink/information ratio
in that they mainly display 3 quartiles.  Many practitioners have
found that the "outer values" are difficult to explain to
non-statisticians and many feel that the notion of "outliers" is too
dependent on (false) expectations that data distributions should be Gaussian.

\code{panel.bpplot} is a \code{panel} function for use with \code{trellis}, especially for
\code{bwplot}.  It draws box plots (without the whiskers) with any number
of user-specified "corners" (corresponding to different quantiles),
but it also draws box-percentile plots similar to those drawn by
Jeffrey Banfield's (umsfjban@bill.oscs.montana.edu) \code{bpplot} function.
To quote from Banfield, "box-percentile plots supply more
information about the univariate distributions.  At any height the
width of the irregular 'box' is proportional to the percentile of that
height, up to the 50th percentile, and above the 50th percentile the
width is proportional to 100 minus the percentile.  Thus, the width at
any given height is proportional to the percent of observations that
are more extreme in that direction.  As in boxplots, the median, 25th
and 75th percentiles are marked with line segments across the box."

\code{panel.bpplot} is a generalization of \code{bpplot} and
\code{\link[lattice]{panel.bwplot}} in 
that it works with \code{trellis} (making the plots horizontal so that
category labels are more visable), it allows the user to specify the
quantiles to connect and those for which to draw reference lines, 
and it displays means (by default using dots).

\code{bpplt} draws horizontal box-percentile plot much like those drawn
by \code{panel.bpplot} but taking as the starting point a matrix
containing quantiles summarizing the data.  \code{bpplt} is primarily
intended to be used internally by \code{plot.summary.formula.reverse}
but when used with no arguments has a general purpose: to draw an
annotated example box-percentile plot with the default quantiles used
and with the mean drawn with a solid dot.  This schematic plot is
rendered nicely in postscript with an image height of 3.5 inches.
}
\usage{
panel.bpplot(x, y, box.ratio=1, means=TRUE, qref=c(.5,.25,.75),
             probs=c(.05,.125,.25,.375), nout=0,
             datadensity=FALSE, scat1d.opts=NULL,
             font=box.dot$font, pch=box.dot$pch, 
             cex =box.dot$cex,  col=box.dot$col, \dots)

# E.g. bwplot(formula, panel=panel.bpplot, panel.bpplot.parameters)

bpplt(stats, xlim, xlab='', box.ratio = 1, means=TRUE,
      qref=c(.5,.25,.75), qomit=c(.025,.975),
      pch=16, cex.labels=par('cex'), cex.points=if(prototype)1 else 0.5,
      grid=FALSE)
}
\arguments{
\item{x}{
continuous variable whose distribution is to be examined
}
\item{y}{
grouping variable
}
\item{box.ratio}{
see \code{\link[lattice]{panel.bwplot}}
}
\item{means}{
set to \code{FALSE} to suppress drawing a character at the mean value
}
\item{qref}{
vector of quantiles for which to draw reference lines.  These do not
need to be included in \code{probs}.
}
\item{probs}{
vector of quantiles to display in the box plot.  These should all be
less than 0.5; the mirror-image quantiles are added automatically.  By
default, \code{probs} is set to \code{c(.05,.125,.25,.375)} so that intervals
contain 0.9, 0.75, 0.5, and 0.25 of the data.
To draw all 99 percentiles, i.e., to draw a box-percentile plot,
set \code{probs=seq(.01,.49,by=.01)}.
To make a more traditional box plot, use \code{probs=.25}.
}
\item{nout}{
tells the function to use \code{scat1d} to draw tick marks showing the
\code{nout} smallest and \code{nout} largest values if \code{nout >= 1}, or to
show all values less than the \code{nout} quantile or greater than the
\code{1-nout} quantile if \code{0 < nout <= 0.5}.  If \code{nout} is a whole number,
only the first \code{n/2} observations are shown on either side of the
median, where \code{n} is the total number of observations. 
}
\item{datadensity}{
set to \code{FALSE} to invoke \code{scat1d} to draw a data density (one-dimensional
scatter diagram or rug plot) inside each box plot.
}
\item{scat1d.opts}{
a list containing named arguments (without abbreviations) to pass to
\code{scat1d} when \code{datadensity=TRUE} or \code{nout > 0}
}
\item{font}{}
\item{pch}{}
\item{cex}{}
\item{col}{see \code{\link[lattice]{panel.bwplot}}}
\item{\dots}{arguments passed to \code{points}}
\item{stats}{}
\item{xlim}{}
\item{xlab}{}
\item{qomit}{}
\item{cex.labels}{}
\item{cex.points}{}
\item{grid}{undocumented arguments to \code{bpplt}}
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University School of Medicine
\cr
f.harrell@vanderbilt.edu
}
\references{
Esty, W. W. and Banfield, J. D. (1992)
"The Box-Percentile Plot,"
Technical Report (May 15, 1992),
Department of Mathematical Sciences,
Montana State University.
}
\seealso{
\code{\link{bpplot}}, \code{\link[lattice]{panel.bwplot}}, \code{\link{scat1d}}, \code{\link{quantile}}, \code{\link{Ecdf}}
}
\examples{
set.seed(13)
x <- rnorm(1000)
g <- sample(1:6, 1000, replace=TRUE)
x[g==1][1:20] <- rnorm(20)+3   # contaminate 20 x's for group 1


# default trellis box plot
require(lattice)
bwplot(g ~ x)


# box-percentile plot with data density (rug plot)
bwplot(g ~ x, panel=panel.bpplot, probs=seq(.01,.49,by=.01), datadensity=TRUE)
# add ,scat1d.opts=list(tfrac=1) to make all tick marks the same size
# when a group has > 125 observations


# small dot for means, show only .05,.125,.25,.375,.625,.75,.875,.95 quantiles
bwplot(g ~ x, panel=panel.bpplot, cex=.3)


# suppress means and reference lines for lower and upper quartiles
bwplot(g ~ x, panel=panel.bpplot, probs=c(.025,.1,.25), means=FALSE, qref=FALSE)


# continuous plot up until quartiles ("Tootsie Roll plot")
bwplot(g ~ x, panel=panel.bpplot, probs=seq(.01,.25,by=.01))


# start at quartiles then make it continuous ("coffin plot")
bwplot(g ~ x, panel=panel.bpplot, probs=seq(.25,.49,by=.01))


# same as previous but add a spike to give 0.95 interval
bwplot(g ~ x, panel=panel.bpplot, probs=c(.025,seq(.25,.49,by=.01)))


# decile plot with reference lines at outer quintiles and median
bwplot(g ~ x, panel=panel.bpplot, probs=c(.1,.2,.3,.4), qref=c(.5,.2,.8))


# default plot with tick marks showing all observations outside the outer
# box (.05 and .95 quantiles), with very small ticks
bwplot(g ~ x, panel=panel.bpplot, nout=.05, scat1d.opts=list(frac=.01))


# show 5 smallest and 5 largest observations
bwplot(g ~ x, panel=panel.bpplot, nout=5)


# Use a scat1d option (preserve=TRUE) to ensure that the right peak extends 
# to the same position as the extreme scat1d
bwplot(~x , panel=panel.bpplot, probs=seq(.00,.5,by=.001), 
       datadensity=TRUE, scat1d.opt=list(preserve=TRUE))

# Draw a prototype showing how to interpret the plots
bpplt()

# make a local copy of bwplot that always uses panel.bpplot (S-Plus only)
# bwplot$panel <- panel.bpplot
# bwplot(g ~ x, nout=.05)
}
\keyword{nonparametric}
\keyword{hplot}
\keyword{distribution}
\concept{trellis}
\concept{lattice}
