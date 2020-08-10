\name{summaryRc}
\alias{summaryRc}
\title{Graphical Summarization of Continuous Variables Against a Response}
\description{
	\code{summaryRc} is a continuous version of \code{\link{summary.formula}}
	with \code{method='response'}.  It uses the \code{\link{plsmo}}
	function to compute the possibly stratified \code{\link{lowess}}
	nonparametric regression estimates, and plots them along with the data
	density, with selected quantiles of the overall distribution (over
	strata) of each \code{x} shown as arrows on top of the graph.  All the
	\code{x} variables must be numeric and continuous or nearly continuous.
}
\usage{
summaryRc(formula, data=NULL, subset=NULL,
          na.action=NULL, fun = function(x) x,
          na.rm = TRUE, ylab=NULL, ylim=NULL, xlim=NULL,
          nloc=NULL, datadensity=NULL,
          quant = c(0.05, 0.1, 0.25, 0.5, 0.75,
                    0.90, 0.95), quantloc=c('top','bottom'),
          cex.quant=.6, srt.quant=0,
          bpplot = c('none', 'top', 'top outside', 'top inside', 'bottom'),
          height.bpplot=0.08,
          trim=NULL, test = FALSE, vnames = c('labels', 'names'), \dots)
}
\arguments{
  \item{formula}{
    An \R formula with additive effects.  The \code{formula} may contain
		one or more invocations of the \code{stratify} function whose
		arguments are defined below.  This causes 
    the entire analysis to be stratified by cross-classifications of the
    combined list of stratification factors.  This stratification will be
    reflected as separate \code{lowess} curves.}
  \item{data}{
    name or number of a data frame.  Default is the current frame.
  }
  \item{subset}{
    a logical vector or integer vector of subscripts used to specify the
    subset of data to use in the analysis.  The default is to use all
    observations in the data frame.
  }
  \item{na.action}{
    function for handling missing data in the input data.  The default is
    a function defined here called \code{na.retain}, which keeps all
    observations for processing, with missing variables or not.
  }
  \item{fun}{
    function for transforming \code{lowess} estimates.  Default is the
		identity function.}
  \item{na.rm}{
    \code{TRUE} (the default) to exclude \code{NA}s before passing data to
    \code{fun} to compute statistics, \code{FALSE} otherwise.
  }
	\item{ylab}{\code{y}-axis label.  Default is label attribute of
		\code{y} variable, or its name.}
	\item{ylim}{\code{y}-axis limits.  By default each graph is scaled on
		its own.}
	\item{xlim}{a list with elements named as the variable names appearing
		on the \code{x}-axis, with each element being a 2-vector specifying
		lower and upper limits.  Any variable not appearing in the list will
		have its limits computed and possibly \code{trim}med.}
	\item{nloc}{location for sample size.  Specify \code{nloc=FALSE} to
		suppress, or \code{nloc=list(x=,y=)} where \code{x,y} are relative
		coordinates in the data window.  Default position is in the largest
		empty space.}
	\item{datadensity}{see \code{\link{plsmo}}.  Defaults to \code{TRUE}
		if there is a \code{stratify} variable, \code{FALSE} otherwise.}
  \item{quant}{
    vector of quantiles to use for summarizing the marginal distribution
		of each \code{x}. This must be numbers between 0 and 1
    inclusive.  Use \code{NULL} to omit quantiles.
  }
	\item{quantloc}{specify \code{quantloc='bottom'} to place at the
		bottom of each plot rather than the default}
	\item{cex.quant}{character size for writing which quantiles are
		represented.  Set to \code{0} to suppress quantile labels.}
	\item{srt.quant}{angle for text for quantile labels}
	\item{bpplot}{if not \code{'none'} will draw extended box plot at
		location given by \code{bpplot}, and quantiles discussed above will
		be suppressed.  Specifying \code{bpplot='top'} is the same as
		specifying \code{bpplot='top inside'}.}
	\item{height.bpplot}{height in inches of the horizontal extended box plot}
	\item{trim}{The default is to plot from the 10th smallest to the 10th
		largest \code{x} if the number of non-NAs exceeds 200, otherwise to
		use the entire range of \code{x}.  Specify another quantile to use
		other limits, e.g.,  \code{trim=0.01} will use the first and last
		percentiles}
  \item{test}{
    Set to \code{TRUE} to plot test statistics (not yet implemented).
  }
  \item{vnames}{
    By default, plots are usually labeled with variable labels
    (see the \code{label} and \code{sas.get} functions).  To use the shorter
    variable names, specify \code{vnames="names"}.
  }
  \item{...}{arguments passed to \code{\link{plsmo}}}
}
\value{no value is returned}
\author{
  Frank Harrell\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{fh@fharrell.com}
}
\seealso{
  \code{\link{plsmo}}, \code{\link{stratify}},
  \code{\link{label}}, \code{\link{formula}}, \code{\link{panel.bpplot}} 
}
\examples{
options(digits=3)
set.seed(177)
sex <- factor(sample(c("m","f"), 500, rep=TRUE))
age <- rnorm(500, 50, 5)
bp  <- rnorm(500, 120, 7)
units(age) <- 'Years'; units(bp) <- 'mmHg'
label(bp) <- 'Systolic Blood Pressure'
L <- .5*(sex == 'm') + 0.1 * (age - 50)
y <- rbinom(500, 1, plogis(L))
par(mfrow=c(1,2))
summaryRc(y ~ age + bp)
# For x limits use 1st and 99th percentiles to frame extended box plots
summaryRc(y ~ age + bp, bpplot='top', datadensity=FALSE, trim=.01)
summaryRc(y ~ age + bp + stratify(sex),
          label.curves=list(keys='lines'), nloc=list(x=.1, y=.05))
y2 <- rbinom(500, 1, plogis(L + .5))
Y <- cbind(y, y2)
summaryRc(Y ~ age + bp + stratify(sex),
          label.curves=list(keys='lines'), nloc=list(x=.1, y=.05))
}
\keyword{hplot}



