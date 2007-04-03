\name{dotchart2}
\alias{dotchart2}
\title{
Enhanced Dot Chart
}
\description{
\code{dotchart2} is an enhanced version of the \code{dotchart} function 
with several new options.
}
\usage{
dotchart2(data, labels, groups=NULL, gdata=NA, horizontal=TRUE, pch=16,
          xlab='', ylab='', auxdata, auxgdata=NULL, auxtitle,
          lty=if(.R.) 1 else 2, lines=TRUE, dotsize = .8,
          cex = par("cex"), cex.labels = cex,
          cex.group.labels = cex.labels*1.25, sort.=TRUE, 
	      add=FALSE, dotfont=par('font'), groupfont=2, 
	      reset.par=add, xaxis=TRUE, width.factor=1.1,
          lcolor=if(.R.)'gray' else par('col'), ...)
}
\arguments{
  \item{data}{a numeric vector whose values are shown on the x-axis}
  \item{labels}{a vector of labels for each point, corresponding to
	\code{x}.  If omitted, \code{names(data)} are used, and if there are
	no \code{names}, integers prefixed by \code{"#"} are used.}
  \item{groups}{an optional categorical variable indicating how
	\code{data} values are grouped}
  \item{gdata}{data values for groups, typically summaries such as group
	medians}
  \item{horizontal}{set to \code{FALSE} to make the chart vertical
	instead of the default}
  \item{pch}{
	default character number or value for plotting dots in dot charts.
	The default is 16.}
  \item{xlab}{x-axis title}
  \item{ylab}{y-axis title}
  \item{auxdata}{
	a vector of auxiliary data given to \code{dotchart2}, of the same length
	as the first (\code{data}) argument.  If present, this
	vector of values will be printed outside the right margin of the dot
	chart.  Usually \code{auxdata} represents cell sizes.
  }
  \item{auxgdata}{
	similar to \code{auxdata} but corresponding to the \code{gdata}
	argument.  These usually represent overall sample sizes for each
	group of lines.}
  \item{auxtitle}{
	if \code{auxdata} is given, \code{auxtitle} specifies a column
	heading for the extra printed data in the chart, e.g., \code{"N"}}
  \item{lty}{line type for horizontal lines.  Default is 1 for R, 2 for S-Plus}
  \item{lines}{set to \code{FALSE} to suppress drawing of reference
	lines}
  \item{dotsize}{
	\code{cex} value for drawing dots.  Default is 0.8.  Note that the original
	\code{dotchart} function used a default of 1.2.}
  \item{cex}{see \code{\link{par}}}
  \item{cex.labels}{
	\code{cex} parameter that applies only to the line labels for the
	dot chart \code{cex} parameter for major grouping labels for
	\code{dotchart2}.  Defaults to \code{cex}.}
  \item{cex.group.labels}{value of \code{cex} corresponding to \code{gdata}}
  \item{sort.}{
	set to \code{FALSE} to keep \code{dotchart2} from sorting the input
	data, i.e., it will assume that the data are already properly
	arranged.  This is especially useful when you are using \code{gdata}
	and \code{groups} and you want to control the
	order that groups appear on the chart (from top to bottom).}
  \item{add}{set to \code{TRUE} to add to an existing plot}
  \item{dotfont}{
	font number of plotting dots.  Default is one.  Use \code{-1} to
	use "outline" fonts.  For example, \code{pch=183, dotfont=-1}
	plots an open circle for UNIX on postscript.  \code{pch=1} makes
	an open octagon under Windows.}
  \item{groupfont}{
	font number to use in drawing \code{group} labels for \code{dotchart2}.
	Default is \code{2} for boldface.
  }
  \item{reset.par}{
	set to \code{FALSE} to cause \code{dotchart2} to not reset the \code{par}
	parameters when finished.  This is useful when \code{add=TRUE} is about to
	be used in another call.  The default is to reset the \code{par}
	parameters if \code{add=TRUE} and not if \code{add=FALSE}, i.e., the
	program assumes that only one set of points will be added to an
	existing set.  If you fail to use \code{reset.par=TRUE} for the 
	first of a series of plots, the next call to \code{plot} with
	\code{add=TRUE} will result in distorted x-axis scaling.}
  \item{xaxis}{set to \code{FALSE} to suppress drawing x-axis}
  \item{width.factor}{
	When the calculated left margin turns out to be faulty, specify a
	factor by which to multiple the left margin as \code{width.factor} to get
	the appropriate space for labels on horizonal charts.}
  \item{lcolor}{
	color for horizontal reference lines.  Default is \code{"gray"} for R,
	\code{par("col")} for S-Plus.}
  \item{...}{arguments passed to \code{plot.default}}
}
\section{Side Effects}{
\code{dotchart} will leave \code{par} altered if \code{reset.par=FALSE}.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
f.harrell@vanderbilt.edu
}
\seealso{
\code{\link{dotchart}}
}
\keyword{hplot}
