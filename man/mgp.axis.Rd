\name{mgp.axis}
\alias{mgp.axis}
\alias{mgp.axis.labels}
\title{Draw Axes With Side-Specific mgp Parameters}
\description{
\code{mgp.axis} is a version of \code{axis} that uses the appropriate
side-specific \code{mgp} parameter (see \code{\link{par}}) to account
for different space requirements for axis labels vertical vs. horizontal
tick marks.  \code{mgp.axis} also fixes a bug in \code{axis(2,\dots)}
that causes it to assume \code{las=1}.

\code{mgp.axis.labels} is used so that different spacing between tick
marks and axis tick mark labels may be specified for x- and y-axes.  Use
\code{mgp.axis.labels('default')} to set defaults. Users can set values
manually using \code{mgp.axis.labels(x,y)} where \code{x} and \code{y}
are 2nd value of \code{par('mgp')} to use.  Use
\code{mgp.axis.labels(type=w)} to retrieve values, where \code{w='x'},
\code{'y'}, \code{'x and y'}, \code{'xy'}, to get 3 \code{mgp} values
(first 3 types) or 2 \code{mgp.axis.labels}.
}
\usage{
mgp.axis(side, at = NULL, \dots,
         mgp = mgp.axis.labels(type = if (side == 1 | side == 3) "x"
                               else "y"),
         axistitle = NULL, cex.axis=par('cex.axis'), cex.lab=par('cex.lab'))

mgp.axis.labels(value,type=c('xy','x','y','x and y'))
}

\arguments{
  \item{side,at}{see \code{\link{par}}}
  \item{\dots}{arguments passed through to \code{\link{axis}}}
  \item{mgp,cex.axis,cex.lab}{see \code{\link{par}}}
  \item{axistitle}{if specified will cause \code{axistitle} to be drawn
	on the appropriate axis as a title}
  \item{value}{vector of values to which to set system option
	\code{mgp.axis.labels}}
  \item{type}{see above}
}
\section{Side Effects}{\code{mgp.axis.labels} stores the value in the
  system option \code{mgp.axis.labels}}
\value{
  \code{mgp.axis.labels} returns the value of \code{mgp} (only the
  second element of \code{mgp} if \code{type="xy"} or a list with
  elements \code{x} and \code{y} if \code{type="x or y"}, each list
  element being a 3-vector) for the 
  appropriate axis if \code{value} is not specified, otherwise it
  returns nothing but the system option \code{mgp.axis.labels} is set.

  \code{mgp.axis} returns nothing.
}
\author{Frank Harrell}
\seealso{\code{\link{par}}}
\examples{
\dontrun{
mgp.axis.labels(type='x')  # get default value for x-axis
mgp.axis.labels(type='y')  # get value for y-axis
mgp.axis.labels(type='xy') # get 2nd element of both mgps
mgp.axis.labels(type='x and y')  # get a list with 2 elements
mgp.axis.labels(c(3,.5,0), type='x')  # set
options('mgp.axis.labels')            # retrieve

plot(..., axes=FALSE)
mgp.axis(1, "X Label")
mgp.axis(2, "Y Label")

}}
\keyword{iplot}
\keyword{dplot}
\keyword{environment}
