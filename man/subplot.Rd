\name{subplot}
\alias{subplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Embed a new plot within an existing plot}
\description{
  Subplot will embed a new plot within an existing plot at the
  coordinates specified (in user units of the existing plot).
}
\usage{
subplot(fun, x, y, size=c(1,1), vadj=0.5, hadj=0.5, pars=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{an expression or function defining the new plot to be embedded.}
  \item{x}{\code{x}-coordinate(s) of the new plot (in user coordinates
    of the existing plot).}
  \item{y}{\code{y}-coordinate(s) of the new plot, \code{x} and \code{y}
    can be specified in any of the ways understood by \code{xy.coords}.}
  \item{size}{The size of the embedded plot in inches if \code{x} and
    \code{y} have length 1.}
  \item{vadj}{vertical adjustment of the plot when \code{y} is a scalar,
    the default is to center vertically, 0 means place the bottom of the
    plot at \code{y}, 1 places the top of the plot at \code{y}.}
  \item{hadj}{horizontal adjustment of the plot when \code{x} is a
    scalar, the default is to center horizontally, 0 means place the
    left edge of the plot at \code{x}, and 1 means place the right edge
    of the plot at \code{x}.}
  \item{pars}{a list of parameters to be passed to \code{par} before
    running \code{fun}.}
}
\details{
  The coordinates \code{x} and \code{y} can be scalars or vectors of
  length 2.  If vectors of length 2 then they determine the opposite
  corners of the rectangle for the embedded plot (and the parameters
  \code{size}, \code{vadj}, and \code{hadj} are all ignored.

  If \code{x} and \code{y} are given as scalars then the plot position
  relative to the point and the size of the plot will be determined by
  the arguments \code{size}, \code{vadj}, and \code{hadj}.  The default
  is to center a 1 inch by 1 inch plot at \code{x,y}.  Setting
  \code{vadj} and \code{hadj} to \code{(0,0)} will position the lower
  left corner of the plot at \code{(x,y)}.

  The rectangle defined by \code{x}, \code{y}, \code{size}, \code{vadj},
  and \code{hadj} will be used as the plotting area of the new plot.
  Any tick marks, axis labels, main and sub titles will be outside of
  this rectangle.
  
  Any graphical parameter settings that you would like to be in place
  before \code{fun} is evaluated can be specified in the \code{pars}
  argument (warning: specifying layout parameters here (\code{plt},
  \code{mfrow}, etc.) may cause unexpected results).

  After the function completes the graphical parameters will have been
  reset to what they were before calling the function (so you can
  continue to augment the original plot).
}
\value{
 An invisible list with the graphical parameters that were in effect
 when the subplot was created.  Passing this list to \code{par} will
 enable you to augment the embedded plot.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Greg Snow \email{greg.snow@imail.org}}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{cnvrt.coords}}, \code{\link{par}}, \code{\link{symbols}}}
\examples{
# make an original plot
plot( 11:20, sample(51:60) )

# add some histograms

subplot( hist(rnorm(100)), 15, 55)
subplot( hist(runif(100),main='',xlab='',ylab=''), 11, 51, hadj=0, vadj=0)
subplot( hist(rexp(100, 1/3)), 20, 60, hadj=1, vadj=1, size=c(0.5,2) )
subplot( hist(rt(100,3)), c(12,16), c(57,59), pars=list(lwd=3,ask=FALSE) )

tmp <- rnorm(25)
qqnorm(tmp)
qqline(tmp)
tmp2 <- subplot( hist(tmp,xlab='',ylab='',main=''), 
		cnvrt.coords(0.1,0.9,'plt')$usr, vadj=1, hadj=0 )
abline(v=0, col='red') # wrong way to add a reference line to histogram

# right way to add a reference line to histogram
op <- par(no.readonly=TRUE)
par(tmp2)
abline(v=0, col='green')
par(op)


}
\keyword{aplot}% at least one, from doc/KEYWORDS
\keyword{dplot}

