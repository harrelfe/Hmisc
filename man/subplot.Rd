\name{subplot}
\alias{subplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Embed a new plot within an existing plot}
\description{
  Subplot will embed a new plot within an existing plot at the
  coordinates specified (in user units of the existing plot).
}
\usage{
subplot(fun, x, y, size=c(1,1), vadj=0.5, hadj=0.5,
 inset=c(0,0), type=c('plt','fig'), pars=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{an expression defining the new plot to be embedded.}
  \item{x}{\code{x}-coordinate(s) of the new plot (in user coordinates
    of the existing plot), or a character string.}
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
  \item{inset}{1 or 2 numbers representing the proportion of the plot to
  inset the subplot from edges when x is a character string.  The first
  element is the horizontal inset, the second is the vertical inset.}
  \item{type}{Character string, if 'plt' then the plotting region is
    defined by \code{x}, \code{y}, and \code{size} with axes,
    etc. outside that box; if 'fig' then all annotations are also inside
  the box.}
  \item{pars}{a list of parameters to be passed to \code{par} before
    running \code{fun}.}
}
\details{
  The coordinates \code{x} and \code{y} can be scalars or vectors of
  length 2.  If vectors of length 2 then they determine the opposite
  corners of the rectangle for the embedded plot (and the parameters
  \code{size}, \code{vadj}, and \code{hadj} are all ignored).

  If \code{x} and \code{y} are given as scalars then the plot position
  relative to the point and the size of the plot will be determined by
  the arguments \code{size}, \code{vadj}, and \code{hadj}.  The default
  is to center a 1 inch by 1 inch plot at \code{x,y}.  Setting
  \code{vadj} and \code{hadj} to \code{(0,0)} will position the lower
  left corner of the plot at \code{(x,y)}.

  If \code{x} is a character string, then it will be parsed for the
  strings "left", "right", "top", and "bottom" and x and y will be set
  appropriately (anything not specified will be set at the center in
  that dimension) using also the \code{inset} argument.  This allows the
  position of the subplot to be specified as 'topleft' or 'bottom', etc.
  The \code{inset} argument is in proportion of the plot units, so 0.1
  means inset 10\% of the width/height of the plotting distance.  If
  \code{hadj}/\code{vadj} are not specified, they will be set appropriately.

  The rectangle defined by \code{x}, \code{y}, \code{size}, \code{vadj},
  and \code{hadj} will be used as the plotting area of the new plot.
  Any tick marks, axis labels, main and sub titles will be outside of
  this rectangle if \code{type} is 'plt'.  If type is 'fig' then the
  annotations will be inside the box.

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

\seealso{\code{\link{grconvertX}}, \code{\link{par}},
  \code{\link{symbols}}, \code{\link{my.symbols}}}
\examples{
# make an original plot
plot( 11:20, sample(51:60) )

# add some histograms

subplot( hist(rnorm(100)), 15, 55)
subplot( hist(runif(100),main='',xlab='',ylab=''), 11, 51, hadj=0, vadj=0)
subplot( hist(rexp(100, 1/3)), 20, 60, hadj=1, vadj=1, size=c(0.5,2) )
subplot( hist(rt(100,3)), c(12,16), c(57,59), pars=list(lwd=3,ask=FALSE) )

### some of the following examples work fine in an interactive session,
### but loading the packages required does not work well in testing.

# augment a map
if( interactive() && require(maptools) ){
	plot(state.vbm,fg=NULL)
	tmp <- cbind( state.vbm$center_x, state.vbm$center_y )
	for( i in 1:50 ){
		tmp2 <- as.matrix(USArrests[i,c(1,4)])
		tmp3 <- max(USArrests[,c(1,4)])
		subplot( barplot(tmp2, ylim=c(0,tmp3),names=c('',''),yaxt='n'),
			x=tmp[i,1], y=tmp[i,2], size=c(.1,.1))
	}
}

tmp <- rnorm(25)
qqnorm(tmp)
qqline(tmp)
tmp2 <- subplot( hist(tmp,xlab='',ylab='',main=''),
		grconvertX(0.1,from='npc'), grconvertY(0.9,from='npc'), 
		vadj=1, hadj=0 )
abline(v=0, col='red') # wrong way to add a reference line to histogram

# right way to add a reference line to histogram
op <- par(no.readonly=TRUE)
par(tmp2)
abline(v=0, col='green')
par(op)


# scatter-plot using images
if(interactive() && require(EBImage)) {

  image.EBImage <- function(x,...) {
    tmp <- imageData(flip(x))
    cols <- rgb( tmp[,,1], tmp[,,2], tmp[,,3] )
    z <- 1:length(cols)
    dim(z) <- dim(tmp[,,1])
    image(z, col=cols, axes=FALSE, ...)
  }

  logo <- readImage( paste( R.home('doc'), '/html/logo.jpg', sep='' ) )

  x <- runif(10)
  y <- runif(10)
  plot(x,y, type='n')
  for(i in 1:10) {
    subplot(image.EBImage(logo), x[i], y[i], size=c(0.3,0.3))
  }
}

}
\keyword{aplot}% at least one, from doc/KEYWORDS
\keyword{dplot}

