\name{cnvrt.coords}
\alias{cnvrt.coords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert between the 4 different coordinate sytems on a graphical device}
\description{
  Takes a set of coordinates in any of the 4 coordinate systems (usr,
  plt, fig, or dev) and returns the same points in all 4 coordinate systems.
}
\usage{
cnvrt.coords(x, y = NULL, input = c("usr", "plt", "fig", "dev"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector, Matrix, or list of x coordinates (or x and y
    coordinates), NA's allowed. }
  \item{y}{y coordinates (if \code{x} is a vector), NA's allowed. }
  \item{input}{Character scalar indicating the coordinate system of the
    input points. }
}
\details{
  Every plot has 4 coordinate systems:

  usr (User): the coordinate system of the data, this is shown by the
  tick marks and axis labels.

  plt (Plot): Plot area, coordinates range from 0 to 1 with 0
  corresponding to the x and y axes and 1 corresponding to the top and
  right of the plot area.  Margins of the plot correspond to plot
  coordinates less than 0 or greater than 1.

  fig (Figure): Figure area, coordinates range from 0 to 1 with 0
  corresponding to the bottom and left edges of the figure (including
  margins, label areas) and 1 corresponds to the top and right edges.
  fig and dev coordinates will be identical if there is only 1 figure
  area on the device (layout, mfrow, or mfcol has not been used).

  dev (Device): Device area, coordinates range from 0 to 1 with 0
  corresponding to the bottom and left edges of the device (piece of
  paper, window on screen) and 1 corresponds to the top and right edges.
}
\value{
  A list with 4 components, each component is a list with vectors named
  x and y.  The 4 sublists are:
  \item{usr}{The coordinates of the input points in usr (User) coordinates.}
  \item{plt}{The coordinates of the input points in plt (Plot)
    coordinates.}
  \item{fig}{The coordinates of the input points in fig (Figure)
    coordinates.}
  \item{dev}{The coordinates of the input points in dev (Device) coordinates.}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Greg Snow \email{greg.snow@ihc.com}}
\note{ You must provide both x and y, but one of the may be \code{NA}. }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{par}} specifically 'usr','plt', and 'fig'.  Also
  'xpd' for plotting outside of the plotting region and 'mfrow' and
  'mfcol' for multi figure plotting.}
\examples{

old.par <- par(no.readonly=TRUE)

par(mfrow=c(2,2),xpd=NA)

# generate some sample data
tmp.x <- rnorm(25, 10, 2)
tmp.y <- rnorm(25, 50, 10)
tmp.z <- rnorm(25, 0, 1)

plot( tmp.x, tmp.y)

# draw a diagonal line across the plot area
tmp1 <- cnvrt.coords( c(0,1), c(0,1), input='plt' )
lines(tmp1$usr, col='blue')

# draw a diagonal line accross figure region
tmp2 <- cnvrt.coords( c(0,1), c(1,0), input='fig')
lines(tmp2$usr, col='red')

# save coordinate of point 1 and y value near top of plot for future plots
tmp.point1 <- cnvrt.coords(tmp.x[1], tmp.y[1])
tmp.range1 <- cnvrt.coords(NA, 0.98, input='plt')

# make a second plot and draw a line linking point 1 in each plot
plot(tmp.y, tmp.z)

tmp.point2 <- cnvrt.coords( tmp.point1$dev, input='dev' )
arrows( tmp.y[1], tmp.z[1], tmp.point2$usr$x, tmp.point2$usr$y,
 col='green')

# draw another plot and add rectangle showing same range in 2 plots

plot(tmp.x, tmp.z)
tmp.range2 <- cnvrt.coords(NA, 0.02, input='plt')
tmp.range3 <- cnvrt.coords(NA, tmp.range1$dev$y, input='dev')
rect( 9, tmp.range2$usr$y, 11, tmp.range3$usr$y, border='yellow')

# put a label just to the right of the plot and 
#  near the top of the figure region.
text( cnvrt.coords(1.05, NA, input='plt')$usr$x,
	cnvrt.coords(NA, 0.75, input='fig')$usr$y,
	"Label", adj=0)

par(mfrow=c(1,1))

## create a subplot within another plot

plot(1:10, 1:10)

tmp <- cnvrt.coords( c( 1, 4, 6, 9), c(6, 9, 1, 4) )

par(plt = c(tmp$dev$x[1:2], tmp$dev$y[1:2]), new=TRUE)
hist(rnorm(100))

par(fig = c(tmp$dev$x[3:4], tmp$dev$y[3:4]), new=TRUE)
hist(rnorm(100))

par(old.par)




## The function is currently defined as
function(x,y=NULL,input=c('usr','plt','fig','dev')) {

  input <- match.arg(input)
  xy <- xy.coords(x,y)
  
  cusr <- par('usr')
  cplt <- par('plt')
  cfig <- par('fig')
  
  if(input=='usr'){
    usr <- xy
    
    plt <- list()
    plt$x <- (xy$x-cusr[1])/(cusr[2]-cusr[1])
    plt$y <- (xy$y-cusr[3])/(cusr[4]-cusr[3])

    fig <- list()
    fig$x <- plt$x*(cplt[2]-cplt[1])+cplt[1]
    fig$y <- plt$y*(cplt[4]-cplt[3])+cplt[3]

    dev <- list()
    dev$x <- fig$x*(cfig[2]-cfig[1])+cfig[1]
    dev$y <- fig$y*(cfig[4]-cfig[3])+cfig[3]

    return( list( usr=usr, plt=plt, fig=fig, dev=dev ) )
  }

  if(input=='plt') {

    plt <- xy

    usr <- list()
    usr$x <- plt$x*(cusr[2]-cusr[1])+cusr[1]
    usr$y <- plt$y*(cusr[4]-cusr[3])+cusr[3]

    fig <- list()
    fig$x <- plt$x*(cplt[2]-cplt[1])+cplt[1]
    fig$y <- plt$y*(cplt[4]-cplt[3])+cplt[3]

    dev <- list()
    dev$x <- fig$x*(cfig[2]-cfig[1])+cfig[1]
    dev$y <- fig$y*(cfig[4]-cfig[3])+cfig[3]

    return( list( usr=usr, plt=plt, fig=fig, dev=dev ) )
  }

  if(input=='fig') {

    fig <- xy

    plt <- list()
    plt$x <- (fig$x-cplt[1])/(cplt[2]-cplt[1])
    plt$y <- (fig$y-cplt[3])/(cplt[4]-cplt[3])

    usr <- list()
    usr$x <- plt$x*(cusr[2]-cusr[1])+cusr[1]
    usr$y <- plt$y*(cusr[4]-cusr[3])+cusr[3]

    dev <- list()
    dev$x <- fig$x*(cfig[2]-cfig[1])+cfig[1]
    dev$y <- fig$y*(cfig[4]-cfig[3])+cfig[3]
    
    return( list( usr=usr, plt=plt, fig=fig, dev=dev ) )
  }

  dev <- xy

  fig <- list()
  fig$x <- (dev$x-cfig[1])/(cfig[2]-cfig[1])
  fig$y <- (dev$y-cfig[3])/(cfig[4]-cfig[3])

  plt <- list()
  plt$x <- (fig$x-cplt[1])/(cplt[2]-cplt[1])
  plt$y <- (fig$y-cplt[3])/(cplt[4]-cplt[3])

  usr <- list()
  usr$x <- plt$x*(cusr[2]-cusr[1])+cusr[1]
  usr$y <- plt$y*(cusr[4]-cusr[3])+cusr[3]

  return( list( usr=usr, plt=plt, fig=fig, dev=dev ) )

  }
}
\keyword{ dplot }% at least one, from doc/KEYWORDS
\keyword{ aplot }% __ONLY ONE__ keyword per line
