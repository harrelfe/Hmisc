\name{bpplot}
\alias{bpplot}
\title{
Box-percentile plots
}
\description{
Producess side-by-side box-percentile plots from several vectors or a
list of vectors.  
}
\usage{
bpplot(\dots, name=TRUE, main="Box-Percentile Plot", 
       xlab="", ylab="", srtx=0, plotopts=NULL)
}
\arguments{
\item{...}{
vectors or lists containing 
numeric components (e.g., the output of \code{split}).
}
\item{name}{
character vector of names for the groups.  
Default is \code{TRUE} to put names on the x-axis.  Such names are taken from the 
data vectors or the \code{names} attribute of the first argument if it is a list.
Set \code{name} to \code{FALSE} to suppress names.
If a character vector is supplied the names in the vector are
used to label the groups.
}
\item{main}{
main title for the plot.
}
\item{xlab}{
x axis label.
}
\item{ylab}{
y axis label.
}
\item{srtx}{rotation angle for x-axis labels.  Default is zero.}
\item{plotopts}{a list of other parameters to send to \code{plot}}
}
\value{
There are no returned values
}
\section{Side Effects}{
A plot is created on the current graphics device.
}
\section{BACKGROUND}{
Box-percentile plots are similiar to boxplots, except box-percentile plots
supply more information about the univariate distributions.  At any height
the width of the irregular "box" is proportional to the percentile of that
height, up to the 50th percentile, and above the 50th percentile the width
is proportional to 100 minus the percentile.  Thus, the width at any given
height is proportional to the percent of observations that are more 
extreme in that direction.  As in boxplots, the median, 25th and 75th 
percentiles are marked with line segments across the box.
}
\author{
Jeffrey Banfield
\cr
\email{umsfjban@bill.oscs.montana.edu}
\cr
Modified by F. Harrell 30Jun97
}
\references{
Esty WW, Banfield J: The box-percentile plot.  J Statistical
Software 8 No. 17, 2003.
}
\seealso{
\code{\link{panel.bpplot}}, \code{\link{boxplot}}, \code{\link{Ecdf}},
\code{\link[lattice:xyplot]{bwplot}} 
}
\examples{
set.seed(1)
x1 <- rnorm(500)
x2 <- runif(500, -2, 2)
x3 <- abs(rnorm(500))-2
bpplot(x1, x2, x3)
g <- sample(1:2, 500, replace=TRUE)
bpplot(split(x2, g), name=c('Group 1','Group 2'))
rm(x1,x2,x3,g)
}
\keyword{nonparametric}
\keyword{hplot}
% Converted by Sd2Rd version 1.21.
