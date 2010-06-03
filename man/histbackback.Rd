\name{histbackback}
\alias{histbackback}
\title{
Back to Back Histograms
}
\description{
Takes two vectors or a list with \code{x} and \code{y} components, and produces 
back to back histograms of the two datasets.
}
\usage{
histbackback(x, y, brks=NULL, xlab=NULL, axes=TRUE, probability=FALSE,
             xlim=NULL, ylab='', \dots)
}
\arguments{
\item{x,y}{
either two vectors or a list given as \code{x} with two components.  If the
components have names, they will be used to label the axis
(modification FEH).
}
\item{brks}{
vector of the desired breakpoints for the histograms.
}
\item{xlab}{
a vector of two character strings naming the two datasets.
}
\item{axes}{
logical flag stating whether or not to label the axes.
}
\item{probability}{
logical flag: if \code{TRUE}, then the x-axis corresponds to the units for a
density.  If \code{FALSE}, then the units are counts.
}
\item{xlim}{
x-axis limits.  First value must be negative, as the left histogram is
placed at negative x-values.  Second value must be positive, for the
right histogram.  To make the limits symmetric, use e.g. \code{ylim=c(-20,20)}.
}
\item{ylab}{
label for y-axis.  Default is no label.
}
\item{...}{
additional graphics parameters may be given.
}}
\value{
a list is returned invisibly with the following components:

\item{left}{
the counts for the dataset plotted on the left.
}
\item{right}{
the counts for the dataset plotted on the right.
}
\item{breaks}{
the breakpoints used.
}}
\section{Side Effects}{
a plot is produced on the current graphics device.
}
\author{
Pat Burns
\cr
Salomon Smith Barney
\cr
London
\cr
\email{pburns@dorado.sbi.com}
}
\seealso{
\code{\link{hist}}, \code{\link[lattice]{histogram}}
}
\examples{
options(digits=3)
set.seed(1)
histbackback(rnorm(20), rnorm(30))


fool <- list(x=rnorm(40), y=rnorm(40))
histbackback(fool)
age <- rnorm(1000,50,10)
sex <- sample(c('female','male'),1000,TRUE)
histbackback(split(age, sex))
agef <- age[sex=='female']; agem <- age[sex=='male']
histbackback(list(Female=agef,Male=agem), probability=TRUE, xlim=c(-.06,.06))
}
\keyword{dplot}
\keyword{hplot}
\keyword{distribution}
% Converted by Sd2Rd version 1.21.
