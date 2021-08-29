\name{mtitle}
\alias{mtitle}
\title{
Margin Titles
}
\description{
Writes overall titles and subtitles after a multiple image plot is drawn.
If \code{par()$oma==c(0,0,0,0)}, \code{title} is used instead of \code{mtext}, to draw
titles or subtitles that are inside the plotting region for a single plot.
}
\usage{
mtitle(main, ll, lc,  
       lr=format(Sys.time(),'\%d\%b\%y'),
       cex.m=1.75, cex.l=.5, \dots)
}
\arguments{
\item{main}{
main title to be centered over entire figure, default is none
}
\item{ll}{
subtitle for lower left of figure, default is none
}
\item{lc}{
subtitle for lower center of figure, default is none
}
\item{lr}{
subtitle for lower right of figure, default is today's date in format
23Jan91 for UNIX or R (Thu May 30 09:08:13 1996 format for Windows). 
Set to \code{""} to suppress lower right title.
}
\item{cex.m}{
character size for main, default is 1.75
}
\item{cex.l}{
character size for subtitles
}
\item{...}{
other arguments passed to \code{mtext}
}}
\value{
nothing
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
\code{\link{par}}, \code{\link{mtext}}, \code{\link{title}}, \code{\link{unix}}, \code{\link{pstamp}}
}
\examples{
#Set up for 1 plot on figure, give a main title,
#use date for lr
plot(runif(20),runif(20))
mtitle("Main Title")


#Set up for 2 x 2 matrix of plots with a lower left subtitle and overall title
par(mfrow=c(2,2), oma=c(3,0,3,0))
plot(runif(20),runif(20))
plot(rnorm(20),rnorm(20))
plot(exp(rnorm(20)),exp(rnorm(20)))
mtitle("Main Title",ll="n=20")
}
\keyword{hplot}
% Converted by Sd2Rd version 1.21.
