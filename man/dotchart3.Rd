\name{dotchart3}
\alias{dotchart3}
\title{Enhanced Version of dotchart Function}
\description{
This is an adaptation of the R dotchart function that sorts categories
top to bottom, adds \code{auxdata} and \code{auxtitle} arguments to put
extra information in the right margin, and adds arguments \code{cex.labels},
\code{cex.group.labels}, and \code{groupfont}.  By default, group
headings are in a larger, bold font.
\code{dotchart3} also cuts a bit of white space from the top and bottom
of the chart.  The most significant change, however, is in how \code{x}
is interpreted.  Columns of \code{x} no longer provide an alternate way
to define groups.  Instead, they define superpositioned values.  This is
useful for showing three quartiles, for example.  Going along with this
change, \code{pch} can now be a vector specifying symbols to use going
across columns of \code{x}.  \code{x} was changed in this way because to
put multiple points on a line (e.g., quartiles) and keeping track of
\code{par()} parameters when \code{dotchart2} was called
with \code{add=TRUE} was cumbersome.  All the dotchart functions change
the margins to account for horizontal labels.
}
\usage{
dotchart3(x, labels = NULL, groups = NULL, gdata = NULL,
          cex = par("cex"), pch = 21, gpch = 21, bg = par("bg"),
          color = par("fg"), gcolor = par("fg"), lcolor = "gray",
          xlim = range(x[is.finite(x)]), main = NULL, xlab = NULL,
          ylab = NULL, auxdata = NULL, auxtitle = NULL,
          cex.labels = cex, cex.group.labels = cex.labels * 1.25,
          groupfont = 2, ...)
}
\arguments{
  \item{x}{a numeric vector or matrix}
  \item{labels}{labels for categories corresponding to rows of
	\code{x}.  If not specified these are taken from row names of \code{x}.}
  \item{groups,gdata,cex,pch,gpch,bg,color,gcolor,lcolor,xlim,main,xlab,ylab}{see \code{\link{dotchart}}}
  \item{auxdata}{a vector of information to be put in the right margin,
	in the same order as \code{x}.  May be numeric, character, or a
	vector of expressions containing \code{\link{plotmath}} markup}
  \item{auxtitle}{a column heading for \code{auxdata}}
  \item{cex.labels}{\code{cex} for labels}
  \item{cex.group.labels}{\code{cex} for group labels}
  \item{groupfont}{font number for group headings}
  \item{\dots}{other arguments passed to some of the graphics functions}
}
\value{the function returns invisibly}
\author{Frank Harrell}
\seealso{\code{\link{dotchart}},\code{\link{dotchart2}}}
\examples{
set.seed(135)
maj <- factor(c(rep('North',13),rep('South',13)))
g <- paste('Category',rep(letters[1:13],2))
n <- sample(1:15000, 26, replace=TRUE)
y1 <- runif(26)
y2 <- pmax(0, y1 - runif(26, 0, .1))
dotchart3(cbind(y1,y2), g, groups=maj, auxdata=n, auxtitle='n',
          xlab='Y', pch=c(1,17))
## Compare with dotchart function (no superpositioning or auxdata allowed):
## dotchart(y1, g, groups=maj, xlab='Y')
}
\keyword{hplot}
