\name{latexDotchart}
\alias{latexDotchart}
\title{Enhanced Dot Chart for LaTeX Picture Environment with epic}
\description{
\code{latexDotchart}  is a translation of the \code{dotchart3} function
for producing a vector of character strings containing LaTeX picture
environment markup that mimics \code{dotchart3} output.  The LaTeX
\code{epic} and \code{color} packages are required.  The \code{add} and
\code{horizontal=FALSE} options are not available for
\code{latexDotchart}, however. 
}
\usage{
latexDotchart(data, labels, groups=NULL, gdata=NA, 
  xlab='', auxdata, auxgdata=NULL, auxtitle,
  w=4, h=4, margin,      
  lines=TRUE, dotsize = .075, size='small', size.labels='small',
  size.group.labels='normalsize', ttlabels=FALSE, sort.=TRUE,
  xaxis=TRUE, lcolor='gray', ...)
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
  \item{xlab}{x-axis title}
  \item{auxdata}{
	a vector of auxiliary data, of the same length
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
  \item{w}{width of picture in inches}
  \item{h}{height of picture in inches}
  \item{margin}{a 4-vector representing, in inches, the margin to the
	left of the x-axis, below the y-axis, to the right of the x-axis,
	and above the y-axis.  By default these are computed making educated
	cases about how to accommodate \code{auxdata} etc.}
  \item{lines}{set to \code{FALSE} to suppress drawing of reference
	lines}
  \item{dotsize}{diameter of filled circles, in inches, for drawing dots}
  \item{size}{size of text in picture.  This and the next two arguments
	are LaTeX font commands without the opening backslash, e.g.,
	\code{'normalsize'}, \code{'small'}, \code{'large'}, \code{smaller[2]}.}
  \item{size.labels}{size of labels}
  \item{size.group.labels}{size of labels corresponding to \code{groups}}
  \item{ttlabels}{set to \code{TRUE} to use typewriter monospaced font
	for labels}
  \item{sort.}{
	set to \code{FALSE} to keep \code{latexDotchart} from sorting the input
	data, i.e., it will assume that the data are already properly
	arranged.  This is especially useful when you are using \code{gdata}
	and \code{groups} and you want to control the
	order that groups appear on the chart (from top to bottom).}
  \item{xaxis}{set to \code{FALSE} to suppress drawing x-axis}
  \item{lcolor}{
	color for horizontal reference lines.  Default is \code{"gray"}}
  \item{...}{ignored}
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{fh@fharrell.com}
}
\seealso{
\code{\link[Hmisc]{dotchart3}}
}
\examples{
\dontrun{
z <- latexDotchart(c(.1,.2), c('a','bbAAb'), xlab='This Label',
                   auxdata=c(.1,.2), auxtitle='Zcriteria')
f <- '/tmp/t.tex'
cat('\\documentclass{article}\n\\usepackage{epic,color}\n\\begin{document}\n', file=f)
cat(z, sep='\n', file=f, append=TRUE)
cat('\\end{document}\n', file=f, append=TRUE)

set.seed(135)
maj <- factor(c(rep('North',13),rep('South',13)))
g <- paste('Category',rep(letters[1:13],2))
n <- sample(1:15000, 26, replace=TRUE)
y1 <- runif(26)
y2 <- pmax(0, y1 - runif(26, 0, .1))
z <- latexDotchart(y1, g, groups=maj, auxdata=n, auxtitle='n', xlab='Y',
                   size.group.labels='large', ttlabels=TRUE)
f <- '/tmp/t2.tex'
cat('\\documentclass{article}\n\\usepackage{epic,color}\n\\begin{document}\n\\framebox{', file=f)
cat(z, sep='\n', file=f, append=TRUE)
cat('}\\end{document}\n', file=f, append=TRUE)
}
}
\keyword{hplot}
