\name{dotchart3}
\alias{dotchart3}
\alias{summaryD}
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

\code{summaryD} creates aggregate data using \code{\link{summarize}} and
calls \code{dotchart3} with suitable arguments to summarize data by
major and minor categories.
}
\usage{
dotchart3(x, labels = NULL, groups = NULL, gdata = NULL,
          cex = par("cex"), pch = 21, gpch = pch, bg = par("bg"),
          color = par("fg"), gcolor = par("fg"), lcolor = "gray",
          xlim = range(c(x, gdata), na.rm=TRUE), main = NULL, xlab = NULL,
          ylab = NULL, auxdata = NULL, auxtitle = NULL, auxgdata=NULL,
          axisat=NULL, axislabels=NULL,
          cex.labels = cex, cex.group.labels = cex.labels * 1.25,
          cex.auxdata=cex, groupfont = 2, ...)

summaryD(formula, data=NULL, fun=mean, funm=fun,
         groupsummary=TRUE, auxvar=NULL, auxtitle='',
         vals=length(auxvar) > 0, fmtvals=format,
         cex.auxdata=.7, xlab=v[1], ylab=NULL,
         gridevery=NULL, gridcol=gray(.95), sort=TRUE, ...)
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
  \item{auxgdata}{similar to \code{auxdata} but corresponding to the
		\code{gdata} argument.  These usually represent overall sample sizes
		for each group of lines.}
 \item{axisat}{a vector of tick mark locations to pass to \code{axis}.
	Useful if transforming the data axis}
  \item{axislabels}{a vector of strings specifying axis tick mark
	labels.  Useful if transforming the data axis}
  \item{cex.labels}{\code{cex} for labels}
  \item{cex.group.labels}{\code{cex} for group labels}
	\item{cex.auxdata}{\code{cex} for \code{auxdata}}
  \item{groupfont}{font number for group headings}
  \item{\dots}{other arguments passed to some of the graphics functions,
    or to \code{dotchart3} from \code{summaryD}}
	\item{formula}{a formula with one variable on the left hand side (the
         variable to compute summary statistics on), and one or two
         variables on the right hand side.  If there are two variables,
         the first is taken as the major grouping variable.  If the left
         hand side variable is a matrix it has to be a legal R variable
         name, not an expression, and \code{fun} needs to be able to
         process a matrix.}
  \item{data}{a data frame or list used to find the variables in
    \code{formula}.  If omitted, the parent environment is used.}
	\item{fun}{a summarization function creating a single number from a
    vector.  Default is the mean.}
	\item{funm}{applies if there are two right hand variables and
         \code{groupsummary=TRUE} and the marginal summaries over just
         the first \code{x} variable need to be computed differently
         than the summaries that are cross-classified by both
         variables.  \code{funm} defaults to \code{fun} and should
         have the same structure as \code{fun}.}
	\item{groupsummary}{By default, when there are two right-hand
         variables, \code{summarize(..., fun)} is called a second time
         without the use of the second variable, to obtain marginal
         summaries for the major grouping variable and display the
         results as a dot (and optionally in the right margin).  Set
         \code{groupsummary=FALSE} to suppress this information.}
  \item{auxvar}{when \code{fun} returns more than one statistic and the
         user names the elements in the returned vector, you can specify
         \code{auxvar} as a single character string naming one of them.
         This will cause the named element to be written in the right
         margin, and that element to be deleted when plotting the statistics.}
	\item{vals}{set to \code{TRUE} to show data values (dot
    locations) in the right margin.  Defaults to \code{TRUE} if
         \code{auxvar} is specified.}
	\item{fmtvals}{an optional function to format values before putting
         them in the right margin.  Default is the \code{format}
         function.}
	\item{gridevery}{specify a positive number to draw very faint vertical
    grid lines every \code{gridevery} \code{x}-axis units}
	\item{gridcol}{color for grid lines; default is very faint gray scale}
	\item{sort}{specify \code{sort=FALSE} to plot data in the original
         order, from top to bottom on the dot chart}
}
\value{the function returns invisibly}
\author{Frank Harrell}
\seealso{\code{\link{dotchart}},\code{\link{dotchart2}},\code{\link{summarize}},
         \code{\link{rlegend}}}
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

summaryD(y1 ~ maj + g, xlab='Mean')
summaryD(y1 ~ maj + g, groupsummary=FALSE)
summaryD(y1 ~ g, fmtvals=function(x) sprintf('\%4.2f', x))
Y <- cbind(y1, y2)   # summaryD cannot handle cbind(...) ~ ...
summaryD(Y  ~ maj + g, fun=function(y) y[1,], pch=c(1,17))
rlegend(.1, 26, c('y1','y2'), pch=c(1,17))

summaryD(y1 ~ maj, fun=function(y) c(mean(y), n=length(y)),
         auxvar='n', auxtitle='N')
}
\keyword{hplot}
