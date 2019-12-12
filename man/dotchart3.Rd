\name{dotchart3}
\alias{dotchart3}
\alias{dotchartp}
\alias{summaryD}
\alias{summaryDp}
\title{Enhanced Version of dotchart Function}
\description{
These are adaptations of the R dotchart function that sorts categories
top to bottom, adds \code{auxdata} and \code{auxtitle} arguments to put
extra information in the right margin, and for \code{dotchart3} adds
arguments \code{cex.labels}, \code{cex.group.labels}, and
\code{groupfont}.  By default, group headings are in a larger, bold
font.  \code{dotchart3} also cuts a bit of white space from the top and
bottom of the chart.  The most significant change, however, is in how
\code{x} is interpreted.  Columns of \code{x} no longer provide an
alternate way to define groups.  Instead, they define superpositioned
values.  This is useful for showing three quartiles, for example.  Going
along with this change, for \code{dotchart3} \code{pch} can now be a
vector specifying symbols to use going across columns of \code{x}.
\code{x} was changed in this way because to put multiple points on a
line (e.g., quartiles) and keeping track of \code{par()} parameters when
\code{dotchart2} was called with \code{add=TRUE} was cumbersome.
\code{dotchart3} changes the margins to account for horizontal labels.

\code{dotchartp} is a version of \code{dotchart3} for making the chart
with the \code{plotly} package.

\code{summaryD} creates aggregate data using \code{\link{summarize}} and
calls \code{dotchart3} with suitable arguments to summarize data by
major and minor categories.  If \code{options(grType='plotly')} is in
effect and the \code{plotly} package is installed, \code{summaryD} uses
\code{dotchartp} instead of \code{dotchart3}.

\code{summaryDp} is a streamlined \code{summaryD}-like function that
uses the \code{dotchartpl} function to render a \code{plotly} graphic.
It is used to compute summary statistics stratified separately by a
series of variables.
}
\usage{
dotchart3(x, labels = NULL, groups = NULL, gdata = NULL,
          cex = par("cex"), pch = 21, gpch = pch, bg = par("bg"),
          color = par("fg"), gcolor = par("fg"), lcolor = "gray",
          xlim = range(c(x, gdata), na.rm=TRUE), main = NULL, xlab = NULL,
          ylab = NULL, auxdata = NULL, auxtitle = NULL, auxgdata=NULL,
          axisat=NULL, axislabels=NULL,
          cex.labels = cex, cex.group.labels = cex.labels * 1.25,
          cex.auxdata=cex, groupfont = 2,
          auxwhere=NULL, height=NULL, width=NULL, \dots)

dotchartp(x, labels = NULL, groups = NULL, gdata = NULL,
            xlim = range(c(x, gdata), na.rm=TRUE), main=NULL,
            xlab = NULL, ylab = '', auxdata=NULL, auxtitle=NULL,
            auxgdata=NULL, auxwhere=c('right', 'hover'),
            symbol='circle', col=colorspace::rainbow_hcl,
            legendgroup=NULL,
            axisat=NULL, axislabels=NULL, sort=TRUE, digits=4, dec=NULL,
            height=NULL, width=700, layoutattr=FALSE, showlegend=TRUE, \dots) 

summaryD(formula, data=NULL, fun=mean, funm=fun,
         groupsummary=TRUE, auxvar=NULL, auxtitle='',
         auxwhere=c('hover', 'right'),
         vals=length(auxvar) > 0, fmtvals=format,
         symbol=if(use.plotly) 'circle' else 21,
         col=if(use.plotly) colorspace::rainbow_hcl else 1:10,
         legendgroup=NULL,
         cex.auxdata=.7, xlab=v[1], ylab=NULL,
         gridevery=NULL, gridcol=gray(.95), sort=TRUE, \dots)

summaryDp(formula,
          fun=function(x) c(Mean=mean(x, na.rm=TRUE),
                            N=sum(! is.na(x))),
          overall=TRUE, xlim=NULL, xlab=NULL,
          data=NULL, subset=NULL, na.action=na.retain,
          ncharsmax=c(50, 30),
          digits=4, ...)
}
\arguments{
  \item{x}{a numeric vector or matrix}
  \item{labels}{labels for categories corresponding to rows of
	\code{x}.  If not specified these are taken from row names of \code{x}.}
  \item{groups,gdata,cex,pch,gpch,bg,color,gcolor,lcolor,xlim,main,xlab,ylab}{see \code{\link{dotchart}}}
  \item{auxdata}{a vector of information to be put in the right margin,
	in the same order as \code{x}.  May be numeric, character, or a
	vector of expressions containing \code{\link{plotmath}} markup.  For
         \code{dotchartp}, \code{auxdata} may be a matrix to go along
         with the numeric x-axis variable, to result in point-specific
         hover text.}
  \item{auxtitle}{a column heading for \code{auxdata}}
  \item{auxgdata}{similar to \code{auxdata} but corresponding to the
		\code{gdata} argument.  These usually represent overall sample sizes
		for each group of lines.}
 \item{axisat}{a vector of tick mark locations to pass to \code{axis}.
	Useful if transforming the data axis}
  \item{axislabels}{a vector of strings specifying axis tick mark
		labels.  Useful if transforming the data axis}
	\item{digits}{number of significant digits for formatting numeric data
    in hover text for \code{dotchartp} and \code{summaryDp}}
	\item{dec}{for \code{dotchartp} only, overrides \code{digits} to
         specify the argument to \code{round()} for rounding values for
         hover labels}
  \item{cex.labels}{\code{cex} for labels}
  \item{cex.group.labels}{\code{cex} for group labels}
	\item{cex.auxdata}{\code{cex} for \code{auxdata}}
  \item{groupfont}{font number for group headings}
	\item{auxwhere}{for \code{summaryD} and \code{dotchartp} specifies
         whether \code{auxdata} 
         and \code{auxgdata} are to be placed on the far right of the
         chart, or should appear as pop-up tooltips when hovering the
         mouse over the ordinary \code{x} data points on the chart.
         Ignored for \code{dotchart3}.}
  \item{\dots}{other arguments passed to some of the graphics functions,
    or to \code{dotchart3} or \code{dotchartp} from \code{summaryD}.
         The \code{auxwhere='hover'} option is a useful argument to pass
         from \code{summaryD} to \code{dotchartp}.  Also used to pass
    other arguments to \code{dotchartpl} from \code{summaryDp}.}
  \item{layoutattr}{set to \code{TRUE} to put \code{plotly::layout}
         information in a list as an attribute \code{layout} of the
         returned \code{plotly} object instead of running the
         \code{plotly} object through the \code{layout} function.  This
         is useful if running \code{dotchartp} multiple times to later
         put together using \code{plotly::subplot} and only then running
         the result through \code{plotly::layout}.}
  \item{showlegend}{set to \code{FALSE} to suppress the \code{plotly}
         legend with \code{dotchartp}}
	\item{formula}{a formula with one variable on the left hand side (the
         variable to compute summary statistics on), and one or two
         variables on the right hand side.  If there are two variables,
         the first is taken as the major grouping variable.  If the left
         hand side variable is a matrix it has to be a legal R variable
         name, not an expression, and \code{fun} needs to be able to
         process a matrix.  For \code{summaryDp} there may be more than
  two right-hand-side variables.}
  \item{data}{a data frame or list used to find the variables in
    \code{formula}.  If omitted, the parent environment is used.}
	\item{fun}{a summarization function creating a single number from a
    vector.  Default is the mean.  For \code{summaryDp}, \code{fun}
         produces a named vector of summary statistics, with the default
         computing the \code{Mean} and \code{N} (number of non-missing values).}
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
  \item{symbol}{a scalar or vector of \code{pch} values for ordinary
         graphics or a character vector or scalar of \code{plotly}
         symbols.   These correspond to columns of \code{x} or elements
         produced by \code{fun}.}
  \item{col}{a function or vector of colors to assign to multiple points
         plotted in one line. If a function it will be evaluated with an
         argument equal to the number of groups/columns.}
  \item{legendgroup}{see \code{plotly} documentation; corresponds to
         column names/\code{fun} output for \code{plotly} graphs only}
 	\item{gridevery}{specify a positive number to draw very faint vertical
    grid lines every \code{gridevery} \code{x}-axis units; for
    non-\code{plotly} charts}
	\item{gridcol}{color for grid lines; default is very faint gray scale}
	\item{sort}{specify \code{sort=FALSE} to plot data in the original
         order, from top to bottom on the dot chart.  For
         \code{dotchartp}, set \code{sort} to \code{'descending'} to
         sort in descending order of the first column of \code{x}, or
         \code{'ascending'} to do the reverse.  These do not make sense
         if \code{groups} is present.}
	\item{height,width}{height and width in pixels for \code{dotchartp} if
         not using \code{plotly} defaults.  Ignored for
         \code{dotchart3}.  If set to \code{"auto"} the height is
         computed using \code{Hmisc::plotlyHeightDotchart}.}
	\item{overall}{set to \code{FALSE} to suppress plotting of
		unstratified estimates}
	\item{subset}{an observation subsetting expression}
	\item{na.action}{an \code{NA} action function}
	\item{ncharsmax}{a 2-vector specifying the number of characters after
  which an html new line character should be placed, respectively for
  the x-axis label and the stratification variable levels}
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

\dontrun{
dotchartp(cbind(y1, y2), g, groups=maj, auxdata=n, auxtitle='n',
          xlab='Y', gdata=cbind(c(0,.1), c(.23,.44)), auxgdata=c(-1,-2),
          symbol=c('circle', 'line-ns-open'))

summaryDp(sbp ~ region + sex + race + cut2(age, g=5), data=mydata)
}

## Put options(grType='plotly') to have the following use dotchartp
## (rlegend will not apply)
## Add argument auxwhere='hover' to summaryD or dotchartp to put
## aux info in hover text instead of right margin
summaryD(y1 ~ maj + g, xlab='Mean')
summaryD(y1 ~ maj + g, groupsummary=FALSE)
summaryD(y1 ~ g, fmtvals=function(x) sprintf('\%4.2f', x))
Y <- cbind(y1, y2)   # summaryD cannot handle cbind(...) ~ ...
summaryD(Y  ~ maj + g, fun=function(y) y[1,], symbol=c(1,17))
rlegend(.1, 26, c('y1','y2'), pch=c(1,17))

summaryD(y1 ~ maj, fun=function(y) c(Mean=mean(y), n=length(y)),
         auxvar='n', auxtitle='N')
}
\keyword{hplot}
