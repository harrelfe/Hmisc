\name{describe}
\alias{describe}
\alias{describe.default}
\alias{describe.vector}
\alias{describe.matrix}
\alias{describe.formula}
\alias{describe.data.frame}
\alias{plot.describe}
\alias{print.describe}
\alias{print.describe.single}
\alias{[.describe}
\alias{latex.describe}
\alias{latex.describe.single}
\alias{html.describe}
\alias{html.describe.single}
\alias{formatdescribeSingle}
\title{Concise Statistical Description of a Vector, Matrix, Data Frame,
	or Formula}
\description{
\code{describe} is a generic method that invokes \code{describe.data.frame},
\code{describe.matrix}, \code{describe.vector}, or
\code{describe.formula}. \code{describe.vector} is the basic 
function for handling a single variable.
This function determines whether the variable is character, factor,
category, binary, discrete numeric, and continuous numeric, and prints
a concise statistical summary according to each. A numeric variable is
deemed discrete if it has <= 10 distinct values. In this case,
quantiles are not printed. A frequency table is printed 
for any non-binary variable if it has no more than 20 distinct
values.  For any variable for which the frequency table is not printed,
the 5 lowest and highest values are printed.  This behavior can be
overriden for long character variables with many levels using the
\code{listunique} parameter, to get a complete tabulation.

\code{describe} is especially useful for
describing data frames created by \code{*.get}, as labels, formats,
value labels, and (in the case of \code{sas.get}) frequencies of special
missing values are printed.

For a binary variable, the sum (number of 1's) and mean (proportion of
1's) are printed. If the first argument is a formula, a model frame
is created and passed to describe.data.frame.  If a variable
is of class \code{"impute"}, a count of the number of imputed values is
printed.  If a date variable has an attribute \code{partial.date}
(this is set up by \code{sas.get}), counts of how many partial dates are
actually present (missing month, missing day, missing both) are also presented.
If a variable was created by the special-purpose function \code{substi} (which
substitutes values of a second variable if the first variable is NA),
the frequency table of substitutions is also printed.

For numeric variables, \code{describe} adds an item called \code{Info}
which is a relative information measure using the relative efficiency of
a proportional odds/Wilcoxon test on the variable relative to the same
test on a variable that has no ties.  \code{Info} is related to how
continuous the variable is, and ties are less harmful the more untied
values there are.  The formula for \code{Info} is one minus the sum of
the cubes of relative frequencies of values divided by one minus the
square of the reciprocal of the sample size.  The lowest information
comes from a variable having only one distinct value following by a
highly skewed binary variable.  \code{Info} is reported to
two decimal places.

A latex method exists for converting the \code{describe} object to a
LaTeX file.  For numeric variables having more than 20 distinct values,
\code{describe} saves in its returned object the frequencies of 100
evenly spaced bins running from minimum observed value to the maximum.
When there are less than or equal to 20 distinct values, the original
values are maintained.
\code{latex} and \code{html} insert a spike histogram displaying these
frequency counts in the tabular material using the LaTeX picture
environment.  For example output see
\url{https://hbiostat.org/doc/rms/book/chapter7edition1.pdf}.
Note that the latex method assumes you have the following styles
installed in your latex installation: setspace and relsize.

The \code{html} method mimics the LaTeX output.  This is useful in the
context of Quarto/Rmarkdown html and html notebook output.
If \code{options(prType='html')} is in effect, calling \code{print} on
an object that is the result of running \code{describe} on a data frame
will result in rendering the HTML version.  If run from the console a
browser window will open.  When \code{which} is specified to
\code{print}, whether or not \code{prType='html'} is in effect, a
\code{gt} package html table will be produced containing only 
the types of variables requested.  When \code{which='both'} a list with
element names \code{Continuous} and \code{Categorical} is produced,
making it convenient for the user to print as desired, or to pass the
list directed to the \code{qreport} \code{maketabs} function when using Quarto.

The \code{plot} method is for \code{describe} objects run on data
frames.  It produces spike histograms for a graphic of
continuous variables and a dot chart for categorical variables, showing
category proportions.  The graphic format is \code{ggplot2} if the user
has not set \code{options(grType='plotly')} or has set the \code{grType}
option to something other than \code{'plotly'}.  Otherwise \code{plotly}
graphics that are interactive are produced, and these can be placed into
an Rmarkdown html notebook.  The user must install the \code{plotly}
package for this to work.  When the use hovers the mouse over a bin for
a raw data value, the actual value will pop-up (formatted using
\code{digits}).  When the user hovers over the minimum data value, most
of the information calculated by \code{describe} will pop up.  For each
variable, the number of missing values is used to assign the color to
the histogram or dot chart, and a legend is drawn.  Color is not used if
there are no missing values in any variable. For categorical variables,
hovering over the leftmost point for a variable displays details, and
for all points proportions, numerators, and denominators are displayed
in the popup.  If both continuous and categorical variables are present
and \code{which='both'} is specified, the \code{plot} method returns an
unclassed \code{list} containing two objects, named \code{'Categorical'}
and \code{'Continuous'}, in that order.

Sample weights may be specified to any of the functions, resulting
in weighted means, quantiles, and frequency tables.

Note: As discussed in Cox and Longton (2008), Stata Technical Bulletin 8(4)
pp. 557, the term "unique" has been replaced with "distinct" in the
output (but not in parameter names).

When \code{weights} are not used, the pseudomedian and Gini's mean difference are computed for
numeric variables.  The pseudomedian is labeled \code{pMedian} and is the median of all possible pairwise averages.  It is a robust and efficient measure of location that equals the mean and median for symmetric distributions.  It is also called the Hodges-Lehmann one-sample estimator.  Gini's mean difference is a robust measure of dispersion that is the
mean absolute difference between any pairs of observations.  In simple
output Gini's difference is labeled \code{Gmd}.

\code{formatdescribeSingle} is a service function for \code{latex},
\code{html}, and \code{print} methods for single variables that is not
intended to be called by the user.
}
\usage{
\method{describe}{vector}(x, descript, exclude.missing=TRUE, digits=4,
         listunique=0, listnchar=12,
         weights=NULL, normwt=FALSE, minlength=NULL, shortmChoice=TRUE,
         rmhtml=FALSE, trans=NULL, lumptails=0.01, \dots)
\method{describe}{matrix}(x, descript, exclude.missing=TRUE, digits=4, \dots)
\method{describe}{data.frame}(x, descript, exclude.missing=TRUE,
    digits=4, trans=NULL, \dots)
\method{describe}{formula}(x, descript, data, subset, na.action,
    digits=4, weights, \dots)
\method{print}{describe}(x, which = c('both', 'categorical', 'continuous'), \dots)
\method{latex}{describe}(object, title=NULL,
      file=paste('describe',first.word(expr=attr(object,'descript')),'tex',sep='.'),
      append=FALSE, size='small', tabular=TRUE, greek=TRUE,
      spacing=0.7, lspace=c(0,0), \dots)
\method{latex}{describe.single}(object, title=NULL, vname,
      file, append=FALSE, size='small', tabular=TRUE, greek=TRUE,
      lspace=c(0,0), \dots)
\method{html}{describe}(object, size=85, tabular=TRUE,
      greek=TRUE, scroll=FALSE, rows=25, cols=100, \dots)
\method{html}{describe.single}(object, size=85,
      tabular=TRUE, greek=TRUE, \dots)
formatdescribeSingle(x, condense=c('extremes', 'frequencies', 'both', 'none'),
           lang=c('plain', 'latex', 'html'), verb=0, lspace=c(0, 0),
           size=85, \dots)
\method{plot}{describe}(x, which=c('both', 'continuous', 'categorical'),
                          what=NULL,
                          sort=c('ascending', 'descending', 'none'),
                          n.unique=10, digits=5, bvspace=2, \dots)
}
\arguments{
\item{x}{
  a data frame, matrix, vector, or formula.  For a data frame, the 
  \code{describe.data.frame}
  function is automatically invoked.  For a matrix, \code{describe.matrix} is
  called.  For a formula, describe.data.frame(model.frame(x))
  is invoked. The formula may or may not have a response variable.  For
  \code{print}, \code{latex}, \code{html}, or
	\code{formatdescribeSingle}, \code{x} is an object created by
  \code{describe}.
}
\item{descript}{
  optional title to print for x. The default is the name of the argument
  or the "label" attributes of individual variables. When the first argument
  is a formula, \code{descript} defaults to a character representation of
  the formula.
}
\item{exclude.missing}{
  set toTRUE to print the names of variables that contain only missing values.
  This list appears at the bottom of the printout, and no space is taken
  up for such variables in the main listing.
}
\item{digits}{
  number of significant digits to print.  For \code{plot.describe} is
	the number of significant digits to put in hover text for
	\code{plotly} when showing raw variable values.} 
\item{listunique}{
  For a character variable that is not an \code{mChoice} variable, that
  has its longest string length greater than \code{listnchar}, and that
  has no more than \code{listunique} distinct values, all values are
  listed in alphabetic order.  Any value having more than one occurrence
  has the frequency of occurrence included.  Specify
  \code{listunique} equal to some value at least as large as the number
  of observations to ensure that all character variables will have all
  their values listed.  For purposes of tabulating character strings,
  multiple white spaces of any kind are translated to a single space,
  leading and trailing white space are ignored, and case is ignored.
}
\item{listnchar}{see \code{listunique}}
\item{weights}{
  a numeric vector of frequencies or sample weights.  Each observation
  will be treated as if it were sampled \code{weights} times.
}
\item{minlength}{value passed to summary.mChoice}
\item{shortmChoice}{set to \code{FALSE} to have summary of
	\code{mChoice} variables use actual levels everywhere, instead of
	abbreviating to integers and printing of all original labels at the
	top}
\item{rmhtml}{set to \code{TRUE} to strip html from variable labels}
\item{trans}{for \code{describe.vector} is a list specifying how to
	transform \code{x} for constructing the frequency distribution used in
	spike histograms.  The first element of the list is a character string
	describing the transformation, the second is the transformation
	function, and the third argument is the inverse of this function that
	is used in labeling points on the original scale,
	e.g. \code{trans=list('log', log, exp)}.  For
	\code{describe.data.frame} \code{trans} is a list of such lists, with
	the name of each list being name of the variable to which the
	transformation applies.  See
	\url{https://hbiostat.org/rmsc/impred.html#data} for an example.}
\item{lumptails}{specifies the quantile to use (its complement is also
	used) for grouping observations in the tails so that outliers have
	less chance of distorting the variable's range for sparkline spike
	histograms.  The default is 0.01, i.e., observations below the 0.01
	quantile are grouped together in the leftmost bin, and observations
	above the 0.99 quantile are grouped to form the last bin.}
\item{normwt}{
  The default, \code{normwt=FALSE} results in the use of \code{weights} as
  weights in computing various statistics.  In this case the sample size
  is assumed to be equal to the sum of \code{weights}.  Specify
  \code{normwt=TRUE} to divide 
  \code{weights} by a constant so that \code{weights} sum to the number of
  observations (length of vectors specified to \code{describe}).  In this
  case the number of observations is taken to be the actual number of
  records given to \code{describe}.
}
\item{object}{a result of \code{describe}}
\item{title}{unused}
\item{data}{a data frame, data table, or list}
\item{subset}{a subsetting expression}
\item{na.action}{
  These are used if a formula is specified.  \code{na.action} defaults to
  \code{na.retain} which does not delete any \code{NA}s from the data frame.
  Use \code{na.action=na.omit} or \code{na.delete} to drop any observation with
  any \code{NA} before processing.
}
\item{\dots}{
  arguments passed to \code{describe.default} which are passed to calls
  to \code{format} for numeric variables.  For example if using R
  \code{POSIXct} or \code{Date} date/time formats, specifying
  \code{describe(d,format='\%d\%b\%y')} will print date/time variables as
  \code{"01Jan2000"}.  This is useful for omitting the time
  component.  See the help file for \code{format.POSIXct} or
  \code{format.Date} for more
  information.  For \code{plot} methods, \dots is ignored.
  For \code{html} and \code{latex} methods, \dots is used to pass
	optional arguments to \code{formatdescribeSingle}, especially the
	\code{condense} argument.  For the \code{print} method when
	\code{which=} is given, possible
	arguments to use for tabulating continuous variable output are
	\code{sparkwidth} (the width of the spike histogram sparkline in pixels,
	defaulting to 200), \code{qcondense} (set to \code{FALSE} to devote
	separate columns to all quantiles), \code{extremes} (set to
	\code{TRUE} to print the 5 lowest and highest values in the table of
	continuous variables).  For categorical variable output, the argument
	\code{freq} can be used to specify how frequency tables are rendered:
	\code{'chart'} (the default; an interactive sparkline frequency bar chart) or
	\code{freq='table'} for small tables.  \code{sort} is another argument
	passed to \code{html_describe_cat}.  For sparkline frequency charts
	the default is to sort non-numeric categories in descending order of
	frequency. 	Set \code{code=FALSE} to use the original data order.  The
	\code{w} argument also applies to categorical variable output.
	}
\item{file}{
name of output file (should have a suffix of .tex).  Default name is
formed from the first word of the \code{descript} element of the
\code{describe} object, prefixed by \code{"describe"}.  Set
\code{file=""} to send LaTeX code to standard output instead of a file.
}
\item{append}{
set to \code{TRUE} to have \code{latex} append text to an existing file
named \code{file}
}
\item{size}{
LaTeX text size (\code{"small"}, the default, or \code{"normalsize"},
\code{"tiny"}, \code{"scriptsize"}, etc.) for the \code{describe} output
in LaTeX. For html is the percent of the prevailing font size to use for
the output.
}
\item{tabular}{
  set to \code{FALSE} to use verbatim rather than tabular (or html
	table) environment for the summary statistics output.  By default,
	tabular is used if the output is not too wide.}
\item{greek}{By default, the \code{latex} and \code{html} methods
  will change names of greek letters that appear in variable
  labels to appropriate LaTeX symbols in math mode, or html symbols,  unless
  \code{greek=FALSE}.}
\item{spacing}{By default, the \code{latex} method for \code{describe} run
  on a matrix or data frame uses the \code{setspace} LaTeX package with a
  line spacing of 0.7 so as to no waste space.  Specify \code{spacing=0}
  to suppress the use of the \code{setspace}'s \code{spacing} environment,
  or specify another positive value to use this environment with a
  different spacing.}
\item{lspace}{extra vertical scape, in character size units (i.e., "ex"
  as appended to the space).  When using certain font sizes, there is
  too much space left around LaTeX verbatim environments.  This
  two-vector specifies space to remove (i.e., the values are negated in
  forming the \code{vspace} command) before (first element) and after
  (second element of \code{lspace}) verbatims}
\item{scroll}{set to \code{TRUE} to create an html scrollable box for
	the html output}
\item{rows, cols}{the number of rows or columns to allocate for the
	scrollable box}
\item{vname}{unused argument in \code{latex.describe.single}}
\item{which}{specifies whether to plot numeric continuous or
	binary/categorical variables, or both.  When \code{"both"} a list with
	two elements is created.  Each element is a \code{ggplot2} or
	\code{plotly} object.  If there are no variables of a given type, a
	single \code{ggplot2} or \code{plotly} object is returned, ready to
	print.  For \code{print.describe} may be \code{"categorical"} or
	\code{"continuous"}, causing a \code{gt} table to be created with the
	categorical or continuous variable \code{describe} results.}  
\item{what}{character or numeric vector specifying which variables to
	plot; default is to plot all}
\item{sort}{specifies how and whether variables are sorted in order of
	the proportion of positives when \code{which="categorical"}.  Specify
	\code{sort="none"} to leave variables in the order they appear in the
	original data.}
\item{n.unique}{the minimum number of distinct values a numeric variable
	must have before \code{plot.describe} uses it in a continuous variable
	plot}
\item{bvspace}{the between-variable spacing for categorical variables.
	Defaults to 2, meaning twice the amount of vertical space as what is
	used for between-category spacing within a variable}
\item{condense}{specifies whether to condense the output with regard to
  the 5 lowest and highest values (\code{"extremes"}) and the frequency table
}
\item{lang}{specifies the markup language}
\item{verb}{set to 1 if a verbatim environment is already in effect for LaTeX}
}
\value{
a list containing elements \code{descript}, \code{counts},
\code{values}.  The list  is of class \code{describe}.  If the input
object was a matrix or a data 
frame, the list is a list of lists, one list for each variable
analyzed. \code{latex} returns a standard \code{latex} object.  For numeric
variables having at least 20 distinct values, an additional component
\code{intervalFreq}.  This component is a list with two elements, \code{range}
(containing two values) and \code{count}, a vector of 100 integer frequency
counts.  \code{print} with \code{which=} returns a `gt` table object.
The user can modify the table by piping formatting changes, column
removals, and other operations, before final rendering.
}
\details{
If \code{options(na.detail.response=TRUE)}
has been set and \code{na.action} is \code{"na.delete"} or
\code{"na.keep"}, summary  statistics on
the response variable are printed separately for missing and non-missing
values of each predictor.  The default summary function returns
the number of non-missing response values and the mean of the last
column of the response values, with a \code{names} attribute of
\code{c("N","Mean")}. 
When the response is a \code{Surv} object and the mean is used, this will
result in the crude proportion of events being used to summarize
the response.  The actual summary function can be designated through
\code{options(na.fun.response = "function name")}.

If you are modifying LaTex \code{parskip} or certain other parameters,
you may need to shrink the area around \code{tabular} and
\code{verbatim} environments produced by \code{latex.describe}.  You can
do this using for example
\code{\\usepackage{etoolbox}\\makeatletter\\preto{\\@verbatim}{\\topsep=-1.4pt
	\\partopsep=0pt}\\preto{\\@tabular}{\\parskip=2pt
	\\parsep=0pt}\\makeatother} in the LaTeX preamble.

Multiple choice (\code{mChoice}) variables' \code{describe} output renders well in html but not when included in a \code{Quarto} document.
}
\author{
Frank Harrell
\cr
Vanderbilt University
\cr
\email{fh@fharrell.com}
}
\seealso{
\code{\link{spikecomp}}, \code{\link{sas.get}}, \code{\link{quantile}},
\code{\link{GiniMd}}, \code{\link{pMedian}},
\code{\link{table}}, \code{\link{summary}},
\code{\link{model.frame.default}}, 
\code{\link{naprint}}, \code{\link{lapply}}, \code{\link{tapply}},
\code{\link[survival]{Surv}}, \code{\link{na.delete}},
\code{\link{na.keep}}, 
\code{\link{na.detail.response}}, \code{\link{latex}}
}
\examples{
set.seed(1)
describe(runif(200),dig=2)    #single variable, continuous
                              #get quantiles .05,.10,\dots

dfr <- data.frame(x=rnorm(400),y=sample(c('male','female'),400,TRUE))
describe(dfr)

\dontrun{
options(grType='plotly')
d <- describe(mydata)
p <- plot(d)   # create plots for both types of variables
p[[1]]; p[[2]] # or p$Categorical; p$Continuous
plotly::subplot(p[[1]], p[[2]], nrows=2)  # plot both in one
plot(d, which='categorical')    # categorical ones

d <- sas.get(".","mydata",special.miss=TRUE,recode=TRUE)
describe(d)      #describe entire data frame
attach(d, 1)
describe(relig)  #Has special missing values .D .F .M .R .T
                 #attr(relig,"label") is "Religious preference"

#relig : Religious preference  Format:relig
#    n missing  D  F M R T distinct 
# 4038     263 45 33 7 2 1        8
#
#0:none (251, 6\%), 1:Jewish (372, 9\%), 2:Catholic (1230, 30\%) 
#3:Jehovah's Witnes (25, 1\%), 4:Christ Scientist (7, 0\%) 
#5:Seventh Day Adv (17, 0\%), 6:Protestant (2025, 50\%), 7:other (111, 3\%) 


# Method for describing part of a data frame:
 describe(death.time ~ age*sex + rcs(blood.pressure))
 describe(~ age+sex)
 describe(~ age+sex, weights=freqs)  # weighted analysis

 fit <- lrm(y ~ age*sex + log(height))
 describe(formula(fit))
 describe(y ~ age*sex, na.action=na.delete)   
# report on number deleted for each variable
 options(na.detail.response=TRUE)  
# keep missings separately for each x, report on dist of y by x=NA
 describe(y ~ age*sex)
 options(na.fun.response="quantile")
 describe(y ~ age*sex)   # same but use quantiles of y by x=NA

 d <- describe(my.data.frame)
 d$age                   # print description for just age
 d[c('age','sex')]       # print description for two variables
 d[sort(names(d))]       # print in alphabetic order by var. names
 d2 <- d[20:30]          # keep variables 20-30
 page(d2)                # pop-up window for these variables

# Test date/time formats and suppression of times when they don't vary
 library(chron)
 d <- data.frame(a=chron((1:20)+.1),
                 b=chron((1:20)+(1:20)/100),
                 d=ISOdatetime(year=rep(2003,20),month=rep(4,20),day=1:20,
                               hour=rep(11,20),min=rep(17,20),sec=rep(11,20)),
                 f=ISOdatetime(year=rep(2003,20),month=rep(4,20),day=1:20,
                               hour=1:20,min=1:20,sec=1:20),
                 g=ISOdate(year=2001:2020,month=rep(3,20),day=1:20))
 describe(d)

# Make a function to run describe, latex.describe, and use the kdvi
# previewer in Linux to view the result and easily make a pdf file

 ldesc <- function(data) {
  options(xdvicmd='kdvi')
  d <- describe(data, desc=deparse(substitute(data)))
  dvi(latex(d, file='/tmp/z.tex'), nomargins=FALSE, width=8.5, height=11)
 }

 ldesc(d)
}
}
\keyword{interface}
\keyword{nonparametric}
\keyword{category}
\keyword{distribution}
\keyword{robust}
\keyword{models}
\keyword{hplot}

