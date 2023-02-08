\name{summaryM}
\alias{summaryM}
\alias{print.summaryM}
\alias{plot.summaryM}
\alias{latex.summaryM}
\alias{html.summaryM}
\alias{printsummaryM}
\title{Summarize Mixed Data Types vs. Groups}
\description{
  \code{summaryM} summarizes the variables listed in an S formula,
  computing descriptive statistics and optionally statistical tests for
  group differences.  This function is typically used when there are
  multiple left-hand-side variables that are independently against by
  groups marked by a single right-hand-side variable.  The summary
  statistics may be passed to \code{print} methods, \code{plot} methods
  for making annotated dot charts and extended box plots, and 
  \code{latex} methods for typesetting tables using LaTeX.  The
	\code{html} method uses \code{htmlTable::htmlTable} to typeset the
	table in html, by passing information to the \code{latex} method with
	\code{html=TRUE}.  This is for use with Quarto/RMarkdown.
  The \code{print} methods use the \code{print.char.matrix} function to
  print boxed tables when \code{options(prType=)} has not been given or
	when \code{prType='plain'}.  For plain tables, \code{print} calls the
	internal function \code{printsummaryM}.  When \code{prType='latex'}
	the \code{latex} method is invoked, and when \code{prType='html'} html
	is rendered.  In Quarto/RMarkdown, proper rendering will result even
	if \code{results='asis'} does not appear in the chunk header.  When
	rendering in html at the console due to having \code{options(prType='html')}
	the table will be rendered in a viewer.

	The \code{plot} method creates \code{plotly} graphics if
	\code{options(grType='plotly')}, otherwise base graphics are used.
	\code{plotly} graphics provide extra information such as which
	quantile is being displayed when hovering the mouse.  Test statistics
	are displayed by hovering over the mean.

  Continuous variables are described by three quantiles (quartiles by
  default) when printing, or by the following quantiles when plotting
  expended box plots using the \code{\link{bpplt}} function:
  0.05, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.95.  The box
  plots are scaled to the 0.025 and 0.975 quantiles of each continuous
  left-hand-side variable.  Categorical variables are 
  described by counts and percentages.
  
  The left hand side of \code{formula} may contain \code{mChoice}
  ("multiple choice") variables.  When \code{test=TRUE} each choice is
  tested separately as a binary categorical response.

  The \code{plot} method for \code{method="reverse"} creates a temporary
  function \code{Key} as is done by the \code{xYplot} and
  \code{Ecdf.formula} functions.  After \code{plot}
  runs, you can type \code{Key()} to put a legend in a default location, or
  e.g. \code{Key(locator(1))} to draw a legend where you click the left
  mouse button.  This key is for categorical variables, so to have the
  opportunity to put the key on the graph you will probably want to use
  the command \code{plot(object, which="categorical")}.  A second function
  \code{Key2} is created if continuous variables are being plotted.  It is
  used the same as \code{Key}.  If the \code{which} argument is not
  specified to \code{plot}, two pages of plots will be produced.  If you
  don't define \code{par(mfrow=)} yourself,
  \code{plot.summaryM} will try to lay out a multi-panel
  graph to best fit all the individual charts for continuous
  variables.
}
\usage{
summaryM(formula, groups=NULL, data=NULL, subset, na.action=na.retain,
         overall=FALSE, continuous=10, na.include=FALSE,
         quant=c(0.025, 0.05, 0.125, 0.25, 0.375, 0.5, 0.625,
                 0.75, 0.875, 0.95, 0.975),
         nmin=100, test=FALSE,
         conTest=conTestkw, catTest=catTestchisq,
         ordTest=ordTestpo)

\method{print}{summaryM}(...)

printsummaryM(x, digits, prn = any(n != N),
      what=c('proportion', '\%'), pctdig = if(what == '\%') 0 else 2,
      npct = c('numerator', 'both', 'denominator', 'none'),
      exclude1 = TRUE, vnames = c('labels', 'names'), prUnits = TRUE,
      sep = '/', abbreviate.dimnames = FALSE,
      prefix.width = max(nchar(lab)), min.colwidth, formatArgs=NULL, round=NULL,
      prtest = c('P','stat','df','name'), prmsd = FALSE, long = FALSE,
      pdig = 3, eps = 0.001, prob = c(0.25, 0.5, 0.75), prN = FALSE, \dots)

\method{plot}{summaryM}(x, vnames = c('labels', 'names'),
     which = c('both', 'categorical', 'continuous'), vars=NULL,
     xlim = c(0,1),
     xlab = 'Proportion',
     pch = c(16, 1, 2, 17, 15, 3, 4, 5, 0), exclude1 = TRUE,
     main, ncols=2,
     prtest = c('P', 'stat', 'df', 'name'), pdig = 3, eps = 0.001,
     conType = c('bp', 'dot', 'raw'), cex.means = 0.5, cex=par('cex'),
     height='auto', width=700, \dots)

\method{latex}{summaryM}(object, title =
      first.word(deparse(substitute(object))),
      file=paste(title, 'tex', sep='.'), append=FALSE, digits, 
      prn = any(n != N), what=c('proportion', '\%'),
      pctdig = if(what == '\%') 0 else 2,
      npct = c('numerator', 'both', 'denominator', 'slash', 'none'),
      npct.size = if(html) mspecs$html$smaller else 'scriptsize',
      Nsize = if(html) mspecs$html$smaller else 'scriptsize',
      exclude1 = TRUE,
      vnames=c("labels", "names"), prUnits = TRUE, middle.bold = FALSE,
      outer.size = if(html) mspecs$html$smaller else "scriptsize",
      caption, rowlabel = "", rowsep=html,
      insert.bottom = TRUE, dcolumn = FALSE, formatArgs=NULL, round=NULL,
      prtest = c('P', 'stat', 'df', 'name'), prmsd = FALSE,
      msdsize = if(html) function(x) x else NULL, brmsd=FALSE,
      long = FALSE, pdig = 3, eps = 0.001,
      auxCol = NULL, table.env=TRUE, tabenv1=FALSE, prob=c(0.25, 0.5, 0.75),
      prN=FALSE, legend.bottom=FALSE, html=FALSE,
      mspecs=markupSpecs, \dots)

\method{html}{summaryM}(object, \dots)
}
\arguments{
  \item{formula}{
    An S formula with additive effects.  There may be several variables
    on the right hand side separated by "+",
		or the numeral \code{1}, indicating that
    there is no grouping variable so that only margin summaries are
    produced.  The right hand side variable, if present, must be a
    discrete variable producing a limited number of groups.  On the
    left hand side there may be any number of variables, separated by
    "+", and these may be of mixed types.  These variables are analyzed
    separately by the grouping variable.
  }
	\item{groups}{if there is more than one right-hand variable, specify
    \code{groups} as a character string containing the name of the
		variable used to produce columns of the table.  The remaining right
      hand variables are combined to produce levels that cause separate
      tables or plots to be produced.}
  \item{x}{an object created by \code{summaryM}.  For
    \code{conTestkw} a numeric vector, and for \code{ordTestpo}, a numeric
    or factor variable that can be considered ordered}
  \item{data}{
    name or number of a data frame.  Default is the current frame.
  }
  \item{subset}{
    a logical vector or integer vector of subscripts used to specify the
    subset of data to use in the analysis.  The default is to use all
    observations in the data frame.
  }
  \item{na.action}{
    function for handling missing data in the input data.  The default is
    a function defined here called \code{na.retain}, which keeps all
    observations for processing, with missing variables or not.
  }
  \item{overall}{
    Setting \code{overall=TRUE} makes a new column with
    overall statistics for the whole sample.  If \code{test=TRUE} these
    marginal statistics are ignored in doing statistical tests.
  }
  \item{continuous}{
    specifies the threshold for when a variable is considered to be
    continuous (when there are at least \code{continuous} unique values).
    \code{factor} variables are always considered to be categorical no matter
    how many levels they have.
  }
  \item{na.include}{
	Set \code{na.include=TRUE} to keep missing values of categorical
      variables from being excluded from the table.  
  }
  \item{nmin}{
    For categories of the response variable in which there
    are less than or equal to \code{nmin} non-missing observations, the raw
    data are retained for later plotting in place of box plots.
  }
  \item{test}{
    Set to \code{TRUE} to compute test
    statistics using tests specified in \code{conTest} and \code{catTest}.
  }
  \item{conTest}{
    a function of two arguments (grouping variable and a continuous
    variable) that returns a list with components \code{P} (the computed
    P-value), \code{stat} (the test statistic, either chi-square or F),
    \code{df} (degrees of freedom), \code{testname} (test name),
		\code{namefun} (\code{"chisq", "fstat"}), \code{statname}
    (statistic name), an optional component \code{latexstat} (LaTeX
    representation of \code{statname}), an optional component
    \code{plotmathstat} (for R - the \code{plotmath} representation of
    \code{statname}, as a character string),  and an
    optional component \code{note} 
    that contains a character string note about the test (e.g.,
    \code{"test not done because n < 5"}).  \code{conTest} is applied to
      continuous variables 
    on the right-hand-side of the formula when \code{method="reverse"}.  The
    default uses the \code{spearman2} function to run the Wilcoxon or
    Kruskal-Wallis test using the F distribution.
  }
  \item{catTest}{
    a function of a frequency table (an integer matrix) that returns a
    list with the same components as created by \code{conTest}.  By default,
    the Pearson chi-square test is done, without continuity correction
    (the continuity correction would make the test conservative like the
    Fisher exact test).
  }
  \item{ordTest}{
    a function of a frequency table (an integer matrix) that returns a
    list with the same components as created by \code{conTest}.  By default,
    the Proportional odds likelihood ratio test is done.
  }
  \item{\dots}{
    For \code{Key} and \code{Key2} these arguments are passed to \code{key},
    \code{text}, or \code{mtitle}.  For \code{print} methods these are
    optional arguments to \code{print.char.matrix}. For \code{latex} methods
    these are passed to \code{latex.default}.  For \code{html} the
		arguments are passed the \code{latex.summaryM}, and the arguments
		may not include \code{file}.  For \code{print} the arguments are
		passed to \code{printsummaryM} or \code{latex.summaryM} depending on
		\code{options(prType=)}.
  }
  \item{object}{an object created by \code{summaryM}}
  \item{quant}{
    vector of quantiles to use for summarizing continuous variables.
	These must be numbers between 0 and 1
    inclusive and must include the numbers 0.5, 0.25, and 0.75 which are
    used for printing and for plotting 
    quantile intervals.  The outer quantiles are used for scaling the x-axes
    for such plots.  Specify outer quantiles as \code{0} and \code{1} to
    scale the x-axes using the whole observed data ranges instead of the
    default (a 0.95 quantile interval).  Box-percentile plots are drawn
    using all but the outer quantiles.
  }
  \item{prob}{
    vector of quantiles to use for summarizing continuous variables.
    These must be numbers between 0 and 1 inclusive and have previously been
    included in the \code{quant} argument of \code{summaryM}.  The vector
    must be of length three.  By default it contains 0.25, 0.5, and 0.75.

    Warning: specifying 0 and 1 as two of the quantiles will result in
    computing the minimum and maximum of the variable.  As for many random
    variables the minimum will continue to become smaller as the sample size
    grows, and the maximum will continue to get larger.  Thus the min and max
    are not recommended as summary statistics.
  }
  \item{vnames}{
    By default, tables and plots are usually labeled with variable labels
    (see the \code{label} and \code{sas.get} functions).  To use the shorter
    variable names, specify \code{vnames="name"}.
  }
  \item{pch}{
    vector of plotting characters to represent different groups, in order
    of group levels.
  }
  \item{abbreviate.dimnames}{see \code{print.char.matrix}}
  \item{prefix.width}{see \code{print.char.matrix}}
  \item{min.colwidth}{
    minimum column width to use for boxes printed with \code{print.char.matrix}.
    The default is the maximum of the minimum column label length and
      the minimum length of entries in the data cells.
  }
  \item{formatArgs}{
    a list containing other arguments to pass to \code{format.default} such as
    \code{scientific}, e.g., \code{formatArgs=list(scientific=c(-5,5))}.  For
    \code{print.summary.formula.reverse} and
	\code{format.summary.formula.reverse}, \code{formatArgs} applies only to
    statistics computed on continuous variables, not to percents,
    numerators, and denominators.  The \code{round} argument may be preferred.
  }
  \item{digits}{
    number of significant digits to print.  Default is to use the current
    value of the \code{digits} system option.
  }
	\item{what}{specifies whether proportions or percentages are to be
      printed or LaTeX'd} 
  \item{pctdig}{
    number of digits to the right of the decimal place for printing
    percentages or proportions. The default is zero if \code{what='\%'},
		so percents will be rounded to the nearest percent.  The default is
    2 for proportions.
  }
  \item{prn}{
    set to \code{TRUE} to print the number of non-missing observations on the
    current (row) variable.  The default is to print these only if any of
    the counts of non-missing values differs from the total number of
    non-missing values of the left-hand-side variable.
  }
  \item{prN}{
    set to \code{TRUE} to print the number of non-missing observations on
    rows that contain continuous variables.
  }
  \item{npct}{
    specifies which counts are to be printed to the right of percentages.
    The default is to print the frequency (numerator of the percent) in
    parentheses.  You can specify \code{"both"} to print both numerator and
    denominator as a fraction, \code{"denominator"}, \code{"slash"} to
		typeset horizontally using a forward slash, or \code{"none"}.
  }
  \item{npct.size}{
    the size for typesetting \code{npct} information which appears after
      percents. The default is \code{"scriptsize"}.
  }
  \item{Nsize}{
    When a second row of column headings is added showing sample sizes,
    \code{Nsize} specifies the LaTeX size for these subheadings.  Default
    is \code{"scriptsize"}.
  }
  \item{exclude1}{
    By default, \code{summaryM} objects will be printed, plotted,  or typeset by
    removing redundant entries from percentage tables for categorical
    variables.  For example, if you print the percent of females, you
    don't need to print the percent of males.  To override this, set
      \code{exclude1=FALSE}. 
  }
  \item{prUnits}{
    set to \code{FALSE} to suppress printing or latexing \code{units}
    attributes of variables, when \code{method='reverse'} or \code{'response'}
  }
  \item{sep}{
    character to use to separate quantiles when printing tables
  }
  \item{prtest}{
    a vector of test statistic components to print if \code{test=TRUE} was in
    effect when \code{summaryM} was called.  Defaults to printing all
    components.  Specify \code{prtest=FALSE} or \code{prtest="none"} to not
    print any tests.  This applies to \code{print}, \code{latex}, and
    \code{plot} methods.
  }
  \item{round}{
	Specify \code{round} to round
	the quantiles and optional mean and standard deviation to
	\code{round} digits after the decimal point.  Set \code{round='auto'}
      to try an automatic choice.
	}
  \item{prmsd}{
    set to \code{TRUE} to print mean and SD after the three quantiles, for
    continuous variables
  }
  \item{msdsize}{
    defaults to \code{NULL} to use the current font size for the mean and
    standard deviation if \code{prmsd} is \code{TRUE}.  Set to a character
    string or function to specify an alternate LaTeX font size.
  }
	\item{brmsd}{set to \code{TRUE} to put the mean and standard deviation
    on a separate line, for html}
  \item{long}{
    set to \code{TRUE} to print the results for the first category on its own
    line, not on the same line with the variable label
  }
  \item{pdig}{
    number of digits to the right of the decimal place for printing
    P-values.  Default is \code{3}.  This is passed to \code{format.pval}.
  }
  \item{eps}{
    P-values less than \code{eps} will be printed as \code{< eps}.  See
    \code{format.pval}.
  }
  \item{auxCol}{
    an optional auxiliary column of information, right justified, to add
    in front of statistics typeset by
    \code{latex.summaryM}.  This argument is a list with a
    single element that has a name specifying the column heading.  If this
    name includes a newline character, the portions of the string before
    and after the newline form respectively the main heading and the
    subheading (typically set in smaller font), respectively.  See the
    \code{extracolheads} argument to \code{latex.default}.  \code{auxCol}
    is filled with blanks when a variable being summarized takes up more
    than one row in the output.  This happens with categorical variables.
  }
	\item{table.env}{set to \code{FALSE} to use \code{tabular} environment
    with no caption}
	\item{tabenv1}{set to \code{TRUE} in the case of stratification when
      you want only the first stratum's table to be in a table
      environment.  This is useful when using \code{hyperref}.}
  \item{which}{Specifies whether to plot results for categorical variables,
    continuous variables, or both (the default).}
	\item{vars}{Subscripts (indexes) of variables to plot for
    \code{plotly} graphics.  Default is to plot all variables of each
    type (categorical or continuous).}
  \item{conType}{
    For drawing plots for continuous variables,
	extended box plots (box-percentile-type plots) are drawn by default,
      using all quantiles in \code{quant} except for the outermost ones
      which are using for scaling the overall plot based on the
      non-stratified marginal distribution of the current response variable.
    Specify \code{conType='dot'} to draw dot plots showing the three
      quartiles instead.  For extended box plots, means are drawn
    with a solid dot and vertical reference lines are placed at the three
    quartiles.  Specify \code{conType='raw'} to make a strip chart showing
    the raw data.  This can only be used if the sample size for each
    right-hand-side group is less than or equal to \code{nmin}.}
  \item{cex.means}{
    character size for means in box-percentile plots; default is .5}
  \item{cex}{character size for other plotted items}
	\item{height,width}{dimensions in pixels for the \code{plotly}
    \code{subplot} object containing all the extended box plots.  If
    \code{height="auto"}, \code{plot.summaryM} will set \code{height}
    based on the number of 
    continuous variables and \code{ncols} or for dot charts it will use
    \code{Hmisc::plotlyHeightDotchart}.  At present \code{height} is
    ignored for extended box plots due to vertical spacing problem with
    \code{plotly} graphics.}
  \item{xlim}{
    vector of length two specifying x-axis limits.  This is only used
      for plotting categorical variables.  Limits for continuous
      variables are determined by the outer quantiles specified in
      \code{quant}. 
  }
  \item{xlab}{x-axis label}
  \item{main}{a main title.  This applies only to the plot for
    categorical variables.}
	\item{ncols}{number of columns for \code{plotly} graphics for extended
    box plots.  Defaults to 2.  Recommendation is for 1-2.}
  \item{caption}{character string containing LaTeX table captions.}
  \item{title}{
    name of resulting LaTeX file omitting the \code{.tex} suffix.  Default
    is the name of the \code{summary} object.  If \code{caption} is specied,
    \code{title} is also used for the table's symbolic reference label. 
  }
	\item{file}{name of file to write LaTeX code to. Specifying
      \code{file=""} will cause LaTeX code to just be printed to
      standard output rather than be stored in a  permanent file.
		}
  \item{append}{specify \code{TRUE} to add code to an existing file}
  \item{rowlabel}{see \code{latex.default} (under the help file
    \code{latex})}
	\item{rowsep}{if \code{html} is \code{TRUE}, instructs the function to
    use a horizontal line to separate variables from one another.
    Recommended if \code{brmsd} is \code{TRUE}.  Ignored for LaTeX.}
  \item{middle.bold}{
    set to \code{TRUE} to have LaTeX use bold face for the middle
    quantile
  }
  \item{outer.size}{the font size for outer quantiles
  }
  \item{insert.bottom}{
    set to \code{FALSE} to suppress inclusion of definitions placed at the
    bottom of LaTeX tables.  You can also specify a character string
      containing other text that overrides the automatic text.  At
    present such text always appears in the main caption for LaTeX.
  }
  \item{legend.bottom}{
    set to \code{TRUE} to separate the table caption and legend.  This
    will place table legends at the bottom of LaTeX tables.
  }
	\item{html}{set to \code{TRUE} to typeset with html}
	\item{mspecs}{list defining markup syntax for various languages,
    defaults to Hmisc \code{markupSpecs} which the user can use as a
    starting point for editing}
  \item{dcolumn}{see \code{latex}}
  }
\value{
  a list.  \code{plot.summaryM} returns the number
  of pages of plots that were made if using base graphics, or
  \code{plotly} objects created by \code{plotly::subplot} otherwise.
	If both categorical and continuous variables were plotted, the
    returned object is a list with two named elements \code{Categorical}
    and \code{Continuous} each containing \code{plotly} objects.
    Otherwise a \code{plotly} object is returned.
    The \code{latex} method returns attributes \code{legend} and
    \code{nstrata}.
}
\section{Side Effects}{
  \code{plot.summaryM} creates a function \code{Key} and
  \code{Key2} in frame 0 that will draw legends, if base graphics are
    being used.
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
\references{
  Harrell FE (2004): Statistical tables and plots using S and LaTeX.
  Document available from
    \url{https://hbiostat.org/R/Hmisc/summary.pdf}. 
}
\seealso{
  \code{\link{mChoice}}, \code{\link{label}}, \code{\link{dotchart3}},
  \code{\link{print.char.matrix}}, \code{\link{update}},
  \code{\link{formula}}, 
  \code{\link{format.default}}, \code{\link{latex}},
  \code{\link{latexTranslate}}, \code{\link{bpplt}},
	\code{\link{tabulr}}, \code{\link{bpplotM}}, \code{\link{summaryP}}
}
\examples{
options(digits=3)
set.seed(173)
sex <- factor(sample(c("m","f"), 500, rep=TRUE))
country <- factor(sample(c('US', 'Canada'), 500, rep=TRUE))
age <- rnorm(500, 50, 5)
sbp <- rnorm(500, 120, 12)
label(sbp) <- 'Systolic BP'
units(sbp) <- 'mmHg'
treatment <- factor(sample(c("Drug","Placebo"), 500, rep=TRUE))
treatment[1]
sbp[1] <- NA

# Generate a 3-choice variable; each of 3 variables has 5 possible levels
symp <- c('Headache','Stomach Ache','Hangnail',
          'Muscle Ache','Depressed')
symptom1 <- sample(symp, 500,TRUE)
symptom2 <- sample(symp, 500,TRUE)
symptom3 <- sample(symp, 500,TRUE)
Symptoms <- mChoice(symptom1, symptom2, symptom3, label='Primary Symptoms')
table(as.character(Symptoms))

# Note: In this example, some subjects have the same symptom checked
# multiple times; in practice these redundant selections would be NAs
# mChoice will ignore these redundant selections

f <- summaryM(age + sex + sbp + Symptoms ~ treatment, test=TRUE)
f
# trio of numbers represent 25th, 50th, 75th percentile
print(f, long=TRUE)
plot(f)    # first specify options(grType='plotly') to use plotly
plot(f, conType='dot', prtest='P')
bpplt()    # annotated example showing layout of bp plot

# Produce separate tables by country
f <- summaryM(age + sex + sbp + Symptoms ~ treatment + country,
              groups='treatment', test=TRUE)
f

\dontrun{
getHdata(pbc)
s5 <- summaryM(bili + albumin + stage + protime + sex + 
               age + spiders ~ drug, data=pbc)

print(s5, npct='both')
# npct='both' : print both numerators and denominators
plot(s5, which='categorical')
Key(locator(1))  # draw legend at mouse click
par(oma=c(3,0,0,0))  # leave outer margin at bottom
plot(s5, which='continuous')  # see also bpplotM
Key2()           # draw legend at lower left corner of plot
                 # oma= above makes this default key fit the page better

options(digits=3)
w <- latex(s5, npct='both', here=TRUE, file='')

options(grType='plotly')
pbc <- upData(pbc, moveUnits = TRUE)
s <- summaryM(bili + albumin + alk.phos + copper + spiders + sex ~
              drug, data=pbc, test=TRUE)
# Render html
options(prType='html')
s   # invokes print.summaryM
a <- plot(s)
a$Categorical
a$Continuous
plot(s, which='con')
}
}
\keyword{category}
\keyword{interface}
\keyword{hplot}
\keyword{manip}
\concept{grouping}
\concept{stratification}
\concept{aggregation}
