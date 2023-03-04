\name{summary.formula}
\alias{summary.formula}
\alias{stratify}
\alias{print.summary.formula.response}
\alias{plot.summary.formula.response}
\alias{latex.summary.formula.response}
\alias{print.summary.formula.reverse}
\alias{plot.summary.formula.reverse}
\alias{latex.summary.formula.reverse}
\alias{[.summary.formula.response}
\alias{print.summary.formula.cross}
\alias{latex.summary.formula.cross}
\alias{formula.summary.formula.cross}
\alias{na.retain}
\alias{cumcategory}
\alias{conTestkw}
\alias{catTestchisq}
\alias{ordTestpo}
\title{Summarize Data for Making Tables and Plots}
\description{
  \code{summary.formula} summarizes the variables listed in an S formula,
  computing descriptive statistics (including ones in a
  user-specified function).  The summary statistics may be passed to
  \code{print} methods, \code{plot} methods for making annotated dot charts, and
  \code{latex} methods for typesetting tables using LaTeX. 
  \code{summary.formula} has three methods for computing descriptive
  statistics on univariate or multivariate responses, subsetted by
  categories of other variables.  The method of summarization is
  specified in the parameter \code{method} (see details below).  For the
  \code{response} and \code{cross} methods, the statistics used to
  summarize the data 
  may be specified in a very flexible way (e.g., the geometric mean,
  33rd percentile, Kaplan-Meier 2-year survival estimate, mixtures of
  several statistics).  The default summary statistic for these methods
  is the mean (the proportion of positive responses for a binary
  response variable).  The \code{cross} method is useful for creating data
  frames which contain summary statistics that are passed to \code{trellis}
  as raw data (to make multi-panel dot charts, for example).  The
  \code{print} methods use the \code{print.char.matrix} function to print boxed
  tables.

  The right hand side of \code{formula} may contain \code{mChoice}
  (\dQuote{multiple choice}) variables.  When \code{test=TRUE} each choice is
  tested separately as a binary categorical response.

  The \code{plot} method for \code{method="reverse"} creates a temporary
  function \code{Key} in frame 0 as is done by the \code{xYplot} and
  \code{Ecdf.formula} functions.  After \code{plot} runs, you can type
  \code{Key()} to put a legend in a default location, or
  e.g. \code{Key(locator(1))} to draw a legend where you click the left
  mouse button.  This key is for categorical variables, so to have the
  opportunity to put the key on the graph you will probably want to use
  the command \code{plot(object, which="categorical")}.  A second function
  \code{Key2} is created if continuous variables are being plotted.  It is
  used the same as \code{Key}.  If the \code{which} argument is not
  specified to \code{plot}, two pages of plots will be produced.  If you
  don't define \code{par(mfrow=)} yourself,
  \code{plot.summary.formula.reverse} will try to lay out a multi-panel
  graph to best fit all the individual dot charts for continuous
  variables.

  There is a subscripting method for objects created with
  \code{method="response"}. 
  This can be used to print or plot selected variables or summary statistics
  where there would otherwise be too many on one page.

  \code{cumcategory} is a utility function useful when summarizing an ordinal
  response variable.  It converts such a variable having \code{k} levels to a
  matrix with \code{k-1} columns, where column \code{i} is a vector of zeros and
  ones indicating that the categorical response is in level \code{i+1} or
  greater.  When the left hand side of \code{formula} is \code{cumcategory(y)},
  the default \code{fun} will summarize it by computing all of the relevant
  cumulative proportions.

  Functions \code{conTestkw}, \code{catTestchisq}, \code{ordTestpo} are
  the default statistical test functions for \code{summary.formula}.
  These defaults are: Wilcoxon-Kruskal-Wallis test for continuous
  variables, Pearson chi-square test for categorical variables, and the
  likelihood ratio chi-square test from the proportional odds model for
  ordinal variables.  These three functions serve also as templates for
  the user to create her own testing functions that are self-defining in
  terms of how the results are printed or rendered in LaTeX, or plotted.
}
\usage{
\method{summary}{formula}(formula, data=NULL, subset=NULL,
        na.action=NULL, fun = NULL,
        method = c("response", "reverse", "cross"),
        overall = method == "response" | method == "cross",
        continuous = 10, na.rm = TRUE, na.include = method != "reverse",
        g = 4, quant = c(0.025, 0.05, 0.125, 0.25, 0.375, 0.5, 0.625,
                         0.75, 0.875, 0.95, 0.975),
        nmin = if (method == "reverse") 100
               else 0,
        test = FALSE, conTest = conTestkw, catTest = catTestchisq,
        ordTest = ordTestpo, \dots)

\method{[}{summary.formula.response}(x, i, j, drop=FALSE)

\method{print}{summary.formula.response}(x, vnames=c('labels','names'), prUnits=TRUE,
      abbreviate.dimnames=FALSE,
      prefix.width, min.colwidth, formatArgs=NULL, markdown=FALSE, \dots)

\method{plot}{summary.formula.response}(x, which = 1, vnames = c('labels','names'), xlim, xlab,
     pch = c(16, 1, 2, 17, 15, 3, 4, 5, 0), superposeStrata = TRUE,
     dotfont = 1, add = FALSE, reset.par = TRUE, main, subtitles = TRUE,
     \dots)

\method{latex}{summary.formula.response}(object, title = first.word(deparse(substitute(object))), caption,
      trios, vnames = c('labels', 'names'), prn = TRUE, prUnits = TRUE,
      rowlabel = '', cdec = 2, ncaption = TRUE, \dots)

\method{print}{summary.formula.reverse}(x, digits, prn = any(n != N), pctdig = 0,
      what=c('\%', 'proportion'),
      npct = c('numerator', 'both', 'denominator', 'none'),
      exclude1 = TRUE, vnames = c('labels', 'names'), prUnits = TRUE,
      sep = '/', abbreviate.dimnames = FALSE,
      prefix.width = max(nchar(lab)), min.colwidth, formatArgs=NULL, round=NULL,
      prtest = c('P','stat','df','name'), prmsd = FALSE, long = FALSE,
      pdig = 3, eps = 0.001, \dots)

\method{plot}{summary.formula.reverse}(x, vnames = c('labels', 'names'), what = c('proportion', '\%'),
     which = c('both', 'categorical', 'continuous'),
     xlim = if(what == 'proportion') c(0,1)
            else c(0,100), 
     xlab = if(what=='proportion') 'Proportion'
            else 'Percentage', 
     pch = c(16, 1, 2, 17, 15, 3, 4, 5, 0), exclude1 = TRUE,
     dotfont = 1, main,
     prtest = c('P', 'stat', 'df', 'name'), pdig = 3, eps = 0.001,
     conType = c('dot', 'bp', 'raw'), cex.means = 0.5, \dots)

\method{latex}{summary.formula.reverse}(object, title = first.word(deparse(substitute(object))), digits,
      prn = any(n != N), pctdig = 0, what=c('\%', 'proportion'),
      npct = c("numerator", "both", "denominator", "slash", "none"),
      npct.size = 'scriptsize', Nsize = "scriptsize", exclude1 = TRUE,
      vnames=c("labels", "names"), prUnits = TRUE, middle.bold = FALSE,
      outer.size = "scriptsize", caption, rowlabel = "",
      insert.bottom = TRUE, dcolumn = FALSE, formatArgs=NULL, round = NULL,
      prtest = c('P', 'stat', 'df', 'name'), prmsd = FALSE,
      msdsize = NULL, long = dotchart, pdig = 3, eps = 0.001,
      auxCol = NULL, dotchart=FALSE, \dots)

\method{print}{summary.formula.cross}(x, twoway = nvar == 2, prnmiss = any(stats$Missing > 0), prn = TRUE,
      abbreviate.dimnames = FALSE, prefix.width = max(nchar(v)),
      min.colwidth, formatArgs = NULL, \dots)

\method{latex}{summary.formula.cross}(object, title = first.word(deparse(substitute(object))),
      twoway = nvar == 2, prnmiss = TRUE, prn = TRUE,
      caption=attr(object, "heading"), vnames=c("labels", "names"),
      rowlabel="", \dots)

stratify(\dots, na.group = FALSE, shortlabel = TRUE)

\method{formula}{summary.formula.cross}(x, \dots)

cumcategory(y)

conTestkw(group, x)
catTestchisq(tab)
ordTestpo(group, x)
}
\arguments{
  \item{formula}{
    An \R formula with additive effects.  For \code{method="response"} or
    \code{"cross"}, the dependent variable has the usual connotation.  For
    \code{method="reverse"}, the dependent variable is what is usually thought
    of as an independent variable, and it is one that is used to stratify
    all of the right hand side variables.  For \code{method="response"}
    (only), the \code{formula} may contain one or more invocations of the
    \code{stratify} function whose arguments are defined below.  This causes
    the entire analysis to be stratified by cross-classifications of the
    combined list of stratification factors.  This stratification will be
    reflected as major column groupings in the resulting table, or as more
    response columns for plotting.  If \code{formula} has no dependent variable
    \code{method="reverse"} is the only legal value and so \code{method} defaults to
    \code{"reverse"} in this case.
  }
  \item{x}{an object created by \code{summary.formula}.  For
    \code{conTestkw} a numeric vector, and for \code{ordTestpo}, a numeric
    or factor variable that can be considered ordered}
  \item{y}{
    a numeric, character, category, or factor vector for \code{cumcategory}.
    Is converted to a categorical variable is needed.
  }
  \item{drop}{
    logical. If \code{TRUE} the result is coerced to the
    lowest possible dimension.
  }
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
  \item{fun}{
    function for summarizing data in each cell.  Default is to take the
    mean of each column of the possibly multivariate response variable.
    You can specify \code{fun="\%"} to compute percentages (100 times the mean of a 
    series of logical or binary variables).
    User--specified functions can also return a matrix.  For example, you might 
    compute quartiles on a bivariate response.  Does not apply to
    \code{method="reverse"}. 
  }
  \item{method}{
    The default is \code{"response"}, in which case the response variable may
    be multivariate and any number of statistics may be used to summarize
    them.  Here the responses are summarized separately for each of any
    number of independent variables.  Continuous independent variables
    (see the \code{continuous} parameter below) are automatically stratified
    into \code{g} (see below) quantile groups (if you want to control the
    discretization for selected variables, use the \code{cut2} function on them).  
    Otherwise, the data are
    subsetted by all levels of discrete right hand side variables.  For
    multivariate responses, subjects are considered to be missing if any
    of the columns is missing.  


    The \code{method="reverse"} option is
    typically used to make baseline characteristic tables, for example.
    The single left hand side variable must be categorical (e.g.,
    treatment), and the right hand side variables are broken down one at a
    time by the "dependent" variable.  Continuous variables are described
    by three quantiles (quartiles by default) along with 
    outer quantiles (used only for scaling x-axes when plotting quartiles;
    all are used when plotting box-percentile plots), and
    categorical ones are
    described by counts and percentages.  If there is no left hand side
    variable, \code{summary} assumes that there is only one group in the data,
    so that only one column of summaries will appear.
    If there is no dependent variable in \code{formula}, \code{method} defaults to
    \code{"reverse"} automatically.


    The \code{method="cross"} option allows for a multivariate dependent
    variable and for up to three independents.  Continuous independent
    variables (those with at least \code{continuous} unique values) are
    automatically divided into \code{g} quantile groups.
    The independents are cross-classified, and marginal statistics may optionally be computed.
    The output of \code{summary.formula} in this case is a data frame
    containing the independent variable combinations (with levels of
    \code{"All"} corresponding to marginals) and the corresponding summary
    statistics in the matrix \code{S}.  The output data frame is suitable for
    direct use in \code{trellis}.  The \code{print} and \code{latex} typesetting methods for this
    method allows for a special two-way format if there are two right
    hand variables.
  }
  \item{overall}{
    For \code{method="reverse"}, setting \code{overall=TRUE} makes a new column with
    overall statistics for the whole sample.  For \code{method="cross"},
    \code{overall=TRUE} (the default) results in all marginal statistics being
    computed.  For \code{trellis} displays (usually multi-panel dot plots), 
    these marginals just form other categories.  For \code{"response"}, the
    default is \code{overall=TRUE}, causing a final row of global summary
    statistics to appear in tables and dot charts.  If \code{test=TRUE} these
    marginal statistics are ignored in doing statistical tests.
  }
  \item{continuous}{
    specifies the threshold for when a variable is considered to be
    continuous (when there are at least \code{continuous} unique values).
    \code{factor} variables are always considered to be categorical no matter
    how many levels they have.
  }
  \item{na.rm}{
    \code{TRUE} (the default) to exclude \code{NA}s before passing data to
    \code{fun} to compute statistics, \code{FALSE} otherwise.
    \code{na.rm=FALSE} is useful if the response variable is a matrix and
    you do not wish to exclude a row of the matrix if any of the columns
    in that row are \code{NA}.  \code{na.rm} also applies to summary
    statistic functions such as \code{smean.cl.normal}.  For these \code{na.rm}
    defaults to \code{TRUE} unlike built-in functions.
  }
  \item{na.include}{
    for \code{method="response"}, set \code{na.include=FALSE} to exclude missing values from
    being counted as their own category when subsetting the response(s)
    by levels of a categorical variable.  For \code{method="reverse"} set
    \code{na.include=TRUE} to keep missing values of categorical variables from
    being excluded from the table.  
  }
  \item{g}{
    number of quantile groups to use when variables are automatically
    categorized with \code{method="response"} or \code{"cross"} using \code{cut2}
  }
  \item{nmin}{
    if fewer than \code{nmin} observations exist in a category for \code{"response"}
    (over all strata combined), that category will be ignored.  For
    \code{"reverse"}, for categories of the response variable in which there
    are less than or equal to \code{nmin} non-missing observations, the raw
    data are retained for later plotting in place of box plots.
  }
  \item{test}{
    applies if \code{method="reverse"}.  Set to \code{TRUE} to compute test
    statistics using tests specified in \code{conTest} and \code{catTest}.
  }
  \item{conTest}{
    a function of two arguments (grouping variable and a continuous
    variable) that returns a list with components \code{P} (the computed
    P-value), \code{stat} (the test statistic, either chi-square or F),
    \code{df} (degrees of freedom), \code{testname} (test name), \code{statname}
    (statistic name), \code{namefun} (\code{"chisq", "fstat"}), an
    optional component \code{latexstat} (LaTeX 
    representation of \code{statname}), an optional component
    \code{plotmathstat} (for R - the \code{plotmath} representation of
    \code{statname}, as a character string),  and an
    optional component \code{note} 
    that contains a character string note about the test (e.g.,
    \code{"test not done because n < 5"}).  \code{conTest} is applied to continuous variables
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
  \item{...}{
    for \code{summary.formula} these are optional
    arguments for \code{cut2} when variables are automatically categorized.
    For \code{plot} methods these arguments are passed to \code{dotchart2}.
    For \code{Key} and \code{Key2} these arguments are passed to \code{key},
    \code{text}, or \code{mtitle}.  For \code{print} methods these are
    optional arguments to \code{print.char.matrix}. For \code{latex} methods
    these are passed to \code{latex.default}.  One of the most important of
    these is \code{file}.  Specifying \code{file=""} will cause LaTeX code
    to just be printed to standard output rather than be stored in a
    permanent file.
  }
  \item{object}{an object created by \code{summary.formula}}
  \item{quant}{
    vector of quantiles to use for summarizing data with
    \code{method="reverse"}.  This must be numbers between 0 and 1
    inclusive and must include the numbers 0.5, 0.25, and 0.75 which are
    used for printing and for plotting 
    quantile intervals.  The outer quantiles are used for scaling the x-axes
    for such plots.  Specify outer quantiles as \code{0} and \code{1} to
    scale the x-axes using the whole observed data ranges instead of the
    default (a 0.95 quantile interval).  Box-percentile plots are drawn
    using all but the outer quantiles.
  }
  \item{vnames}{
    By default, tables and plots are usually labeled with variable labels
    (see the \code{label} and \code{sas.get} functions).  To use the shorter
    variable names, specify \code{vnames="name"}.
  }
  \item{pch}{
    vector of plotting characters to represent different groups, in order
    of group levels.  For \code{method="response"} the characters
    correspond to levels of the \code{stratify} variable if
    \code{superposeStrata=TRUE}, and if no 
    \code{strata} are used or if \code{superposeStrata=FALSE}, the 
    \code{pch} vector corresponds to the \code{which} argument for
    \code{method="response"}.
  }
  \item{superposeStrata}{
    If \code{stratify} was used, set \code{superposeStrata=FALSE} to make
    separate dot charts for each level of the \code{stratification}
    variable, for \code{method='response'}.  The default is to
    superposition all strata on one dot chart.
  }
  \item{dotfont}{font for plotting points}
  \item{reset.par}{set to \code{FALSE} to suppress the restoring of the
    old par values in \code{plot.summary.formula.response}
  }

  \item{abbreviate.dimnames}{see \code{print.char.matrix}}
  \item{prefix.width}{see \code{print.char.matrix}}
  \item{min.colwidth}{
    minimum column width to use for boxes printed with \code{print.char.matrix}.
    The default is the maximum of the minimum column label length and the minimum
    length of entries in the data cells.
  }
  \item{formatArgs}{
    a list containing other arguments to pass to \code{format.default} such as
    \code{scientific}, e.g., \code{formatArgs=list(scientific=c(-5,5))}.  For
    \code{print.summary.formula.reverse} and
	\code{format.summary.formula.reverse}, \code{formatArgs} applies only to
    statistics computed on continuous variables, not to percents,
    numerators, and denominators.  The \code{round} argument may be preferred.
  }
  \item{markdown}{for \code{print.summary.formula.response} set to
  \code{TRUE} to use \code{knitr::kable} to produce the table in
  markdown format rather than using raw text output created by \code{print.char.matrix}}
  \item{digits}{
    number of significant digits to print.  Default is to use the current
    value of the \code{digits} system option.
  }
  \item{prn}{
    set to \code{TRUE} to print the number of non-missing observations on the
    current (row) variable.  The default is to print these only if any of
    the counts of non-missing values differs from the total number of
    non-missing values of the left-hand-side variable.
    For \code{method="cross"} the default is to always print \code{N}.
  }
  \item{prnmiss}{
    set to \code{FALSE} to suppress printing counts of missing values for \code{"cross"}
  }
  \item{what}{
    for \code{method="reverse"} specifies whether proportions or percentages
    are to be plotted
  }
  \item{pctdig}{
    number of digits to the right of the decimal place for printing
    percentages. The default is zero, so percents will be rounded to the
    nearest percent.
  }
  \item{npct}{
    specifies which counts are to be printed to the right of percentages.
    The default is to print the frequency (numerator of the percent) in
    parentheses.  You can specify \code{"both"} to print both numerator and
    denominator, \code{"denominator"}, \code{"slash"} to
		typeset horizontally using a forward slash, or \code{"none"}.
  }
  \item{npct.size}{
    the size for typesetting \code{npct} information which appears after percents.
    The default is \code{"scriptsize"}.
  }
  \item{Nsize}{
    When a second row of column headings is added showing sample sizes,
    \code{Nsize} specifies the LaTeX size for these subheadings.  Default
    is \code{"scriptsize"}.
  }
  \item{exclude1}{
    by default, \code{method="reverse"} objects will be printed, plotted,  or typeset by
    removing redundant entries from percentage tables for categorical
    variables.  For example, if you print the percent of females, you
    don't need to print the percent of males.  To override this, set \code{exclude1=FALSE}.
  }
  \item{prUnits}{
    set to \code{FALSE} to suppress printing or latexing \code{units}
    attributes of variables, when \code{method='reverse'} or \code{'response'}
  }
  \item{sep}{
    character to use to separate quantiles when printing
    \code{method="reverse"} tables
  }
  \item{prtest}{
    a vector of test statistic components to print if \code{test=TRUE} was in
    effect when \code{summary.formula} was called.  Defaults to printing all
    components.  Specify \code{prtest=FALSE} or \code{prtest="none"} to not
    print any tests.  This applies to \code{print}, \code{latex}, and
    \code{plot} methods for \code{method='reverse'}.
  }
  \item{round}{
        for \code{print.summary.formula.reverse} and
	\code{latex.summary.formula.reverse} specify \code{round} to round
	the quantiles and optional mean and standard deviation to
	\code{round} digits after the decimal point
	}
  \item{prmsd}{
    set to \code{TRUE} to print mean and SD after the three quantiles, for
    continuous variables with \code{method="reverse"}
  }
  \item{msdsize}{
    defaults to \code{NULL} to use the current font size for the mean and
    standard deviation if \code{prmsd} is \code{TRUE}.  Set to a character
    string to specify an alternate LaTeX font size.
  }
  \item{long}{
    set to \code{TRUE} to print the results for the first category on its own
    line, not on the same line with the variable label (for
    \code{method="reverse"} with \code{print} and \code{latex} methods)
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
    \code{latex.summary.formula.reverse}.  This argument is a list with a
    single element that has a name specifying the column heading.  If this
    name includes a newline character, the portions of the string before
    and after the newline form respectively the main heading and the
    subheading (typically set in smaller font), respectively.  See the
    \code{extracolheads} argument to \code{latex.default}.  \code{auxCol}
    is filled with blanks when a variable being summarized takes up more
    than one row in the output.  This happens with categorical variables.
  }
  \item{twoway}{
    for \code{method="cross"} with two right hand side variables, \code{twoway}
    controls whether the resulting table will be printed in enumeration
    format or as a two-way table (the default)
  }
  \item{which}{
    For \code{method="response"} specifies the sequential number or a vector of
    subscripts of statistics to plot.  If you had any \code{stratify}
    variables, these are counted as if more statistics were computed.
  For \code{method="reverse"} specifies whether to plot results 
    for categorical variables, continuous variables, or both (the default).
  }
  \item{conType}{
    For plotting \code{method="reverse"} plots for continuous variables,
    dot plots showing quartiles are drawn by default.  Specify
    \code{conType='bp'} to draw box-percentile plots using all the
    quantiles in \code{quant} except the outermost ones.  Means are drawn
    with a solid dot and vertical reference lines are placed at the three
    quartiles.  Specify \code{conType='raw'} to make a strip chart showing
    the raw data.  This can only be used if the sample size for each
    left-hand-side group is less than or equal to \code{nmin}.}
  \item{cex.means}{
    character size for means in box-percentile plots; default is .5}
  \item{xlim}{
    vector of length two specifying x-axis limits.  For
    \code{method="reverse"}, this is only used for plotting categorical
    variables.  Limits for continuous variables are determined by the
    outer quantiles specified in \code{quant}.
  }
  \item{xlab}{
    x-axis label
  }
  \item{add}{
    set to \code{TRUE} to add to an existing plot
  }
  \item{main}{
    a main title.  For \code{method="reverse"} this applies only to the plot
    for categorical variables.
  }
  \item{subtitles}{
    set to \code{FALSE} to suppress automatic subtitles
  }
  \item{caption}{
    character string containing LaTeX table captions.
  }
  \item{title}{
    name of resulting LaTeX file omitting the \code{.tex} suffix.  Default
    is the name of the \code{summary} object.  If \code{caption} is specied,
    \code{title} is also used for the table's symbolic reference label. 
  }
  \item{trios}{
    If for \code{method="response"} you summarized the response(s) by using
    three quantiles, specify \code{trios=TRUE} or \code{trios=v} to group each set of
    three statistics into one column for \code{latex} output, using the format
    a B c, where the outer quantiles are in smaller font
    (\code{scriptsize}).  For \code{trios=TRUE}, the overall column names are taken
    from the column names of the original data matrix.  To give new
    column names, specify \code{trios=v}, where \code{v} is a vector of column
    names, of length \code{m/3}, where \code{m} is the original number of columns
    of summary statistics.
  }
  \item{rowlabel}{
    see \code{latex.default} (under the help file \code{latex})
  }
  \item{cdec}{
    number of decimal places to the right of the decimal point for
    \code{latex}.  This value should be a scalar (which will be properly
    replicated), or a vector with length equal to the number of columns
    in the table.  For \code{"response"} tables, this length does not count
    the column for \code{N}.
  }
  \item{ncaption}{
    set to \code{FALSE} to not have \code{latex.summary.formula.response}
    put sample sizes in captions
  }
  \item{i}{
    a vector of integers, or character strings containing variable names
    to subset on.  Note that each row subsetted on in an \code{summary.formula.reverse}
    object subsets on all the levels that make up the corresponding variable
    (automatically).
  }
  \item{j}{
    a vector of integers representing column numbers
  }
  \item{middle.bold}{
    set to \code{TRUE} to have LaTeX use bold face for the middle quantile for
    \code{method="reverse"} 
  }
  \item{outer.size}{
    the font size for outer quantiles for \code{"reverse"} tables
  }
  \item{insert.bottom}{
    set to \code{FALSE} to suppress inclusion of definitions placed at the
    bottom of LaTeX tables for \code{method="reverse"}
  }
  \item{dcolumn}{
    see \code{latex}
  }
  \item{na.group}{
    set to \code{TRUE} to have missing stratification variables given their own
    category (\code{NA})
  }
  \item{shortlabel}{
    set to \code{FALSE} to include stratification variable names and equal signs
    in labels for strata levels
  }
  \item{dotchart}{
    set to \code{TRUE} to output a dotchart in the latex table being
    generated.
  }
  \item{group}{for \code{conTest} and \code{ordTest}, a numeric or
  factor variable with length the same as \code{x}
  }
  \item{tab}{for \code{catTest}, a frequency table such as that created
  by \code{table()}
  }
}
\value{
  \code{summary.formula} returns a data frame or list depending on
  \code{method}.  \code{plot.summary.formula.reverse} returns the number
  of pages of plots that were made.
}
\section{Side Effects}{
  \code{plot.summary.formula.reverse} creates a function \code{Key} and
  \code{Key2} in frame 0 that will draw legends.
}
\author{
  Frank Harrell\cr
  Department of Biostatistics\cr
  Vanderbilt University\cr
  \email{fh@fharrell.com}
}
\references{
  Harrell FE (2007): Statistical tables and plots using S and LaTeX.
  Document available from \url{https://hbiostat.org/R/Hmisc/summary.pdf}.
}
\seealso{
  \code{\link{mChoice}}, \code{\link{smean.sd}}, \code{\link{summarize}},
  \code{\link{label}}, \code{\link[survival]{strata}}, \code{\link{dotchart2}},
  \code{\link{print.char.matrix}}, \code{\link{update}},
  \code{\link{formula}}, \code{\link{cut2}}, \code{\link{llist}},
  \code{\link{format.default}}, \code{\link{latex}},
  \code{\link{latexTranslate}} \code{\link{bpplt}},
  \code{\link{summaryM}}, \code{\link{summary}}
}
\examples{
options(digits=3)
set.seed(173)
sex <- factor(sample(c("m","f"), 500, rep=TRUE))
age <- rnorm(500, 50, 5)
treatment <- factor(sample(c("Drug","Placebo"), 500, rep=TRUE))

# Generate a 3-choice variable; each of 3 variables has 5 possible levels
symp <- c('Headache','Stomach Ache','Hangnail',
          'Muscle Ache','Depressed')
symptom1 <- sample(symp, 500,TRUE)
symptom2 <- sample(symp, 500,TRUE)
symptom3 <- sample(symp, 500,TRUE)
Symptoms <- mChoice(symptom1, symptom2, symptom3, label='Primary Symptoms')
table(Symptoms)

# Note: In this example, some subjects have the same symptom checked
# multiple times; in practice these redundant selections would be NAs
# mChoice will ignore these redundant selections

#Frequency table sex*treatment, sex*Symptoms
summary(sex ~ treatment + Symptoms, fun=table)
# could also do summary(sex ~ treatment +
#  mChoice(symptom1,symptom2,symptom3), fun=table)


#Compute mean age, separately by 3 variables
summary(age ~ sex + treatment + Symptoms)


f <- summary(treatment ~ age + sex + Symptoms, method="reverse", test=TRUE)
f
# trio of numbers represent 25th, 50th, 75th percentile
print(f, long=TRUE)
plot(f)
plot(f, conType='bp', prtest='P')
bpplt()    # annotated example showing layout of bp plot

#Compute predicted probability from a logistic regression model
#For different stratifications compute receiver operating
#characteristic curve areas (C-indexes)
predicted <- plogis(.4*(sex=="m")+.15*(age-50))
positive.diagnosis <- ifelse(runif(500)<=predicted, 1, 0)
roc <- function(z) {
   x <- z[,1];
   y <- z[,2];
   n <- length(x);
   if(n<2)return(c(ROC=NA));
   n1 <- sum(y==1);
   c(ROC= (mean(rank(x)[y==1])-(n1+1)/2)/(n-n1) );
 }
y <- cbind(predicted, positive.diagnosis)
options(digits=2)
summary(y ~ age + sex, fun=roc)


options(digits=3)
summary(y ~ age + sex, fun=roc, method="cross")

#Use stratify() to produce a table in which time intervals go down the
#page and going across 3 continuous variables are summarized using
#quartiles, and are stratified by two treatments

set.seed(1)
d <- expand.grid(visit=1:5, treat=c('A','B'), reps=1:100)
d$sysbp <- rnorm(100*5*2, 120, 10)
label(d$sysbp) <- 'Systolic BP'
d$diasbp <- rnorm(100*5*2, 80,  7)
d$diasbp[1] <- NA
d$age    <- rnorm(100*5*2, 50, 12)
g <- function(y) {
  N <- apply(y, 2, function(w) sum(!is.na(w)))
  h <- function(x) {
    qu <- quantile(x, c(.25,.5,.75), na.rm=TRUE)
    names(qu) <- c('Q1','Q2','Q3')
    c(N=sum(!is.na(x)), qu)
}
  w <- as.vector(apply(y, 2, h))
  names(w) <- as.vector( outer(c('N','Q1','Q2','Q3'), dimnames(y)[[2]],
                                function(x,y) paste(y,x)))
  w
}
#Use na.rm=FALSE to count NAs separately by column
s <- summary(cbind(age,sysbp,diasbp) ~ visit + stratify(treat),
             na.rm=FALSE, fun=g, data=d)
#The result is very wide.  Re-do, putting treatment vertically
x <- with(d, factor(paste('Visit', visit, treat)))
summary(cbind(age,sysbp,diasbp) ~ x, na.rm=FALSE, fun=g, data=d)

#Compose LaTeX code directly
g <- function(y) {
  h <- function(x) {
    qu <- format(round(quantile(x, c(.25,.5,.75), na.rm=TRUE),1),nsmall=1)
    paste('{\\\\scriptsize(',sum(!is.na(x)),
          ')} \\\\hfill{\\\\scriptsize ', qu[1], '} \\\\textbf{', qu[2],
          '} {\\\\scriptsize ', qu[3],'}', sep='')
  }
  apply(y, 2, h)
}
s <- summary(cbind(age,sysbp,diasbp) ~ visit + stratify(treat),
             na.rm=FALSE, fun=g, data=d)
# latex(s, prn=FALSE)
## need option in latex to not print n
#Put treatment vertically
s <- summary(cbind(age,sysbp,diasbp) ~ x, fun=g, data=d, na.rm=FALSE)
# latex(s, prn=FALSE)

#Plot estimated mean life length (assuming an exponential distribution) 
#separately by levels of 4 other variables.  Repeat the analysis
#by levels of a stratification variable, drug.  Automatically break
#continuous variables into tertiles.
#We are using the default, method='response'
\dontrun{
life.expect <- function(y) c(Years=sum(y[,1])/sum(y[,2]))
attach(pbc)
require(survival)
S <- Surv(follow.up.time, death)
s2 <- summary(S ~ age + albumin + ascites + edema + stratify(drug),
                         fun=life.expect, g=3)


#Note: You can summarize other response variables using the same 
#independent variables using e.g. update(s2, response~.), or you 
#can change the list of independent variables using e.g. 
#update(s2, response ~.- ascites) or update(s2, .~.-ascites)
#You can also print, typeset, or plot subsets of s2, e.g.
#plot(s2[c('age','albumin'),]) or plot(s2[1:2,])


s2    # invokes print.summary.formula.response


#Plot results as a separate dot chart for each of the 3 strata levels
par(mfrow=c(2,2))
plot(s2, cex.labels=.6, xlim=c(0,40), superposeStrata=FALSE)


#Typeset table, creating s2.tex
w <- latex(s2, cdec=1)
#Typeset table but just print LaTeX code
latex(s2, file="")    # useful for Sweave


#Take control of groups used for age.  Compute 3 quartiles for
#both cholesterol and bilirubin (excluding observations that are missing
#on EITHER ONE)


age.groups <- cut2(age, c(45,60))
g <- function(y) apply(y, 2, quantile, c(.25,.5,.75))
y <- cbind(Chol=chol,Bili=bili)
label(y) <- 'Cholesterol and Bilirubin'
#You can give new column names that are not legal S names
#by enclosing them in quotes, e.g. 'Chol (mg/dl)'=chol


s <- summary(y ~ age.groups + ascites, fun=g)


par(mfrow=c(1,2), oma=c(3,0,3,0))   # allow outer margins for overall
for(ivar in 1:2) {                  # title 
  isub <- (1:3)+(ivar-1)*3          # *3=number of quantiles/var.
  plot(s3, which=isub, main='', 
       xlab=c('Cholesterol','Bilirubin')[ivar],
       pch=c(91,16,93))            # [, closed circle, ]
  }
mtext(paste('Quartiles of', label(y)), adj=.5, outer=TRUE, cex=1.75)  
#Overall (outer) title


prlatex(latex(s3, trios=TRUE)) 
# trios -> collapse 3 quartiles


#Summarize only bilirubin, but do it with two statistics:
#the mean and the median.  Make separate tables for the two randomized
#groups and make plots for the active arm.


g <- function(y) c(Mean=mean(y), Median=median(y))


for(sub in c("D-penicillamine", "placebo")) {
  ss <- summary(bili ~ age.groups + ascites + chol, fun=g,
                subset=drug==sub)
  cat('\n',sub,'\n\n')
  print(ss)


  if(sub=='D-penicillamine') {
    par(mfrow=c(1,1))
    plot(s4, which=1:2, dotfont=c(1,-1), subtitles=FALSE, main='')
    #1=mean, 2=median     -1 font = open circle
    title(sub='Closed circle: mean;  Open circle: median', adj=0)
    title(sub=sub, adj=1)
  }


  w <- latex(ss, append=TRUE, fi='my.tex', 
             label=if(sub=='placebo') 's4b' else 's4a',
             caption=paste(label(bili),' {\\\\em (',sub,')}', sep=''))
  #Note symbolic labels for tables for two subsets: s4a, s4b
  prlatex(w)
}


#Now consider examples in 'reverse' format, where the lone dependent
#variable tells the summary function how to stratify all the 
#'independent' variables.  This is typically used to make tables 
#comparing baseline variables by treatment group, for example.


s5 <- summary(drug ~ bili + albumin + stage + protime + sex + 
                     age + spiders,
              method='reverse')
#To summarize all variables, use summary(drug ~., data=pbc)
#To summarize all variables with no stratification, use
#summary(~a+b+c) or summary(~.,data=\dots)


options(digits=1)
print(s5, npct='both')
#npct='both' : print both numerators and denominators
plot(s5, which='categorical')
Key(locator(1))  # draw legend at mouse click
par(oma=c(3,0,0,0))  # leave outer margin at bottom
plot(s5, which='continuous')
Key2()           # draw legend at lower left corner of plot
                 # oma= above makes this default key fit the page better


options(digits=3)
w <- latex(s5, npct='both', here=TRUE)     
# creates s5.tex


#Turn to a different dataset and do cross-classifications on possibly 
#more than one independent variable.  The summary function with 
#method='cross' produces a data frame containing the cross-
#classifications.  This data frame is suitable for multi-panel 
#trellis displays, although `summarize' works better for that.


attach(prostate)
size.quartile <- cut2(sz, g=4)
bone <- factor(bm,labels=c("no mets","bone mets"))


s7 <- summary(ap>1 ~ size.quartile + bone, method='cross')
#In this case, quartiles are the default so could have said sz + bone


options(digits=3)
print(s7, twoway=FALSE)
s7   # same as print(s7)
w <- latex(s7, here=TRUE)   # Make s7.tex


library(trellis,TRUE)
invisible(ps.options(reset=TRUE))
trellis.device(postscript, file='demo2.ps')


dotplot(S ~ size.quartile|bone, data=s7, #s7 is name of summary stats
                  xlab="Fraction ap>1", ylab="Quartile of Tumor Size")
#Can do this more quickly with summarize:
# s7 <- summarize(ap>1, llist(size=cut2(sz, g=4), bone), mean,
#                 stat.name='Proportion')
# dotplot(Proportion ~ size | bone, data=s7)


summary(age ~ stage, method='cross')
summary(age ~ stage, fun=quantile, method='cross')
summary(age ~ stage, fun=smean.sd, method='cross')
summary(age ~ stage, fun=smedian.hilow, method='cross')
summary(age ~ stage, fun=function(x) c(Mean=mean(x), Median=median(x)),
        method='cross')
#The next statements print real two-way tables
summary(cbind(age,ap) ~ stage + bone, 
        fun=function(y) apply(y, 2, quantile, c(.25,.75)),
        method='cross')
options(digits=2)
summary(log(ap) ~ sz + bone,
        fun=function(y) c(Mean=mean(y), quantile(y)),
        method='cross')


#Summarize an ordered categorical response by all of the needed
#cumulative proportions
summary(cumcategory(disease.severity) ~ age + sex)

}
}
\keyword{category}
\keyword{interface}
\keyword{hplot}
\keyword{manip}
\concept{grouping}
\concept{stratification}
\concept{aggregation}
\concept{cross-classification}
