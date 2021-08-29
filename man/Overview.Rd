\name{HmiscOverview}
\alias{HmiscOverview}
\alias{Hmisc.Overview}
\title{
  Overview of Hmisc Library
}
\description{
  The Hmisc library contains many functions useful for data analysis,
  high-level graphics, utility operations, functions for computing
  sample size and power, translating SAS datasets into \R, imputing
  missing values, advanced table making, variable clustering, character
  string manipulation, conversion of \R objects to LaTeX code, recoding
  variables, and bootstrap repeated measures analysis.  Most of these
  functions were written by F Harrell, but a few were collected from
  statlib and from s-news; other authors are indicated below.  This
  collection of functions includes all of  Harrell's submissions to
  statlib other than  the functions in the \pkg{rms} and display
  libraries.  A few of the functions do not  have \dQuote{Help}
  documentation.
  
  To make \pkg{Hmisc} load silently, issue
  \code{options(Hverbose=FALSE)} before \code{library(Hmisc)}.
}
\section{Functions}{
\tabular{ll}{
\bold{Function Name} \tab  \bold{Purpose} \cr
abs.error.pred  \tab Computes various indexes of predictive accuracy based\cr
\tab    on absolute errors, for linear models\cr
addMarginal     \tab Add marginal observations over selected variables\cr
all.is.numeric  \tab Check if character strings are legal numerics\cr
approxExtrap    \tab Linear extrapolation\cr
aregImpute      \tab Multiple imputation based on additive regression,\cr
                \tab     bootstrapping, and predictive mean matching\cr
areg.boot       \tab Nonparametrically estimate transformations for both\cr
                \tab     sides of a multiple additive regression, and\cr
                \tab     bootstrap these estimates and \eqn{R^2}\cr
ballocation     \tab Optimum sample allocations in 2-sample proportion test\cr
binconf         \tab Exact confidence limits for a proportion and more accurate\cr
                \tab     (narrower!) score stat.-based Wilson interval\cr
                \tab     (Rollin Brant, mod. FEH)\cr
bootkm          \tab Bootstrap Kaplan-Meier survival or quantile estimates\cr
bpower          \tab Approximate power of 2-sided test for 2 proportions\cr
                \tab     Includes bpower.sim for exact power by simulation\cr
bpplot          \tab Box-Percentile plot \cr
                \tab     (Jeffrey Banfield, \email{umsfjban@bill.oscs.montana.edu})\cr
bpplotM         \tab Chart extended box plots for multiple variables\cr
bsamsize        \tab Sample size requirements for test of 2 proportions\cr
bystats         \tab Statistics on a single variable by levels of >=1 factors\cr
bystats2        \tab 2-way statistics\cr
character.table \tab Shows numeric equivalents of all latin characters\cr
                \tab     Useful for putting many special chars. in graph titles\cr
                \tab     (Pierre Joyet, \email{pierre.joyet@bluewin.ch})\cr
ciapower        \tab Power of Cox interaction test\cr
cleanup.import  \tab More compactly store variables in a data frame, and clean up\cr
                \tab     problem data when e.g. Excel spreadsheet had a non-\cr
                \tab     numeric value in a numeric column\cr
combine.levels  \tab Combine infrequent levels of a categorical variable\cr
confbar         \tab Draws confidence bars on an existing plot using multiple\cr
                \tab     confidence levels distinguished using color or gray scale\cr
contents        \tab Print the contents (variables, labels, etc.) of a data frame\cr
cpower          \tab Power of Cox 2-sample test allowing for noncompliance\cr
Cs              \tab Vector of character strings from list of unquoted names\cr
csv.get         \tab Enhanced importing of comma separated files labels\cr
cut2            \tab Like cut with better endpoint label construction and allows\cr
                \tab     construction of quantile groups or groups with given n\cr
datadensity     \tab Snapshot graph of distributions of all variables in\cr
                \tab     a data frame.  For continuous variables uses scat1d.\cr
dataRep         \tab Quantify representation of new observations in a database\cr
ddmmmyy         \tab SAS \dQuote{date7} output format for a chron object\cr
deff            \tab Kish design effect and intra-cluster correlation\cr
describe        \tab Function to describe different classes of objects.\cr
                \tab     Invoke by saying describe(object). It calls one of the\cr
                \tab     following:\cr
describe.data.frame
                \tab Describe all variables in a data frame (generalization\cr
                \tab     of SAS UNIVARIATE)\cr
describe.default
                \tab Describe a variable (generalization of SAS UNIVARIATE)\cr
dotplot3        \tab A more flexible version of dotplot\cr
Dotplot         \tab Enhancement of Trellis dotplot allowing for matrix\cr
                \tab     x-var., auto generation of Key function, superposition\cr
drawPlot        \tab Simple mouse-driven drawing program, including a function\cr
                \tab     for fitting Bezier curves\cr
Ecdf            \tab Empirical cumulative distribution function plot\cr
errbar          \tab Plot with error bars (Charles Geyer, U. Chi., mod FEH)\cr
event.chart     \tab Plot general event charts (Jack Lee, \email{jjlee@mdanderson.org}, \cr
                \tab     Ken Hess, Joel Dubin; Am Statistician 54:63-70,2000)\cr
event.history	\tab Event history chart with time-dependent cov. status\cr
                \tab     (Joel Dubin, \email{jdubin@uwaterloo.ca})\cr
find.matches    \tab Find matches (with tolerances) between columns of 2 matrices\cr
first.word      \tab Find the first word in an \R expression (R Heiberger)\cr
fit.mult.impute \tab Fit most regression models over multiple transcan imputations,\cr
                \tab     compute imputation-adjusted variances and avg. betas\cr
format.df       \tab Format a matrix or data frame with much user control\cr
                \tab     (R Heiberger and FE Harrell)\cr
ftupwr          \tab Power of 2-sample binomial test using Fleiss, Tytun, Ury\cr
ftuss           \tab Sample size for 2-sample binomial test using  "  "  "  "\cr
                \tab     (Both by Dan Heitjan, \email{dheitjan@biostats.hmc.psu.edu})\cr
gbayes          \tab Bayesian posterior and predictive distributions when both\cr
                \tab the prior and the likelihood are Gaussian\cr
getHdata        \tab Fetch and list datasets on our web site\cr
hdquantile      \tab Harrell-Davis nonparametric quantile estimator with s.e.\cr
histbackback    \tab Back-to-back histograms (Pat Burns, Salomon Smith\cr
                \tab     Barney, London, \email{pburns@dorado.sbi.com})\cr
hist.data.frame \tab Matrix of histograms for all numeric vars. in data frame\cr
                \tab     Use hist.data.frame(data.frame.name)\cr
histSpike       \tab Add high-resolution spike histograms or density estimates\cr
                \tab     to an existing plot\cr
hoeffd          \tab Hoeffding's D test (omnibus test of independence of X and Y)\cr
impute          \tab Impute missing data (generic method)\cr
interaction     \tab More flexible version of builtin function\cr
is.present      \tab Tests for non-blank character values or non-NA numeric values\cr
james.stein     \tab James-Stein shrinkage estimates of cell means from raw data\cr
labcurve        \tab Optimally label a set of curves that have been drawn on\cr
                \tab     an existing plot, on the basis of gaps between curves.\cr
                \tab     Also position legends automatically at emptiest rectangle.\cr
label           \tab Set or fetch a label for an \R-object\cr
Lag             \tab Lag a vector, padding on the left with NA or ''\cr
latex           \tab Convert an \R object to LaTeX (R Heiberger & FE Harrell)\cr
list.tree       \tab Pretty-print the structure of any data object\cr
                \tab     (Alan Zaslavsky, \email{zaslavsk@hcp.med.harvard.edu})\cr
Load            \tab Enhancement of \code{load}\cr
mask            \tab 8-bit logical representation of a short integer value\cr
                \tab     (Rick Becker)\cr
matchCases      \tab Match each case on one continuous variable\cr
matxv           \tab Fast matrix * vector, handling intercept(s) and NAs\cr
mgp.axis        \tab Version of axis() that uses appropriate mgp from \cr
                \tab     mgp.axis.labels and gets around bug in axis(2, ...)\cr
                \tab     that causes it to assume las=1\cr
mgp.axis.labels
                \tab Used by survplot and plot in \pkg{rms} library (and other\cr
                \tab     functions in the future) so that different spacing\cr
                \tab     between tick marks and axis tick mark labels may be\cr
                \tab     specified for x- and y-axes.  \cr
                \tab     Use mgp.axis.labels('default') to set defaults.\cr
                \tab     Users can set values manually using \cr
                \tab     mgp.axis.labels(x,y) where x and y are 2nd value of\cr
                \tab     par('mgp') to use.  Use mgp.axis.labels(type=w) to\cr
                \tab     retrieve values, where w='x', 'y', 'x and y', 'xy',\cr
                \tab     to get 3 mgp values (first 3 types) or 2 mgp.axis.labels.\cr
minor.tick      \tab Add minor tick marks to an existing plot\cr
mtitle          \tab Add outer titles and subtitles to a multiple plot layout\cr
multLines       \tab Draw multiple vertical lines at each x\cr
                \tab in a line plot\cr
\%nin\%         \tab Opposite of \%in\%\cr
nobsY           \tab Compute no. non-NA observations for left hand formula side\cr
nomiss          \tab Return a matrix after excluding any row with an NA\cr
panel.bpplot    \tab Panel function for trellis bwplot - box-percentile plots\cr
panel.plsmo     \tab Panel function for trellis xyplot - uses plsmo\cr
pBlock          \tab Block variables for certain lattice charts\cr
pc1             \tab Compute first prin. component and get coefficients on\cr\tab     original scale of variables\cr
plotCorrPrecision  \tab Plot precision of estimate of correlation coefficient\cr
plsmo           \tab Plot smoothed x vs. y with labeling and exclusion of NAs\cr
                \tab     Also allows a grouping variable and plots unsmoothed data\cr
popower         \tab Power and sample size calculations for ordinal responses\cr
                \tab     (two treatments, proportional odds model)\cr
prn             \tab prn(expression) does print(expression) but titles the\cr
                \tab     output with 'expression'.  Do prn(expression,txt) to add\cr
                \tab     a heading (\sQuote{txt}) before the \sQuote{expression} title\cr
pstamp          \tab Stamp a plot with date in lower right corner (pstamp())\cr
                \tab     Add ,pwd=T and/or ,time=T to add current directory \cr
                \tab      name or time\cr
                \tab     Put additional text for label as first argument, e.g.\cr
                \tab     pstamp('Figure 1')  will draw 'Figure 1  date'\cr
putKey          \tab Different way to use key()\cr
putKeyEmpty     \tab Put key at most empty part of existing plot\cr
rcorr           \tab Pearson or Spearman correlation matrix with pairwise deletion\cr
                \tab     of missing data\cr
rcorr.cens      \tab Somers' Dxy rank correlation with censored data\cr
rcorrp.cens     \tab Assess difference in concordance for paired predictors\cr
rcspline.eval   \tab Evaluate restricted cubic spline design matrix\cr
rcspline.plot   \tab Plot spline fit with nonparametric smooth and grouped estimates\cr
rcspline.restate
                \tab Restate restricted cubic spline in unrestricted form, and\cr
                \tab     create TeX expression to print the fitted function\cr
reShape         \tab Reshape a matrix into 3 vectors, reshape serial data\cr
rm.boot         \tab Bootstrap spline fit to repeated measurements model,\cr
                \tab     with simultaneous confidence region - least\cr
                \tab     squares using spline function in time\cr
rMultinom       \tab Generate multinomial random variables with varying prob.\cr
samplesize.bin  \tab Sample size for 2-sample binomial problem\cr
                \tab     (Rick Chappell, \email{chappell@stat.wisc.edu})\cr
sas.get         \tab Convert SAS dataset to S data frame\cr
sasxport.get    \tab Enhanced importing of SAS transport dataset in R\cr
Save            \tab Enhancement of \code{save}\cr
scat1d          \tab Add 1-dimensional scatterplot to an axis of an existing plot\cr
                \tab     (like bar-codes, FEH/Martin Maechler, \cr
                \tab     \email{maechler@stat.math.ethz.ch}/Jens Oehlschlaegel-Akiyoshi,\cr
                \tab     \email{oehl@psyres-stuttgart.de})\cr
score.binary    \tab Construct a score from a series of binary variables or\cr
                \tab     expressions\cr
sedit           \tab A set of character handling functions written entirely\cr
                \tab     in \R.  sedit() does much of what the UNIX sed\cr
                \tab     program does.  Other functions included are\cr
                \tab     substring.location, substring<-, replace.string.wild,\cr
                \tab     and functions to check if a string is numeric or\cr
                \tab     contains only the digits 0-9\cr
setTrellis      \tab Set Trellis graphics to use blank conditioning panel strips,\cr
                \tab     line thickness 1 for dot plot reference lines: \cr
                \tab     setTrellis(); 3 optional arguments\cr
show.col        \tab Show colors corresponding to col=0,1,...,99\cr
show.pch        \tab Show all plotting characters specified by pch=.\cr
                \tab     Just type show.pch() to draw the table on the \cr
                \tab     current device.  \cr
showPsfrag      \tab Use LaTeX to compile, and dvips and ghostview to\cr
                \tab    display a postscript graphic containing psfrag strings\cr
solvet          \tab Version of solve with argument tol passed to qr\cr
somers2         \tab Somers' rank correlation and c-index for binary y\cr
spearman        \tab Spearman rank correlation coefficient  spearman(x,y)\cr
spearman.test   \tab Spearman 1 d.f. and 2 d.f. rank correlation test\cr
spearman2       \tab Spearman multiple d.f. \eqn{\rho^2}{rho^2}, adjusted \eqn{\rho^2}{rho^2}, Wilcoxon-Kruskal-\cr
                \tab     Wallis test, for multiple predictors\cr
spower          \tab Simulate power of 2-sample test for survival under\cr
                \tab     complex conditions\cr
                \tab     Also contains the Gompertz2,Weibull2,Lognorm2
functions.\cr
spss.get        \tab Enhanced importing of SPSS files using read.spss
function \cr
src             \tab src(name) = source("name.s") with memory\cr
store           \tab store an object permanently (easy interface to assign function)\cr
strmatch        \tab Shortest unique identifier match \cr
                \tab     (Terry Therneau, \email{therneau@mayo.edu})\cr
subset          \tab More easily subset a data frame\cr
substi          \tab Substitute one var for another when observations NA\cr
summarize       \tab Generate a data frame containing stratified summary\cr
                \tab     statistics.  Useful for passing to trellis.\cr
summary.formula \tab General table making and plotting functions for summarizing\cr
                \tab data\cr
summaryD        \tab Summarizing using user-provided formula and dotchart3\cr
summaryM        \tab Replacement for summary.formula(..., method='reverse')\cr
summaryP        \tab Multi-panel dot chart for summarizing proportions\cr
summaryS        \tab Summarize multiple response variables for multi-panel\cr
                \tab dot chart or scatterplot\cr
summaryRc       \tab Summary for continuous variables using lowess\cr
symbol.freq     \tab X-Y Frequency plot with circles' area prop. to frequency\cr
sys             \tab Execute unix() or dos() depending on what's running\cr
tabulr          \tab Front-end to tabular function in the tables package\cr
tex             \tab Enclose a string with the correct syntax for using\cr
                \tab    with the LaTeX psfrag package, for postscript graphics\cr
transace        \tab ace() packaged for easily automatically transforming all\cr
                \tab     variables in a matrix\cr
transcan        \tab automatic transformation and imputation of NAs for a\cr
                \tab     series of predictor variables\cr
trap.rule       \tab Area under curve defined by arbitrary x and y vectors,\cr
                \tab using trapezoidal rule\cr
trellis.strip.blank
                \tab To make the strip titles in trellis more visible, you can \cr
                \tab     make the backgrounds blank by saying trellis.strip.blank().\cr
                \tab     Use before opening the graphics device.\cr
t.test.cluster  \tab 2-sample t-test for cluster-randomized observations\cr
uncbind         \tab Form individual variables from a matrix\cr
upData          \tab Update a data frame (change names, labels, remove vars, etc.)\cr
units           \tab Set or fetch "units" attribute - units of measurement for var.\cr
varclus         \tab Graph hierarchical clustering of variables using squared\cr
                \tab Pearson or Spearman correlations or Hoeffding D as similarities\cr
                \tab Also includes the naclus function for examining similarities in\cr
                \tab patterns of missing values across variables.\cr
wtd.mean \tab \cr wtd.var \tab \cr  wtd.quantile \tab \cr  wtd.Ecdf \tab
\cr wtd.table \tab \cr  wtd.rank \tab \cr wtd.loess.noiter \tab \cr
num.denom.setup \tab Set of function for obtaining weighted estimates\cr
xy.group        \tab Compute mean x vs. function of y by groups of x\cr
xYplot          \tab Like trellis xyplot but supports error bars and multiple\cr
                \tab     response variables that are connected as separate lines\cr
ynbind          \tab Combine a series of yes/no true/false present/absent variables into a matrix\cr
zoom            \tab Zoom in on any graphical display\cr \tab     (Bill Dunlap, \email{bill@statsci.com})
}}

\references{
See Alzola CF, Harrell FE (2004): An Introduction to S and the
Hmisc and Design Libraries at
\url{https://hbiostat.org/R/doc/sintro.pdf}
for extensive documentation and examples for the Hmisc package.
}

\section{Copyright Notice}{
\bold{GENERAL DISCLAIMER}\cr
This program is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2, or (at your option) any later version.\cr

This program is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU General Public License for more
details.\cr

In short: You may use it any way you like, as long as you
don't charge money for it, remove this notice, or hold anyone liable
for its results.  Also, please acknowledge the source and communicate
changes to the author.\cr

If this software is used is work presented for publication, kindly
reference it using for example:\cr
 Harrell FE (2014): Hmisc: A package of miscellaneous R functions.
  Programs available from \url{https://hbiostat.org/R/Hmisc/}.\cr
  Be sure to reference \R itself and other libraries used.
}
\author{
Frank E Harrell Jr\cr
Professor of Biostatistics\cr
Vanderbilt University School of Medicine\cr
Nashville, Tennessee\cr
\email{fh@fharrell.com}
}
\keyword{misc}
\concept{overview}
