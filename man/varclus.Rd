\name{varclus}
\alias{varclus}
\alias{print.varclus}
\alias{plot.varclus}
\alias{naclus}
\alias{naplot}
\alias{plotMultSim}
\alias{na.pattern}
\title{
Variable Clustering
}
\description{
Does a hierarchical cluster analysis on variables, using the Hoeffding
D statistic, squared Pearson or Spearman correlations, or proportion
of observations for which two variables are both positive as similarity
measures.  Variable clustering is used for assessing collinearity,
redundancy, and for separating variables into clusters that can be
scored as a single variable, thus resulting in data reduction.  For
computing any of the three similarity measures, pairwise deletion of
NAs is done.  The clustering is done by \code{hclust()}.  A small function
\code{naclus} is also provided which depicts similarities in which
observations are missing for variables in a data frame.  The
similarity measure is the fraction of \code{NAs} in common between any two
variables.  The diagonals of this \code{sim} matrix are the fraction of NAs
in each variable by itself.  \code{naclus} also computes \code{na.per.obs}, the
number of missing variables in each observation, and \code{mean.na}, a
vector whose ith element is the mean number of missing variables other
than variable i, for observations in which variable i is missing.  The
\code{naplot} function makes several plots (see the \code{which} argument).

So as to not generate too many dummy variables for multi-valued
character or categorical predictors, \code{varclus} will automatically
combine infrequent cells of such variables using
\code{\link{combine.levels}}.

\code{plotMultSim} plots multiple similarity matrices, with the similarity
measure being on the x-axis of each subplot.

\code{na.pattern} prints a frequency table of all combinations of
missingness for multiple variables.  If there are 3 variables, a
frequency table entry labeled \code{110} corresponds to the number of
observations for which the first and second variables were missing but
the third variable was not missing.
}
\usage{
varclus(x, similarity=c("spearman","pearson","hoeffding","bothpos","ccbothpos"),
        type=c("data.matrix","similarity.matrix"), 
        method="complete",
        data=NULL, subset=NULL, na.action=na.retain,
        trans=c("square", "abs", "none"), ...)
\method{print}{varclus}(x, abbrev=FALSE, ...)
\method{plot}{varclus}(x, ylab, abbrev=FALSE, legend.=FALSE, loc, maxlen, labels, \dots)

naclus(df, method)
naplot(obj, which=c('all','na per var','na per obs','mean na',
                    'na per var vs mean na'), \dots)

plotMultSim(s, x=1:dim(s)[3],
            slim=range(pretty(c(0,max(s,na.rm=TRUE)))),
            slimds=FALSE,
            add=FALSE, lty=par('lty'), col=par('col'),
            lwd=par('lwd'), vname=NULL, h=.5, w=.75, u=.05,
            labelx=TRUE, xspace=.35)

na.pattern(x)
}
\arguments{
\item{x}{
a formula,
a numeric matrix of predictors, or a similarity matrix.  If \code{x} is
a formula, \code{model.matrix} is used to convert it to a design matrix.
If the formula excludes an intercept (e.g., \code{~ a + b -1}),
the first categorical (\code{factor}) variable in the formula will have
dummy variables generated for all levels instead of omitting one for
the first level.  For
\code{plot} and \code{print}, \code{x} is an object created by
\code{varclus}.  For \code{na.pattern}, \code{x} is a data table, data frame,
or matrix.

For \code{plotMultSim}, is a numeric vector specifying the ordered
unique values on the x-axis, corresponding to the third dimension of
\code{s}.
}
\item{df}{a data frame}
\item{s}{
an array of similarity matrices.  The third dimension of this array
corresponds to different computations of similarities.  The first two
dimensions come from a single similarity matrix.  This is useful for
displaying similarity matrices computed by \code{varclus}, for example.  A
use for this might be to show pairwise similarities of variables
across time in a longitudinal study (see the example below).  If
\code{vname} is not given, \code{s} must have \code{dimnames}.
}
\item{similarity}{
the default is to use squared Spearman correlation coefficients, which
will detect monotonic but nonlinear relationships.  You can also
specify linear correlation or Hoeffding's (1948) D statistic, which
has the advantage of being sensitive to many types
of dependence, including highly non-monotonic relationships.  For
binary data, or data to be made binary, \code{similarity="bothpos"} uses as
a similarity measure the proportion of observations for which two
variables are both positive.  \code{similarity="ccbothpos"} uses a
chance-corrected measure which is the proportion of observations for
which both variables are positive minus the product of the two
marginal proportions.  This difference is expected to be zero under
independence.  For diagonals, \code{"ccbothpos"} still uses the proportion
of positives for the single variable.  So \code{"ccbothpos"} is not really
a similarity measure, and clustering is not done.  This measure is
useful for plotting with \code{plotMultSim} (see the last example).
}
\item{type}{
if \code{x} is not a formula, it may be a data matrix or a similarity matrix.
By default, it is assumed to be a data matrix.
}
\item{method}{
see \code{hclust}.  The default, for both \code{varclus} and \code{naclus}, is
\code{"compact"} (for \R it is \code{"complete"}).
}
\item{data}{a data frame, data table, or list}
\item{subset}{a standard subsetting expression}
\item{na.action}{
These may be specified if \code{x} is a formula.  The default
\code{na.action} is \code{na.retain}, defined by \code{varclus}.  This
causes all observations to be kept in the model frame, with later
pairwise deletion of \code{NA}s.}
\item{trans}{By default, when the similarity measure is based on
	Pearson's or Spearman's correlation coefficients, the coefficients are
	squared.  Specify \code{trans="abs"} to take absolute values or
	\code{trans="none"} to use the coefficients as they stand.}
\item{...}{for \code{varclus} these are optional arguments to pass to
  the \code{\link{dataframeReduce}} function.  Otherwise,
passed to \code{plclust} (or to \code{dotchart} or \code{dotchart2} for
\code{naplot}).
}
\item{ylab}{
y-axis label.  Default is constructed on the basis of \code{similarity}.
}
\item{legend.}{
set to \code{TRUE} to plot a legend defining the abbreviations
}
\item{loc}{
a list with elements \code{x} and \code{y} defining coordinates of the
upper left corner of the legend.  Default is \code{locator(1)}.
}
\item{maxlen}{
if a legend is plotted describing abbreviations, original labels
longer than \code{maxlen} characters are truncated at \code{maxlen}.
}
\item{labels}{
a vector of character strings containing labels corresponding to
columns in the similar matrix, if the column names of that matrix are
not to be used
}
\item{obj}{an object created by \code{naclus}}
\item{which}{
defaults to \code{"all"} meaning to have \code{naplot} make 4 separate
plots.  To 
make only one of the plots, use \code{which="na per var"} (dot chart of
fraction of NAs for each variable), ,\code{"na per obs"} (dot chart showing
frequency distribution of number of variables having NAs in an
observation), \code{"mean na"} (dot chart showing mean number of other
variables missing when the indicated variable is missing), or 
\code{"na per var vs mean na"}, a scatterplot showing on the x-axis the
fraction of NAs in the variable and on the y-axis the mean number of
other variables that are NA when the indicated variable is NA.
}
\item{abbrev}{
set to \code{TRUE} to abbreviate variable names for plotting or
printing.  Is set to \code{TRUE} automatically if \code{legend=TRUE}.
}
\item{slim}{
2-vector specifying the range of similarity values for scaling the
y-axes.  By default this is the observed range over all of \code{s}.
}
\item{slimds}{set to \code{slimds} to \code{TRUE} to scale diagonals and
off-diagonals separately}
\item{add}{
set to \code{TRUE} to add similarities to an existing plot (usually
specifying \code{lty} or \code{col})
}
\item{lty, col, lwd}{
line type, color, or line thickness for \code{plotMultSim}
}
\item{vname}{
optional vector of variable names, in order, used in \code{s}
}
\item{h}{
relative height for subplot
}
\item{w}{
relative width for subplot
}
\item{u}{
relative extra height and width to leave unused inside the subplot.
Also used as the space between y-axis tick mark labels and graph border.
}
\item{labelx}{
  set to \code{FALSE} to suppress drawing of labels in the x direction
}
\item{xspace}{
  amount of space, on a scale of 1:\code{n} where \code{n} is the number
  of variables, to set aside for y-axis labels
}
}
\value{
for \code{varclus} or \code{naclus}, a list of class \code{varclus} with elements
\code{call} (containing the calling statement), \code{sim} (similarity matrix),
\code{n} (sample size used if \code{x} was not a correlation matrix already -
\code{n} is a matrix), \code{hclust}, the object created by \code{hclust},
\code{similarity}, and \code{method}.  \code{naclus} also returns the
two vectors listed under 
description, and \code{naplot} returns an invisible vector that is the
frequency table of the number of missing variables per observation.
\code{plotMultSim} invisibly returns the limits of similarities used in
constructing the y-axes of each subplot.  For \code{similarity="ccbothpos"}
the \code{hclust} object is \code{NULL}.

\code{na.pattern} creates an integer vector of frequencies.
}
\details{
\code{options(contrasts= c("contr.treatment", "contr.poly"))} is issued 
temporarily by \code{varclus} to make sure that ordinary dummy variables
are generated for \code{factor} variables.  Pass arguments to the
\code{\link{dataframeReduce}} function to remove problematic variables
(especially if analyzing all variables in a data frame).
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
\references{
Sarle, WS: The VARCLUS Procedure.  SAS/STAT User's Guide, 4th Edition,
1990.  Cary NC: SAS Institute, Inc.


Hoeffding W. (1948): A non-parametric test of independence.  Ann Math Stat
19:546--57.
}
\seealso{
\code{\link{hclust}}, \code{\link{plclust}}, \code{\link{hoeffd}}, \code{\link{rcorr}}, \code{\link{cor}}, \code{\link{model.matrix}},
\code{\link{locator}}, \code{\link{na.pattern}}, \code{\link{cut2}}, \code{\link{combine.levels}}
}
\examples{
set.seed(1)
x1 <- rnorm(200)
x2 <- rnorm(200)
x3 <- x1 + x2 + rnorm(200)
x4 <- x2 + rnorm(200)
x <- cbind(x1,x2,x3,x4)
v <- varclus(x, similarity="spear")  # spearman is the default anyway
v    # invokes print.varclus
print(round(v$sim,2))
plot(v)

# Convert the dendrogram to be horizontal
v <- as.dendrogram(v$hclust)
plot(v, horiz=TRUE, axes=FALSE, xlab=expression(paste('Spearman ', rho^2)))
rh <- seq(0, 1, by=0.1)  # re-label x-axis re:similarity not distance
axis(1, at=1 - rh, labels=format(rh))

# plot(varclus(~ age + sys.bp + dias.bp + country - 1), abbrev=TRUE)
# the -1 causes k dummies to be generated for k countries
# plot(varclus(~ age + factor(disease.code) - 1))
#
#
# use varclus(~., data= fracmiss= maxlevels= minprev=) to analyze all
# "useful" variables - see dataframeReduce for details about arguments


df <- data.frame(a=c(1,2,3),b=c(1,2,3),c=c(1,2,NA),d=c(1,NA,3),
                 e=c(1,NA,3),f=c(NA,NA,NA),g=c(NA,2,3),h=c(NA,NA,3))
par(mfrow=c(2,2))
for(m in c("ward","complete","median")) {
  plot(naclus(df, method=m))
  title(m)
}
naplot(naclus(df))
n <- naclus(df)
plot(n); naplot(n)
na.pattern(df)

# plotMultSim example: Plot proportion of observations
# for which two variables are both positive (diagonals
# show the proportion of observations for which the
# one variable is positive).  Chance-correct the
# off-diagonals by subtracting the product of the
# marginal proportions.  On each subplot the x-axis
# shows month (0, 4, 8, 12) and there is a separate
# curve for females and males
d <- data.frame(sex=sample(c('female','male'),1000,TRUE),
                month=sample(c(0,4,8,12),1000,TRUE),
                x1=sample(0:1,1000,TRUE),
                x2=sample(0:1,1000,TRUE),
                x3=sample(0:1,1000,TRUE))
s <- array(NA, c(3,3,4))
opar <- par(mar=c(0,0,4.1,0))  # waste less space
for(sx in c('female','male')) {
  for(i in 1:4) {
    mon <- (i-1)*4
    s[,,i] <- varclus(~x1 + x2 + x3, sim='ccbothpos', data=d,
                      subset=d$month==mon & d$sex==sx)$sim
    }
  plotMultSim(s, c(0,4,8,12), vname=c('x1','x2','x3'),
              add=sx=='male', slimds=TRUE,
              lty=1+(sx=='male'))
  # slimds=TRUE causes separate  scaling for diagonals and
  # off-diagonals
}
par(opar)
}
\keyword{cluster}
\keyword{multivariate}
\keyword{category}
\keyword{manip}


