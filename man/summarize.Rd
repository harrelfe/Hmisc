\name{summarize}
\alias{summarize}
\alias{mApply}
\alias{asNumericMatrix}
\alias{matrix2dataFrame}
\alias{subsAttr}
\title{Summarize Scalars or Matrices by Cross-Classification}
\description{
\code{summarize} is a fast version of \code{summary(formula,
method="cross",overall=FALSE)} for producing stratified summary statistics
and storing them in a data frame for plotting (especially with trellis
\code{xyplot} and \code{dotplot} and Hmisc \code{xYplot}).  Unlike
\code{aggregate}, \code{summarize} accepts a matrix as its first
argument and a multi-valued \code{FUN}
argument and \code{summarize} also labels the variables in the new data
frame using their original names.  Unlike methods based on
\code{tapply}, \code{summarize} stores the values of the stratification
variables using their original types, e.g., a numeric \code{by} variable
will remain a numeric variable in the collapsed data frame.
\code{summarize} also retains \code{"label"} attributes for variables.
\code{summarize} works especially well with the Hmisc \code{xYplot}
function for displaying multiple summaries of a single variable on each
panel, such as means and upper and lower confidence limits.

\code{mApply} is like \code{tapply} except that the first argument can
be a matrix, and the output is cleaned up if \code{simplify=TRUE}.  It
uses code adapted from Tony Plate (\email{tplate@blackmesacapital.com}) to
operate on grouped submatrices.

As \code{mApply} can be much faster than using \code{by}, it is often
worth the trouble of converting a data frame to a numeric matrix for
processing by \code{mApply}.  \code{asNumericMatrix} will do this, and
\code{matrix2dataFrame} will convert a numeric matrix back into a data
frame if attributes and storage modes of the original variables are
saved by calling \code{subsAttr}.  \code{subsAttr} saves attributes that
are commonly preserved across row subsetting (i.e., it does not save
\code{dim}, \code{dimnames}, or \code{names} attributes).
}
\usage{
summarize(X, by, FUN, \dots, 
          stat.name=deparse(substitute(X)),
          type=c('variables','matrix'), subset=TRUE)

mApply(X, INDEX, FUN=NULL, \dots, simplify=TRUE)

asNumericMatrix(x)

subsAttr(x)

matrix2dataFrame(x, at, restoreAll=TRUE)
}
\arguments{
\item{X}{
a vector or matrix capable of being operated on by the
function specified as the \code{FUN} argument
}
\item{by}{
one or more stratification variables.  If a single
variable, \code{by} may be a vector, otherwise it should be a list.
Using the Hmisc \code{llist} function instead of \code{list} will result
in individual variable names being accessible to \code{summarize}.  For
example, you can specify \code{llist(age.group,sex)} or
\code{llist(Age=age.group,sex)}.  The latter gives \code{age.group} a
new temporary name, \code{Age}. 
}
\item{FUN}{
a function of a single vector argument, used to create the statistical
summaries for \code{summarize}.  \code{FUN} may compute any number of
statistics. 
}
\item{simplify}{set to \code{FALSE} to suppress simplification of the
  result in to an array, matrix, etc.}
\item{...}{extra arguments are passed to \code{FUN}}
\item{stat.name}{
the name to use when creating the main summary variable.  By default,
the name of the \code{X} argument is used.  Set \code{stat.name} to
\code{NULL} to suppress this name replacement.
}
\item{type}{
Specify \code{type="matrix"} to store the summary variables (if there are
more than one) in a matrix.
}
\item{subset}{
a logical vector or integer vector of subscripts used to specify the
subset of data to use in the analysis.  The default is to use all
observations in the data frame.
}
\item{INDEX}{
vector or list of vectors to cross-classify on, similar to \code{by}.
See \code{tapply}.}
\item{x}{
  a data frame (for \code{asNumericMatrix}) or a numeric matrix (for
  \code{matrix2dataFrame}).  For \code{subsAttr}, \code{x} may be a data
  frame, list, or a vector.
}
\item{at}{
  result of \code{subsAttr}
}
\item{restoreAll}{
  set to \code{FALSE} to only restore attributes \code{label},
  \code{units}, and \code{levels} instead of all attributes
}
}
\value{
For \code{summarize}, a data frame containing the \code{by} variables and the
statistical summaries (the first of which is named the same as the \code{X}
variable unless \code{stat.name} is given).  If \code{type="matrix"}, the
summaries are stored in a single variable in the data frame, and this
variable is a matrix.  For \code{mApply}, the returned value is a vector,
matrix, or list.  If \code{FUN} returns more than one number, the result
is an array if \code{simplify=TRUE} and is a list otherwise.  If a
matrix is returned, its rows correspond to unique combinations of
\code{INDEX}.  If \code{INDEX} is a list with more than one vector,
\code{FUN} returns more than one number, and \code{simplify=FALSE}, the
returned value is a list that is an array with the first dimension
corresponding to the last vector in \code{INDEX}, the second dimension
corresponding to the next to last vector in \code{INDEX}, etc., and the
elements of the list-array correspond to the values computed by
\code{FUN}.  In this situation the returned value is a regular array if
\code{simplify=TRUE}.   The order of dimensions is as previously but the
additional (last) dimension corresponds to values computed by
\code{FUN}.  \code{asNumericMatrix} returns a numeric matrix, and
\code{matrix2dataFrame} returns a data frame.  \code{subsAttr} returns a
list of attribute lists if its argument is a list or data frame, and a
list containing attributes of a single variable.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
f.harrell@vanderbilt.edu
}
\seealso{
\code{\link{label}}, \code{\link{cut2}}, \code{\link{llist}}, \code{\link{by}}
}
\examples{
\dontrun{
s <- summarize(ap>1, llist(size=cut2(sz, g=4), bone), mean,
               stat.name='Proportion')
dotplot(Proportion ~ size | bone, data=s7)
}

set.seed(1)
temperature <- rnorm(300, 70, 10)
month <- sample(1:12, 300, TRUE)
year  <- sample(2000:2001, 300, TRUE)
g <- function(x)c(Mean=mean(x,na.rm=TRUE),Median=median(x,na.rm=TRUE))
summarize(temperature, month, g)
mApply(temperature, month, g)

mApply(temperature, month, mean, na.rm=TRUE)
w <- summarize(temperature, month, mean, na.rm=TRUE)
if(.R.) library(lattice)
xyplot(temperature ~ month, data=w) # plot mean temperature by month

w <- summarize(temperature, llist(year,month), 
               quantile, probs=c(.5,.25,.75), na.rm=TRUE, type='matrix')
xYplot(Cbind(temperature[,1],temperature[,-1]) ~ month | year, data=w)
mApply(temperature, llist(year,month),
       quantile, probs=c(.5,.25,.75), na.rm=TRUE)

# Compute the median and outer quartiles.  The outer quartiles are
# displayed using "error bars"
set.seed(111)
dfr <- expand.grid(month=1:12, year=c(1997,1998), reps=1:100)
attach(dfr)
y <- abs(month-6.5) + 2*runif(length(month)) + year-1997
s <- summarize(y, llist(month,year), smedian.hilow, conf.int=.5)
s
mApply(y, llist(month,year), smedian.hilow, conf.int=.5)

xYplot(Cbind(y,Lower,Upper) ~ month, groups=year, data=s, 
       keys='lines', method='alt')
# Can also do:
s <- summarize(y, llist(month,year), quantile, probs=c(.5,.25,.75),
               stat.name=c('y','Q1','Q3'))
xYplot(Cbind(y, Q1, Q3) ~ month, groups=year, data=s, keys='lines')
# To display means and bootstrapped nonparametric confidence intervals
# use for example:
s <- summarize(y, llist(month,year), smean.cl.boot)
xYplot(Cbind(y, Lower, Upper) ~ month | year, data=s)

# For each subject use the trapezoidal rule to compute the area under
# the (time,response) curve using the Hmisc trap.rule function
x <- cbind(time=c(1,2,4,7, 1,3,5,10),response=c(1,3,2,4, 1,3,2,4))
subject <- c(rep(1,4),rep(2,4))
trap.rule(x[1:4,1],x[1:4,2])
summarize(x, subject, function(y) trap.rule(y[,1],y[,2]))

\dontrun{
# Another approach would be to properly re-shape the mm array below
# This assumes no missing cells.  There are many other approaches.
# mApply will do this well while allowing for missing cells.
m <- tapply(y, list(year,month), quantile, probs=c(.25,.5,.75))
mm <- array(unlist(m), dim=c(3,2,12), 
            dimnames=list(c('lower','median','upper'),c('1997','1998'),
                          as.character(1:12)))
# aggregate will help but it only allows you to compute one quantile
# at a time; see also the Hmisc mApply function
dframe <- aggregate(y, list(Year=year,Month=month), quantile, probs=.5)

# Compute expected life length by race assuming an exponential
# distribution - can also use summarize
g <- function(y) { # computations for one race group
  futime <- y[,1]; event <- y[,2]
  sum(futime)/sum(event)  # assume event=1 for death, 0=alive
}
mApply(cbind(followup.time, death), race, g)

# To run mApply on a data frame:
m <- mApply(asNumericMatrix(x), race, h)
# Here assume h is a function that returns a matrix similar to x
at <- subsAttr(x)  # get original attributes and storage modes
matrix2dataFrame(m, at)


# Get stratified weighted means
g <- function(y) wtd.mean(y[,1],y[,2])
summarize(cbind(y, wts), llist(sex,race), g, stat.name='y')
mApply(cbind(y,wts), llist(sex,race), g)

# Compare speed of mApply vs. by for computing 
d <- data.frame(sex=sample(c('female','male'),100000,TRUE),
                country=sample(letters,100000,TRUE),
                y1=runif(100000), y2=runif(100000))
g <- function(x) {
  y <- c(median(x[,'y1']-x[,'y2']),
         med.sum =median(x[,'y1']+x[,'y2']))
  names(y) <- c('med.diff','med.sum')
  y
}

system.time(by(d, llist(sex=d$sex,country=d$country), g))
system.time({
             x <- asNumericMatrix(d)
             a <- subsAttr(d)
             m <- mApply(x, llist(sex=d$sex,country=d$country), g)
            })
system.time({
             x <- asNumericMatrix(d)
             summarize(x, llist(sex=d$sex, country=d$country), g)
            })

# An example where each subject has one record per diagnosis but sex of
# subject is duplicated for all the rows a subject has.  Get the cross-
# classified frequencies of diagnosis (dx) by sex and plot the results
# with a dot plot

count <- rep(1,length(dx))
d <- summarize(count, llist(dx,sex), sum)
Dotplot(dx ~ count | sex, data=d)
}
detach('dfr')
}
\keyword{category}
\keyword{manip}
\concept{grouping}
\concept{stratification}
\concept{aggregation}
\concept{cross-classification}

