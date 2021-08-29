\name{summarize}
\alias{summarize}
\alias{asNumericMatrix}
\alias{matrix2dataFrame}
\title{Summarize Scalars or Matrices by Cross-Classification}
\description{
\code{summarize} is a fast version of \code{summary.formula(formula,
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


\code{asNumericMatrix} converts a data frame into a numeric matrix,
saving attributes to reverse the process by \code{matrix2dataframe}.
It saves attributes that are commonly preserved across row
subsetting (i.e., it does not save \code{dim}, \code{dimnames}, or
\code{names} attributes).

\code{matrix2dataFrame} converts a numeric matrix back into a data
frame if it was created by \code{asNumericMatrix}.
}
\usage{
summarize(X, by, FUN, \dots, 
          stat.name=deparse(substitute(X)),
          type=c('variables','matrix'), subset=TRUE,
          keepcolnames=FALSE)

asNumericMatrix(x)

matrix2dataFrame(x, at=attr(x, 'origAttributes'), restoreAll=TRUE)
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
\item{keepcolnames}{by default when \code{type="matrix"}, the first
	column of the computed matrix is the name of the first argument to
	\code{summarize}.  Set \code{keepcolnames=TRUE} to retain the name of
	the first column created by \code{FUN}}
\item{x}{
  a data frame (for \code{asNumericMatrix}) or a numeric matrix (for
  \code{matrix2dataFrame}).
}
\item{at}{List containing attributes of original data frame that survive
  subsetting. Defaults to attribute \code{"origAttributes"} of the
  object \code{x}, created by the call to \code{asNumericMatrix}}
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
variable is a matrix.

\code{asNumericMatrix} returns a numeric matrix and stores an object
\code{origAttributes} as an attribute of the returned object, with original
attributes of component variables, the \code{storage.mode}. 

\code{matrix2dataFrame} returns a data frame.

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
library(lattice)
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
xn <- asNumericMatrix(x)
m <- mApply(xn, race, h)
# Here assume h is a function that returns a matrix similar to x
matrix2dataFrame(m)


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
d <- list(x=1:10, a=factor(rep(c('a','b'), 5)),
          b=structure(letters[1:10], label='label for b'),
          d=c(rep(TRUE,9), FALSE), f=pi*(1 : 10))
x <- asNumericMatrix(d)
attr(x, 'origAttributes')
matrix2dataFrame(x)

detach('dfr')

# Run summarize on a matrix to get column means
x <- c(1:19,NA)
y <- 101:120
z <- cbind(x, y)
g <- c(rep(1, 10), rep(2, 10))
summarize(z, g, colMeans, na.rm=TRUE, stat.name='x')
# Also works on an all numeric data frame
summarize(as.data.frame(z), g, colMeans, na.rm=TRUE, stat.name='x')
}
\keyword{category}
\keyword{manip}
\keyword{multivariate}
\concept{grouping}
\concept{stratification}
\concept{aggregation}
\concept{cross-classification}

