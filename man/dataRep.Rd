\name{dataRep}
\alias{dataRep}
\alias{print.dataRep}
\alias{predict.dataRep}
\alias{print.predict.dataRep}
\alias{roundN}
\alias{[.roundN}
\title{
Representativeness of Observations in a Data Set
}
\description{
These functions are intended to be used to describe how well a given
set of new observations (e.g., new subjects) were represented in a
dataset used to develop a predictive model.
The \code{dataRep} function forms a data frame that contains all the unique
combinations of variable values that existed in a given set of
variable values.  Cross--classifications of values are created using
exact values of variables, so for continuous numeric variables it is
often necessary to round them to the nearest \code{v} and to possibly
curtail the values to some lower and upper limit before rounding.
Here \code{v} denotes a numeric constant specifying the matching tolerance
that will be used.  \code{dataRep} also stores marginal distribution
summaries for all the variables.  For numeric variables, all 101
percentiles are stored, and for all variables, the frequency
distributions are also stored (frequencies are computed after any
rounding and curtailment of numeric variables).  For the purposes of
rounding and curtailing, the \code{roundN} function is provided.  A \code{print}
method will summarize the calculations made by \code{dataRep}, and if
\code{long=TRUE} all unique combinations of values and their frequencies in
the original dataset are printed.

The \code{predict} method for \code{dataRep} takes a new data frame having
variables named the same as the original ones (but whose factor levels
are not necessarily in the same order) and examines the collapsed
cross-classifications created by \code{dataRep} to find how many
observations were similar to each of the new observations after any
rounding or curtailment of limits is done.  \code{predict} also does some
calculations to describe how the variable values of the new
observations "stack up" against the marginal distributions of the
original data.  For categorical variables, the percent of observations
having a given variable with the value of the new observation (after
rounding for variables that were through \code{roundN} in the formula given
to \code{dataRep}) is computed.  For numeric variables, the percentile of
the original distribution in which the current value falls will be
computed.  For this purpose, the data are not rounded because the 101
original percentiles were retained; linear interpolation is used to
estimate percentiles for values between two tabulated percentiles.
The lowest marginal frequency of matching values across all variables
is also computed.  For example, if an age, sex combination matches 10
subjects in the original dataset but the age value matches 100 ages
(after rounding) and the sex value matches the sex code of 300
observations, the lowest marginal frequency is 100, which is a "best
case" upper limit for multivariable matching.  I.e., matching on all
variables has to result on a lower frequency than this amount.
A \code{print} method for the output of \code{predict.dataRep} prints all
calculations done by \code{predict} by default.  Calculations can be
selectively suppressed.
}
\usage{
dataRep(formula, data, subset, na.action)

roundN(x, tol=1, clip=NULL)

\method{print}{dataRep}(x, long=FALSE, \dots)

\method{predict}{dataRep}(object, newdata, \dots)

\method{print}{predict.dataRep}(x, prdata=TRUE, prpct=TRUE, \dots)
}
\arguments{
\item{formula}{
a formula with no left-hand-side.  Continuous numeric variables in
need of rounding should appear in the formula as e.g. \code{roundN(x,5)} to
have a tolerance of e.g. +/- 2.5 in matching.  Factor or character
variables as well as numeric ones not passed through \code{roundN} are
matched on exactly.
}
\item{x}{
a numeric vector or an object created by \code{dataRep}
}
\item{object}{
the object created by \code{dataRep} or \code{predict.dataRep}
}
\item{data, subset, na.action}{
standard modeling arguments.  Default \code{na.action} is \code{na.delete},
i.e., observations in the original dataset having any variables
missing are deleted up front.
}
\item{tol}{
rounding constant (tolerance is actually \code{tol/2} as values are rounded
to the nearest \code{tol})
}
\item{clip}{
a 2-vector specifying a lower and upper limit to curtail values of \code{x}
before rounding
}
\item{long}{
set to \code{TRUE} to see all unique combinations and frequency count
}
\item{newdata}{
a data frame containing all the variables given to \code{dataRep} but not
necessarily in the same order or having factor levels in the same order
}
\item{prdata}{
set to \code{FALSE} to suppress printing \code{newdata} and the count of matching
observations (plus the worst-case marginal frequency). 
}
\item{prpct}{set to \code{FALSE} to not print percentiles and percents}
\item{\dots}{unused}
}
\value{
\code{dataRep} returns a list of class \code{"dataRep"} containing the collapsed
data frame and frequency counts along with marginal distribution
information.  \code{predict} returns an object of class \code{"predict.dataRep"}
containing information determined by matching observations in
\code{newdata} with the original (collapsed) data.
}
\section{Side Effects}{
\code{print.dataRep} prints.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University School of Medicine
\cr
\email{fh@fharrell.com}
}
\seealso{
\code{\link{round}}, \code{\link{table}}
}
\examples{
set.seed(13)
num.symptoms <- sample(1:4, 1000,TRUE)
sex <- factor(sample(c('female','male'), 1000,TRUE))
x    <- runif(1000)
x[1] <- NA
table(num.symptoms, sex, .25*round(x/.25))


d <- dataRep(~ num.symptoms + sex + roundN(x,.25))
print(d, long=TRUE)


predict(d, data.frame(num.symptoms=1:3, sex=c('male','male','female'),
                      x=c(.03,.5,1.5)))
}
\keyword{datasets}
\keyword{category}
\keyword{cluster}
\keyword{manip}
\keyword{models}
% Converted by Sd2Rd version 1.21.
