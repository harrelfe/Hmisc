\name{na.detail.response}
\alias{na.detail.response}
\title{
Detailed Response Variable Information
}
\description{
This function is called by certain \code{na.action} functions if
\code{options(na.detail.response=TRUE)} is set.  By default, this function
returns a matrix of counts of non-NAs and the mean of the response variable
computed separately by whether or not each predictor is NA.  The default
action uses the last column of a \code{Surv} object, in effect computing the
proportion of events.  Other summary functions may be specified by
using \code{options(na.fun.response="name of function")}.
}
\usage{
na.detail.response(mf)
}
\arguments{
\item{mf}{
a model frame
}}
\value{
a matrix, with rows representing the different statistics that are
computed for the response, and columns representing the different
subsets for each predictor (NA and non-NA value subsets).
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
\code{\link{na.omit}}, \code{\link{na.delete}}, \code{\link{model.frame.default}}, 
\code{\link{naresid}}, \code{\link{naprint}}, \code{\link{describe}}
}
\examples{
# sex
# [1] m f f m f f m m m m m m m m f f f m f m
# age
# [1] NA 41 23 30 44 22 NA 32 37 34 38 36 36 50 40 43 34 22 42 30
# y
# [1] 0 1 0 0 1 0 1 0 0 1 1 1 0 0 1 1 0 1 0 0
# options(na.detail.response=TRUE, na.action="na.delete", digits=3)
# lrm(y ~ age*sex)
#
# Logistic Regression Model
# 
# lrm(formula = y ~ age * sex)
#
#
# Frequencies of Responses
#   0 1 
#  10 8
#
# Frequencies of Missing Values Due to Each Variable
#  y age sex 
#  0   2   0
#
#
# Statistics on Response by Missing/Non-Missing Status of Predictors
#
#     age=NA age!=NA sex!=NA Any NA  No NA 
#   N    2.0  18.000   20.00    2.0 18.000
# Mean    0.5   0.444    0.45    0.5  0.444
#
# \dots\dots
# options(na.action="na.keep")
# describe(y ~ age*sex)
# Statistics on Response by Missing/Non-Missing Status of Predictors
#
#      age=NA age!=NA sex!=NA Any NA  No NA 
#    N    2.0  18.000   20.00    2.0 18.000
# Mean    0.5   0.444    0.45    0.5  0.444
#
# \dots
# options(na.fun.response="table")  #built-in function table()
# describe(y ~ age*sex)
#
# Statistics on Response by Missing/Non-Missing Status of Predictors
#
#   age=NA age!=NA sex!=NA Any NA No NA 
# 0      1      10      11      1    10
# 1      1       8       9      1     8
#
# \dots
}
\keyword{models}
\keyword{regression}
% Converted by Sd2Rd version 1.21.
