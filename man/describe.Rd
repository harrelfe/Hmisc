\name{describe}
\alias{describe}
\alias{describe.default}
\alias{describe.vector}
\alias{describe.matrix}
\alias{describe.formula}
\alias{describe.data.frame}
\alias{print.describe}
\alias{print.describe.single}
\alias{[.describe}
\alias{latex.describe}
\alias{latex.describe.single}
\title{
Concise Statistical Description of a Vector, Matrix, Data Frame, or Formula
}
\description{
\code{describe} is a generic method that invokes \code{describe.data.frame},
\code{describe.matrix}, \code{describe.vector}, or
\code{describe.formula}. \code{describe.vector} is the basic 
function for handling a single variable.
This function determines whether the variable is character, factor,
category, binary, discrete numeric, and continuous numeric, and prints
a concise statistical summary according to each. A numeric variable is
deemed discrete if it has <= 10 unique values. In this case,
quantiles are not printed. A frequency table is printed 
for any non-binary variable if it has no more than 20 unique
values.  For any variable with at least 20 unique values, the 5 lowest
and highest values are printed.  \code{describe} is especially useful for
describing data frames created by \code{sas.get}, as SAS labels, formats,
value labels, and frequencies of special missing values are printed.

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

A latex method
exists for converting the \code{describe} object to a LaTeX file.  For
numeric variables having at least 20 unique values, \code{describe} saves
in its returned object the frequencies of 100 evenly spaced bins
running from minimum observed value to the maximum.  \code{latex} inserts a
spike histogram displaying these frequency counts in the tabular
material using the LaTeX picture environment.  For example output see
\url{http://biostat.mc.vanderbilt.edu/twiki/pub/Main/Hmisc/counties.pdf}.

Sample weights may be specified to any of the functions, resulting
in weighted means, quantiles, and frequency tables.
}
\usage{
\method{describe}{vector}(x, descript, exclude.missing=TRUE, digits=4,
         weights, normwt, \dots)
\method{describe}{matrix}(x, descript, exclude.missing=TRUE, digits=4, \dots)
\method{describe}{data.frame}(x, descript, exclude.missing=TRUE,
    digits=4, \dots)
\method{describe}{formula}(x, descript, data, subset, na.action,
    digits=4, weights, \dots)
\method{print}{describe}(x, condense=TRUE, \dots)
\method{latex}{describe}(object, title=NULL, condense=TRUE, 
      file=paste('describe',first.word(expr=attr(object,'descript')),'tex',sep='.'),
      append=FALSE, size='small', tabular=TRUE, \dots)
\method{latex}{describe.single}(object, title=NULL, condense=TRUE, vname,
      file, append=FALSE, size='small', tabular=TRUE, \dots)
}
\arguments{
\item{x}{
  a data frame, matrix, vector, or formula.  For a data frame, the 
  \code{describe.data.frame}
  function is automatically invoked.  For a matrix, \code{describe.matrix} is
  called.  For a formula, describe.data.frame(model.frame(x))
  is invoked. The formula may or may not have a response variable.  For
  \code{print} or \code{latex}, \code{x} is an object created by
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
  number of significant digits to print
}
\item{weights}{
  a numeric vector of frequencies or sample weights.  Each observation
  will be treated as if it were sampled \code{weights} times.
}
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
\item{condense}{
  default isTRUE to condense the output with regard to the 5 lowest and
  highest values and the frequency table
}
\item{data}{
}
\item{subset}{
}
\item{na.action}{
  There are used if a formula is specified.  \code{na.action} defaults to
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
  information.  For \code{latex} methods, \dots is ignored.}
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
LaTeX text size (\code{"small"}, the default, or \code{"normalsize"}, \code{"tiny"},
\code{"scriptsize"}, etc.) for the \code{describe} output in LaTeX.
}
\item{tabular}{
  set to \code{FALSE} to use verbatim rather than tabular environment
  for the summary statistics output.  By default, tabular is used if the
  output is not too wide.}
\item{vname}{unused argument in \code{latex.describe.single}}
}
\value{
a list containing elements \code{descript}, \code{counts},
\code{values}.  The list  is of class \code{describe}.  If the input
object was a matrix or a data 
frame, the list is a list of lists, one list for each variable
analyzed. \code{latex} returns a standard \code{latex} object.  For numeric
variables having at least 20 unique values, an additional component
\code{intervalFreq}.  This component is a list with two elements, \code{range}
(containing two values) and \code{count}, a vector of 100 integer frequency
counts.
}
\details{
If \code{options(na.detail.response=TRUE)}
has been set and \code{na.action} is \code{"na.delete"} or
\code{"na.keep"}, summary  statistics on
the response variable are printed separately for missing and non-missing
values of each predictor.  The default summary function returns
the number of non-missing response values and the mean of the last
column of the response values, with a \code{names} attribute of \code{c("N","Mean")}.
When the response is a \code{Surv} object and the mean is used, this will
result in the crude proportion of events being used to summarize
the response.  The actual summary function can be designated through
\code{options(na.fun.response = "function name")}.
}
\author{
Frank Harrell
\cr
Vanderbilt University
\cr
\email{f.harrell@vanderbilt.edu}
}
\seealso{
\code{\link{sas.get}}, \code{\link{quantile}}, \code{\link{table}}, \code{\link{summary}},  \code{\link{model.frame.default}},
\code{\link{naprint}}, \code{\link{lapply}}, \code{\link{tapply}}, \code{\link[survival]{Surv}}, \code{\link{na.delete}}, \code{\link{na.keep}},
\code{\link{na.detail.response}}, \code{\link{latex}}
}
\examples{
set.seed(1)
describe(runif(200),dig=2)    #single variable, continuous
                              #get quantiles .05,.10,\dots

dfr <- data.frame(x=rnorm(400),y=sample(c('male','female'),400,TRUE))
describe(dfr)

\dontrun{
d <- sas.get(".","mydata",special.miss=TRUE,recode=TRUE)
describe(d)      #describe entire data frame
attach(d, 1)
describe(relig)  #Has special missing values .D .F .M .R .T
                 #attr(relig,"label") is "Religious preference"

#relig : Religious preference  Format:relig
#    n missing  D  F M R T unique 
# 4038     263 45 33 7 2 1      8
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

}
}
\keyword{interface}
\keyword{nonparametric}
\keyword{category}
\keyword{distribution}
\keyword{robust}
\keyword{models}

