\name{upData}
\alias{cleanup.import}
\alias{upData}
\alias{exportDataStripped}
\title{
Update a Data Frame or Cleanup a Data Frame after Importing
}
\description{
\code{cleanup.import} will correct errors and shrink
the size of data frames created by the S-Plus \code{File \dots Import}
dialog or by other methods such as \code{scan} and \code{read.table}.  By
default, double precision numeric variables are changed to single
precision (S-Plus only) or to integer when they contain no fractional
components. 
Infinite values or values greater than 1e20 in absolute value are set
to NA.  This solves problems of importing Excel spreadsheets that
contain occasional character values for numeric columns, as S-Plus
converts these to \code{Inf} without warning.  There is also an option to
convert variable names to lower case and to add labels to variables.
The latter can be made easier by importing a CNTLOUT dataset created
by SAS PROC FORMAT and using the \code{sasdict} option as shown in the
example below.  \code{cleanup.import} can also transform character or
factor variables to dates.

\code{upData} is a function facilitating the updating of a data frame
without attaching it in search position one.  New variables can be
added, old variables can be modified, variables can be removed or renamed, and
\code{"labels"} and \code{"units"} attributes can be provided.  Various checks
are made for errors and inconsistencies, with warnings issued to help
the user.  Levels of factor variables
can be replaced, especially using the \code{list} notation of the standard
\code{merge.levels} function.  Unless \code{force.single} is set to \code{FALSE},
\code{upData} also converts double precision vectors to single precision
(if not under R), or to integer if no fractional values are present in
a vector.

Both \code{cleanup.import} and \code{upData} will fix a problem with
data frames created under S-Plus before version 5 that are used in S-Plus 5 or
later.  The problem was caused by use of the \code{label} function
to set a variable's class to \code{"labelled"}.  These classes are
removed as the S version 4 language does not support multiple
inheritance.  Failure to run data frames through one of the two
functions when these conditions apply will result in simple numeric
variables being set to \code{factor} in some cases.  Extraneous \code{"AsIs"}
classes are also removed.

For S-Plus, a function \code{exportDataStripped} is provided that allows
exporting of data to other systems 
by removing attributes \code{label, imputed, format, units}, and
\code{comment}.  It calls \code{exportData} after stripping these
attributes.  Otherwise \code{exportData} will fail.
}
\usage{
cleanup.import(obj, labels, lowernames=FALSE, 
               force.single=TRUE, force.numeric=TRUE, rmnames=TRUE,
               big=1e20, sasdict, pr, datevars=NULL, dateformat='\%F',
               fixdates=c('none','year'), charfactor=FALSE)

upData(object, \dots, 
       rename, drop, labels, units, levels,
       force.single=TRUE, lowernames=FALSE, moveUnits=FALSE, charfactor=FALSE)

exportDataStripped(data, \dots)
}
\arguments{
\item{obj}{a data frame or list}
\item{object}{a data frame or list}
\item{data}{a data frame}
\item{force.single}{
By default, double precision variables are converted to single precision
(in S-Plus only) unless \code{force.single=FALSE}.
\code{force.single=TRUE} will also convert vectors having only integer
values to have a storage mode of integer, in R or S-Plus.
}
\item{force.numeric}{
Sometimes importing will cause a numeric variable to be
changed to a factor vector.  By default, \code{cleanup.import} will check
each factor variable to see if the levels contain only numeric values
and \code{""}.  In that case, the variable will be converted to numeric,
with \code{""} converted to NA.  Set \code{force.numeric=FALSE} to prevent
this behavior. 
}
\item{rmnames}{
set to `F' to not have `cleanup.import' remove `names' or `.Names'
attributes from variables
}
\item{labels}{
a character vector the same length as the number of variables in
\code{obj}.  These character values are taken to be variable labels in the
same order of variables in \code{obj}.
For \code{upData}, \code{labels} is a named list or named vector with variables
in no specific order.
}
\item{lowernames}{
set this to \code{TRUE} to change variable names to lower case.
\code{upData} does this before applying any other changes, so variable
names given inside arguments to \code{upData} need to be lower case if
\code{lowernames==TRUE}. 
}
\item{big}{
a value such that values larger than this in absolute value are set to
missing by \code{cleanup.import}
}
\item{sasdict}{
the name of a data frame containing a raw imported SAS PROC CONTENTS
CNTLOUT= dataset.  This is used to define variable names and to add
attributes to the new data frame specifying the original SAS dataset
name and label.
}
\item{pr}{
set to \code{TRUE} or \code{FALSE} to force or prevent printing of the current
variable number being processed.  By default, such messages are printed if the
product of the number of variables and number of observations in \code{obj}
exceeds 500,000.
}
\item{datevars}{character vector of names (after \code{lowernames} is
  applied) of variables to consider as a factor or character vector
  containing dates in a format matching \code{dateformat}.  The
default is \code{"\%F"} which uses the yyyy-mm-dd format.}
\item{dateformat}{for \code{cleanup.import} is the input format (see
  \code{\link{strptime}})}
\item{fixdates}{for any of the variables listed in \code{datevars}
that have a \code{dateformat} that \code{cleanup.import} understands,
specifying \code{fixdates} allows corrections of certain formatting
inconsistencies before the fields are attempted to be converted to
dates (the default is to assume that the \code{dateformat} is followed
for all observation for \code{datevars}).  Currently
\code{fixdates='year'} is implemented, which will cause 2-digit or
4-digit years to be shifted to the alternate number of digits when
\code{dateform} is the default \code{"\%F"} or is \code{"\%y-\%m-\%d"},
\code{"\%m/\%d/\%y"}, or \code{"\%m/\%d/\%Y"}.  Two-digits years are padded with \code{20}
on the left.  Set \code{dateformat} to the desired format, not the
exceptional format.
}
\item{charfactor}{set to \code{TRUE} to change character variables to
  factors if they have at least two characters in an observation but
  have fewer than n/2 unique values}
\item{\dots}{
for \code{upData}, one or more expressions of the form
\code{variable=expression}, to derive new variables or change old ones.
For \code{exportDataStripped}, optional arguments that are passed to
\code{exportData}.
}
\item{rename}{
list or named vector specifying old and new names for variables.  Variables are
renamed before any other operations are done.  For example, to rename
variables \code{age} and \code{sex} to respectively \code{Age} and
\code{gender}, specify \code{rename=list(age="Age", sex="gender")} or
\code{rename=c(age=\dots)}. 
}
\item{drop}{
a vector of variable names to remove from the data frame
}
\item{units}{
a named vector or list defining \code{"units"} attributes of variables, in no
specific order
}
\item{levels}{
a named list defining \code{"levels"} attributes for factor variables, in
no specific order.  The values in this list may be character vectors
redefining \code{levels} (in order) or another list (see
\code{merge.levels} if using S-Plus).
}
\item{moveUnits}{
  set to \code{TRUE} to look for units of measurements in variable
  labels and move them to a \code{"units"} attribute.  If an expression
  in a label is enclosed in parentheses or brackets it is assumed to be
  units if \code{moveUnits=TRUE}.
}
}
\value{a new data frame}
\author{
Frank Harrell, Vanderbilt University
}
\seealso{
  \code{\link{sas.get}}, \code{\link{data.frame}}, \code{\link{describe}},
  \code{\link{label}}, \code{\link{read.csv}}, \code{\link{strptime}},
  \code{\link{POSIXct}},\code{\link{Date}}
}
\examples{
\dontrun{
dat <- read.table('myfile.asc')
dat <- cleanup.import(dat)
}
dat <- data.frame(a=1:3, d=c('01/02/2004',' 1/3/04',''))
cleanup.import(dat, datevars='d', dateformat='\%m/\%d/\%y', fixdates='year')

dat <- data.frame(a=(1:3)/7, y=c('a','b1','b2'), z=1:3)
dat2 <- upData(dat, x=x^2, x=x-5, m=x/10, 
               rename=c(a='x'), drop='z',
               labels=c(x='X', y='test'),
               levels=list(y=list(a='a',b=c('b1','b2'))))
dat2
describe(dat2)
dat <- dat2    # copy to original name and delete dat2 if OK
rm(dat2)

# If you import a SAS dataset created by PROC CONTENTS CNTLOUT=x.datadict,
# the LABELs from this dataset can be added to the data.  Let's also
# convert names to lower case for the main data file
\dontrun{
mydata2 <- cleanup.import(mydata2, lowernames=TRUE, sasdict=datadict)
}
}
\keyword{data}
\keyword{manip}


