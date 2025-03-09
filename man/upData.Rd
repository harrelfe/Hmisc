\name{upData}
\alias{cleanup.import}
\alias{upData}
\alias{dataframeReduce}
\title{
  Update a Data Frame or Cleanup a Data Frame after Importing
}
\description{
  \code{cleanup.import} will correct errors and shrink
  the size of data frames.  By default, double precision numeric
  variables are changed to integer when they contain no fractional components. 
  Infinite values or values greater than 1e20 in absolute value are set
  to NA.  This solves problems of importing Excel spreadsheets that
  contain occasional character values for numeric columns, as S
  converts these to \code{Inf} without warning.  There is also an option to
  convert variable names to lower case and to add labels to variables.
  The latter can be made easier by importing a CNTLOUT dataset created
  by SAS PROC FORMAT and using the \code{sasdict} option as shown in the
  example below.  \code{cleanup.import} can also transform character or
  factor variables to dates.

  \code{upData} is a function facilitating the updating of a data frame
  without attaching it in search position one.  New variables can be
  added, old variables can be modified, variables can be removed or renamed, and
  \code{"labels"} and \code{"units"} attributes can be provided.
  Observations can be subsetted.  Various checks
  are made for errors and inconsistencies, with warnings issued to help
  the user.  Levels of factor variables can be replaced, especially
  using the \code{list} notation of the standard \code{merge.levels}
  function.  Unless \code{force.single} is set to \code{FALSE}, 
  \code{upData} also converts double precision vectors to integer if no
  fractional values are present in 
  a vector.  \code{upData} is also used to process R workspace objects
  created by StatTransfer, which puts variable and value labels as attributes on
  the data frame rather than on each variable. If such attributes are
  present, they are used to define all the labels and value labels
  (through conversion to factor variables) before any label changes
  take place, and \code{force.single} is set to a default of
  \code{FALSE}, as StatTransfer already does conversion to integer.

	Variables having labels but not classed \code{"labelled"} (e.g., data
	imported using the \code{haven} package) have that class added to them
	by \code{upData}.

  The \code{dataframeReduce} function removes variables from a data frame
  that are problematic for certain analyses.  Variables can be removed
  because the fraction of missing values exceeds a threshold, because they
  are character or categorical variables having too many levels, or
  because they are binary and have too small a prevalence in one of the
  two values.  Categorical variables can also have their levels combined
  when a level is of low prevalence.  A data frame listing actions take
  is return as attribute \code{"info"} to the main returned data frame.
}
\usage{
cleanup.import(obj, labels, lowernames=FALSE, 
               force.single=TRUE, force.numeric=TRUE, rmnames=TRUE,
               big=1e20, sasdict, print, datevars=NULL, datetimevars=NULL,
               dateformat='\%F',
               fixdates=c('none','year'),
               autodate=FALSE, autonum=FALSE, fracnn=0.3,
               considerNA=NULL, charfactor=FALSE)

upData(object, \dots, 
       subset, rename, drop, keep, labels, units, levels, force.single=TRUE,
       lowernames=FALSE, caplabels=FALSE, classlab=FALSE, moveUnits=FALSE,
       charfactor=FALSE, print=TRUE, html=FALSE)

dataframeReduce(data, fracmiss=1, maxlevels=NULL,  minprev=0, print=TRUE)
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
    For \code{upData}, \code{labels} is a named list or named vector
		with variables in no specific order.
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
  \item{print}{
    set to \code{TRUE} or \code{FALSE} to force or prevent printing of the current
    variable number being processed.  By default, such messages are printed if the
    product of the number of variables and number of observations in \code{obj}
    exceeds 500,000.  For \code{dataframeReduce} set \code{print} to
    \code{FALSE} to suppress printing information about dropped or
	modified variables.  Similar for \code{upData}.}
  \item{datevars}{character vector of names (after \code{lowernames} is
    applied) of variables to consider as a factor or character vector
    containing dates in a format matching \code{dateformat}.  The
    default is \code{"\%F"} which uses the yyyy-mm-dd format.}
  \item{datetimevars}{character vector of names (after \code{lowernames}
    is applied) of variables to consider to be date-time variables, with
    date formats as described under \code{datevars} followed by a space
    followed by time in hh:mm:ss format.  \code{chron} is used to store
    date-time variables.  If all times in the variable
    are 00:00:00 the variable will be converted to an ordinary date variable.}
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
	\item{autodate}{set to \code{TRUE} to have \code{cleanup.import}
		determine and automatically handle factor or character 
		vectors that mainly contain dates of the form YYYY-mm-dd,
		mm/dd/YYYY, YYYY, or mm/YYYY, where the later two are imputed to,
		respectively, July 3 and the 15th of the month.  Takes effect when
		the fraction of non-dates (of non-missing values) is less than
		\code{fracnn} to allow for some free text such as \code{"unknown"}.
		Attributes 
		\code{special.miss} and \code{imputed} are created for the vector so
		that \code{describe()} will inform the user.  Illegal values are
		converted to \code{NA}s and stored in the \code{special.miss} attribute.}
	\item{autonum}{set to \code{TRUE} to have \code{cleanup.import}
		examine (after \code{autodate}) character and factor variables to
		see if they are legal numerics exact for at most a fraction of
		\code{fracnn} of non-missing non-numeric values.  Qualifying variables are
		converted to numeric, and illegal values set to \code{NA} and stored in
		the \code{special.miss} attribute to enhance \code{describe} output.}
	\item{fracnn}{see \code{autodate} and \code{autonum}}
	\item{considerNA}{for \code{autodate} and \code{autonum}, considers
		character values in the vector \code{considerNA} to be the same as
		\code{NA}.  Leading and trailing white space and upper/lower case
		are ignored.} 
  \item{charfactor}{set to \code{TRUE} to change character variables to
	factors if they have fewer than n/2 unique values.  Null strings and
	blanks are converted to \code{NA}s.}
  \item{\dots}{
    for \code{upData}, one or more expressions of the form
    \code{variable=expression}, to derive new variables or change old ones.
  }
	\item{subset}{an expression that evaluates to a logical vector
		specifying which rows of \code{object} should be retained.  The
		expressions should use the original variable names, i.e., before any
	variables are renamed but after \code{lowernames} takes effect.}
  \item{rename}{
    list or named vector specifying old and new names for variables.  Variables are
    renamed before any other operations are done.  For example, to rename
    variables \code{age} and \code{sex} to respectively \code{Age} and
    \code{gender}, specify \code{rename=list(age="Age", sex="gender")} or
    \code{rename=c(age=\dots)}. 
  }
  \item{drop}{a vector of variable names to remove from the data frame}
	\item{keep}{a vector of variable names to keep, with all other
		variables dropped}
  \item{units}{
    a named vector or list defining \code{"units"} attributes of
		variables, in no specific order
  }
  \item{levels}{
    a named list defining \code{"levels"} attributes for factor variables, in
    no specific order.  The values in this list may be character vectors
    redefining \code{levels} (in order) or another list (see
    \code{merge.levels} if using S-Plus).
  }
  \item{caplabels}{
	set to \code{TRUE} to capitalize the first letter of each word in
	each variable label
	}
  \item{classlab}{
    set to \code{TRUE} (the old default behavior) to automatically have \code{upData} make variables having
    a \code{"label"} attribute have \code{class} of \code{"labelled"}.  Note that when the \code{labels}
    argument to \code{upData} is given, these create \code{labelled}-class variables as always.
  }
  \item{moveUnits}{
    set to \code{TRUE} to look for units of measurements in variable
    labels and move them to a \code{"units"} attribute.  If an expression
    in a label is enclosed in parentheses or brackets it is assumed to be
    units if \code{moveUnits=TRUE}.}
  \item{html}{set to \code{TRUE} to print conversion information as html
		vertabim at 0.6 size.  The user will need to put
		\code{results='asis'} in a \code{knitr} chunk header to properly
		render this output.}
  \item{fracmiss}{the maximum permissable proportion of \code{NA}s for a
    variable to be kept.  Default is to keep all variables no matter how
    many \code{NA}s are present.}
  \item{maxlevels}{the maximum number of levels of a character or
    categorical or factor variable before the variable is dropped}
  \item{minprev}{the minimum proportion of non-missing observations in a
    category for a binary variable to be retained, and the minimum
    relative frequency of a category before it will be combined with other
    small categories}
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
dat3 <- upData(dat, X=X^2, subset = x < (3/7)^2 - 5, rename=c(x='X'))

# Remove hard to analyze variables from a redundancy analysis of all
# variables in the data frame
d <- dataframeReduce(dat, fracmiss=.1, minprev=.05, maxlevels=5)
# Could run redun(~., data=d) at this point or include dataframeReduce
# arguments in the call to redun

# If you import a SAS dataset created by PROC CONTENTS CNTLOUT=x.datadict,
# the LABELs from this dataset can be added to the data.  Let's also
# convert names to lower case for the main data file
\dontrun{
mydata2 <- cleanup.import(mydata2, lowernames=TRUE, sasdict=datadict)
}
}
\keyword{data}
\keyword{manip}
