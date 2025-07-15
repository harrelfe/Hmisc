\name{sas.get}
\alias{sas.get}
\alias{is.special.miss}
\alias{[.special.miss}
\alias{print.special.miss}
\alias{format.special.miss}
\alias{sas.codes}
\alias{code.levels}
\alias{timePOSIXt}
\title{Convert a SAS Dataset to an S Data Frame}
\description{
  Converts a \acronym{SAS} dataset into an S data frame.  
  You may choose to extract only a subset of variables 
  or a subset of observations in the \acronym{SAS} dataset.
  The function will automatically convert \preformatted{PROC FORMAT}-coded
  variables to factor objects.  The original \acronym{SAS} codes are stored in an
  attribute called \code{sas.codes} and these may be added back to the
  \code{levels} of a \code{factor} variable using the \code{code.levels}
  function. 
  Information about special missing values may be captured in an attribute
  of each variable having special missing values.  This attribute is
  called \code{special.miss}, and such variables are given class \code{special.miss}.
  There are \code{print}, \code{[]}, \code{format}, and \code{is.special.miss}
  methods for such variables.
  date, time, and date-time variables use respectively
  \code{\link{Dates}}, \code{\link{DateTimeClasses}}, and
  \code{\link[chron]{chron}} variables.
  If using S-Plus 5 or 6 or later, the \code{timeDate} function is used instead.
  If a date variable represents a partial date (0.5 added if
  month missing, 0.25 added if day missing, 0.75 if both), an attribute
  \code{partial.date} is added to the variable, and the variable also becomes
  a class \code{imputed} variable.
  The \code{describe} function uses information about partial dates and
  special missing values.
  There is an option to automatically \command{PKUNZIP} compressed
  \acronym{SAS} datasets.

  \code{sas.get} works by composing and running a \acronym{SAS} job that
  creates various \acronym{ASCII} files that are read and analyzed
  by \code{sas.get}.  You can also run the \acronym{SAS} \code{sas_get} macro,
  which writes the \acronym{ASCII} files for downloading, in a separate
  step or on another computer, and then tell \code{sas.get} (through the
  \code{sasout} argument) to access these files instead of running \acronym{SAS}.
}
\usage{
sas.get(libraryName, member, variables=character(0), ifs=character(0),
     format.library=libraryName, id,
     dates.=c("sas","yymmdd","yearfrac","yearfrac2"),
     keep.log=TRUE, log.file="_temp_.log", macro=sas.get.macro,
     data.frame.out=existsFunction("data.frame"), clean.up=FALSE, quiet=FALSE,
     temp=tempfile("SaS"), formats=TRUE, 
     recode=formats, special.miss=FALSE, sasprog="sas",
     as.is=.5, check.unique.id=TRUE, force.single=FALSE, pos,
     uncompress=FALSE, defaultencoding="latin1", var.case="lower")

is.special.miss(x, code)

\method{[}{special.miss}(x, \dots, drop=FALSE)

\method{print}{special.miss}(x, \dots)

\method{format}{special.miss}(x, \dots)

sas.codes(object)

code.levels(object)
}
\arguments{
  \item{libraryName}{
    character string naming the directory in which the dataset is kept.
    The default is \code{libraryName = "."}, indicating that the current
    directory is to be used.
  }
  \item{member}{
    character string giving the second part of the two part \acronym{SAS} dataset name.  
    (The first part is irrelevant here - it is mapped to the directory name.)
  }
  \item{x}{
    a variable that may have been created by \code{sas.get} with \code{special.miss=TRUE}
    or with \code{recode} in effect.
  }
  \item{variables}{
    vector of character strings naming the variables in the \acronym{SAS} dataset.  
    The resulting data frame will contain only those variables from the
    \acronym{SAS} dataset.  
    To get all of the variables (the default), an empty string may be given.
    It is a fatal error if any one of the variables is not
    in the \acronym{SAS} dataset.  If you have retrieved a subset of the variables
    in the \acronym{SAS} dataset and which to retrieve the same list of variables
    from another dataset, you can program the value of \code{variables} - see
    one of the last examples.
  }
  \item{ifs}{
    a vector of character strings, each containing one \acronym{SAS} \dQuote{subsetting if}
    statement.  
    These will be used to extract a subset of the observations in the \acronym{SAS} dataset.
  }
  \item{format.library}{
    The directory containing the file \file{formats.sc2}, which contains
    the definitions of the user defined formats used in this dataset.
    By default, we look for the formats in the same directory as the data.
    The user defined formats must be available (so \acronym{SAS} can read the data).
  }
  \item{formats}{
    Set \code{formats} to \code{FALSE} to keep \code{sas.get} from telling the \acronym{SAS} macro to 
    retrieve value label formats from \code{format.library}.  When you do not
    specify \code{formats} or \code{recode}, \code{sas.get} will set \code{format} to \code{TRUE} if a
    \acronym{SAS} format catalog (\file{.sct} or \file{.sc2}) file exists in \code{format.library}.
    \code{sas.get} stores \acronym{SAS} \preformatted{PROC FORMAT VALUE} definitions
    as the \code{formats} attribute of the returned
    object (see below). A format is used if it is referred to by one or more 
    variables
    in the dataset, if it contains no ranges of values (i.e., it identifies
    value labels for single values), and if it is a character format
    or a numeric format that is not used just to label missing values.
    To fetch the values and labels for variable \code{x} in the dataset \code{d} you
    could type:
    \cr
    \code{f <- attr(d\$x, "format")}
    \cr
    \code{formats <- attr(d, "formats")}
    \cr
    \code{formats\$f\$values; formats\$f\$labels}
  }
  \item{recode}{
    This parameter defaults to \code{TRUE} if \code{formats} is \code{TRUE}.  If it is
    \code{TRUE}, variables that have an appropriate format (see above) are
    recoded as \code{factor} objects, which map the values
    to the value labels for the format.  Alternatively, set \code{recode} to
    1 to use labels of the form value:label, e.g. 1:good 2:better 3:best.
    Set \code{recode} to 2 to use labels such as good(1) better(2) best(3).
    Since \code{sas.codes} and \code{code.levels} add flexibility, the usual choice
    for \code{recode} is \code{TRUE}.
  }
  \item{drop}{
    logical. If \code{TRUE} the result is coerced to the
    lowest possible dimension.
  }
  \item{special.miss}{
    For numeric variables, any missing values are stored as NA in S.
    You can recover special missing values by setting \code{special.miss} to
    \code{TRUE}.  This will cause the \code{special.miss} attribute and the
    \code{special.miss} class to be added
    to each variable that has at least one special missing value.  
    Suppose that variable  \code{y} was .E in observation 3 and .G
    in observation 544.  The \code{special.miss} attribute for \code{y} then has the
    value
    \cr
    \code{list(codes=c("E","G"),obs=c(3,544))}
    \cr
    To fetch this information for variable \code{y} you would say for example
    \cr
    \code{s <- attr(y, "special.miss")}
    \cr
    \code{s\$codes; s\$obs}
    \cr
    or use \code{is.special.miss(x)} or the \code{print.special.miss} method, which
    will replace \code{NA} values for the variable with \samp{E} or \samp{G} if they
    correspond to special missing values.
    The describe
    function uses this information in printing a data summary.  
  }
  \item{id}{
    The name of the variable to be used as the row names of the S dataset.
    The id variable becomes the \code{row.names} attribute of a data frame, but
    the id variable is still retained as a variable in the data frame.
    You can also specify a vector of variable names as the \code{id}
    parameter.  After fetching the data from \acronym{SAS}, all these variables will be
    converted to character format and concatenated (with a space as a separator)
    to form a (hopefully) unique identification variable.
  }
  \item{dates.}{
    specifies the format for storing \acronym{SAS} dates in the
    resulting data frame.
  }
  \item{as.is}{
    \acronym{SAS} character variables are converted to S factor
    objects if \code{as.is=FALSE} or if \code{as.is} is a number between 0 and 1 inclusive and
    the number of unique values of the variable is less than
    the number of observations (\code{n}) times \code{as.is}.  The default if \code{as.is} is 0.5,
    so character variables are converted to factors only if they have fewer
    than \code{n/2} unique values.  The primary purpose of this is to keep unique
    identification variables as character values in the data frame instead
    of using more space to store both the integer factor codes and the
    factor labels.
  }
  \item{check.unique.id}{
    If \code{id} is specified, the row names are checked for
    uniqueness if \code{check.unique.id = TRUE}.  If any are duplicated, a warning
    is printed.  Note that if a data frame is being created with duplicate
    row names, statements such as \code{my.data.frame["B23",]} will retrieve
    only the first row with a row name of \samp{B23}.
  }
  \item{force.single}{
    By default, \acronym{SAS} numeric variables having \eqn{LENGTH > 4} are stored as
    S double precision numerics, which allow for the same precision as
    a \acronym{SAS} \preformatted{LENGTH} 8 variable.  Set \code{force.single = TRUE} to store every
    numeric variable in single precision (7 digits of precision).
    This option is useful when the creator of the \acronym{SAS} dataset has
    failed to use a \preformatted{LENGTH} statement.
    R does not have single precision,
    so no attempt is made to convert to single if running \R.
  }
  \item{keep.log}{
    logical: if \code{FALSE}, delete the \acronym{SAS} log file upon completion.
  }
  \item{log.file}{
    the name of the \acronym{SAS} log file.
  }
  \item{macro}{
    the name of an S object in the current search path that contains the text of
    the \acronym{SAS} macro called by S. The S object is a character vector that
    can be edited using, for example, \code{sas.get.macro <- editor(sas.get.macro)}.
  }
  \item{data.frame.out}{
    set to \code{FALSE} to make the result a list instead of a data
    frame
  }
  \item{clean.up}{
    logical flag: if \code{TRUE}, remove all temporary files when finished.  You
    may want to keep these while debugging the \acronym{SAS} macro.  Not needed for \R.
  }
  \item{quiet}{
    logical flag: if \code{FALSE}, print the contents of the
    \acronym{SAS} log file if there has been an error.
  }
  \item{temp}{
    the prefix to use for the temporary files.  Two characters
    will be added to this, the resulting name
    must fit on your file system.
  }
  \item{sasprog}{
    the name of the system command to invoke \acronym{SAS}
  }
  \item{uncompress}{
    set to \code{FALSE} by default.  Set it
    to \code{TRUE} to automatically invoke the DOS \command{PKUNZIP} command
    if \file{\var{member}.zip} exists,
    to uncompress the \acronym{SAS} dataset before
    proceeding.  This assumes you have the file permissions to allow
    uncompressing in place.  If the file is already uncompressed, this
    option is ignored.
  }
  \item{pos}{
    by default, a list or data frame which contains all the variables
    is returned.  If you specify \code{pos}, each individual variable
    is placed into a separate object (whose name is the name
    of the variable) using the \code{assign} function with the
    \code{pos} argument.  For example, you can put each variable
    in its own file in a directory, which in some cases may
    save memory over attaching a data frame.
  }
  \item{code}{
    a special missing value code (\samp{A} through \samp{Z} or
\samp{\_}) to check against. If \code{code} is omitted,
\code{is.special.miss} will return a \code{TRUE} for each 
    observation that has any special missing value.
  }
\item{defaultencoding}{
    encoding to assume if the SAS dataset does not specify one. Defaults to "latin1".
  }
\item{var.case}{
    specify the case that you want variable names to be in.  "lower" for lower case, 
    "upper" for upper case, and "preserve" to retain the case from SAS.
  }
  \item{object}{
    a variable in a data frame created by \code{sas.get}
  }
  \item{\dots}{ignored}
}
\value{
  A data frame resembling the \acronym{SAS} dataset.  If \code{id}
  was specified, that column of the data frame will be used
  as the row names of the data frame.  Each variable in the data frame
  or vector in the list will have the attributes \code{label} and \code{format}
  containing \acronym{SAS} labels and formats.  Underscores in formats are
  converted to periods.  Formats for character variables have \samp{\$} placed
  in front of their names.
  If \code{formats} is \code{TRUE} and there are any 
  appropriate format definitions in \code{format.library}, the returned
  object will have attribute \code{formats} containing lists named the
  same as the format names (with periods substituted for underscores and
  character formats prefixed by \samp{\$}).
  Each of these lists has a vector called \code{values} and one called
  \code{labels} with the \preformatted{PROC FORMAT; VALUE} \code{\dots} definitions.
}
\section{Side Effects}{
  if a \acronym{SAS} error occurs the \acronym{SAS} log file will be
  printed under the control of the \code{pager} function.
}
\details{
  If you specify \code{special.miss = TRUE} and there are no special missing
  values in the data \acronym{SAS} dataset, the \acronym{SAS} step will bomb.

  For variables having a \preformatted{PROC FORMAT VALUE}
  format with some of the levels undefined, \code{sas.get} will interpret those
  values as \code{NA} if you are using \code{recode}.


  If you leave the \code{sasprog} argument at its default value of
  \samp{sas}, be sure that the \acronym{SAS} executable is in the \file{PATH}
  specified in your \file{autoexec.bat} file.  Also make sure that
  you invoke S so that your current project directory is known
  to be the current working directory.  This is best done by creating
  a shortcut in Windows95, for which the command to execute will be
  something like \command{drive:\\spluswin\\cmd\\splus.exe HOME=.} and the
  program is flagged to start in \file{drive:\\myproject} for example.
  In this way, you will be able to examine the \acronym{SAS} log file easily
  since it will be placed in \file{drive:\\myproject} by default.

  \acronym{SAS} will create \samp{SASWORK} and \samp{SASUSER} directories in what it thinks
  are the current working directories.  To specify where \acronym{SAS} should
  put these instead, edit the \file{config.sas} file or specify a
  \code{sasprog} argument of the following form:
  \code{sasprog="\\sas\\sas.exe -saswork c:\\saswork -sasuser c:\\sasuser"}.

  When \code{sas.get} needs to run \acronym{SAS} it is run in iconized form.

  The \acronym{SAS} macro \file{sas_get} uses record lengths of up to 4096 in two
  places.  If you are exporting records that are very long (because of
  a large number of variables and/or long character variables), you
  may want to edit these \samp{LRECL}s to quadruple them, for example.
}
\note{
  If \code{sasout} is not given, you
  must be able to run \acronym{SAS} on your system.  


  If you are reading time or
  date-time variables, you will need to execute the command \code{library(chron)}
  to print those variables or the data frame.
}
\section{BACKGROUND}{
  The references cited below explain the structure of \acronym{SAS} datasets and how
  they are stored.
  See \emph{\acronym{SAS} Language} 
  for a discussion of the \preformatted{subsetting if} statement.
}
\author{
  Terry Therneau, Mayo Clinic
  \cr
  Frank Harrell, Vanderbilt University
  \cr
  Bill Dunlap, University of Washington and Insightful Corp.
  \cr
  Michael W. Kattan, Cleveland Clinic Foundation
  \cr
  Reinhold Koch (encoding)
}
\references{
  \acronym{SAS} Institute Inc. (1990).
  \emph{\acronym{SAS} Language: Reference, Version 6.}
  First Edition.
  \acronym{SAS} Institute Inc., Cary, North Carolina.


  \acronym{SAS} Institute Inc. (1988).
  \acronym{SAS} Technical Report P-176,
  \emph{Using the \acronym{SAS} System, Release 6.03, under UNIX Operating Systems and Derivatives.  }
  \acronym{SAS} Institute Inc., Cary, North Carolina.


  \acronym{SAS} Institute Inc. (1985).
  \emph{\acronym{SAS} Introductory Guide.}
  Third Edition.
  \acronym{SAS} Institute Inc., Cary, North Carolina.
}
\seealso{
  \code{\link{data.frame}}, \code{\link[Hmisc]{describe}},
  \code{\link[Hmisc]{label}}, \code{\link[Hmisc]{upData}}
}
\examples{
\dontrun{
mice <- sas.get("saslib", mem="mice", var=c("dose", "strain", "ld50"))
plot(mice$dose, mice$ld50)

nude.mice <- sas.get(lib=unix("echo $HOME/saslib"), mem="mice",
	ifs="if strain='nude'")

nude.mice.dl <- sas.get(lib=unix("echo $HOME/saslib"), mem="mice",
	var=c("dose", "ld50"), ifs="if strain='nude'")

# Get a dataset from current directory, recode PROC FORMAT; VALUE \dots 
# variables into factors with labels of the form "good(1)" "better(2)",
# get special missing values, recode missing codes .D and .R into new
# factor levels "Don't know" and "Refused to answer" for variable q1
d <- sas.get(mem="mydata", recode=2, special.miss=TRUE)
attach(d)
nl <- length(levels(q1))
lev <- c(levels(q1), "Don't know", "Refused")
q1.new <- as.integer(q1)
q1.new[is.special.miss(q1,"D")] <- nl+1
q1.new[is.special.miss(q1,"R")] <- nl+2
q1.new <- factor(q1.new, 1:(nl+2), lev)
# Note: would like to use factor() in place of as.integer ... but
# factor in this case adds "NA" as a category level

d <- sas.get(mem="mydata")
sas.codes(d$x)    # for PROC FORMATted variables returns original data codes
d$x <- code.levels(d$x)   # or attach(d); x <- code.levels(x)
# This makes levels such as "good" "better" "best" into e.g.
# "1:good" "2:better" "3:best", if the original SAS values were 1,2,3

# For the following example, suppose that SAS is run on a
# different machine from the one on which S is run.
# The sas_get macro is used to create files needed by
# sas.get.  To make a text file containing the sas_get macro
# run the following S command, for example:
#   cat(sas.get.macro, file='/sasmacro/sas_get.sas', sep='\n')

# Here is the SAS job.  This job assumes that you put
# sas_get.sas in an autocall macro library.


#  libname db '/my/sasdata/area';
#  \%sas_get(db.mydata, dict, data, formats, specmiss,
#           formats=1, specmiss=1)


# Substitute whatever file names you may want.
# Next the 4 files are moved to the S machine (using
# ASCII file transfer mode) and the following S
# program is run:


mydata <- sas.get(sasout=c('dict','data','formats','specmiss'),
                  id='idvar')


# If PKZIP is run after \%sas_get, e.g. "PKZIP port dict data formats"
# (assuming that specmiss was not used here), use


mydata <- sas.get(sasout='a:port', id='idvar')


# which will run PKUNZIP port to unzip a:port.zip, creating the
# dict, data, and formats files which are generated (and later
# deleted) by sas.get


# Retrieve the same variables from another dataset (or an update of
# the original dataset)
mydata2 <- sas.get('mydata2', var=names(mydata))
# This only works if none of the original SAS variable names contained _

# Code from Don MacQueen to generate SAS dataset to test import of
# date, time, date-time variables
# data ssd.test;
#     d1='3mar2002'd ;
#     dt1='3mar2002 9:31:02'dt;
#     t1='11:13:45't;
#     output;
#
#     d1='3jun2002'd ;
#     dt1='3jun2002 9:42:07'dt;
#     t1='11:14:13't;
#     output;
#     format d1 mmddyy10. dt1 datetime. t1 time.;
# run;
}
}
\keyword{interface}
\keyword{manip}






