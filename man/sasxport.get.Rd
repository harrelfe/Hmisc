\name{sasxport.get}
\alias{sasxport.get}
\alias{sasdsLabels}
\title{Enhanced Importing of SAS Transport Files using read.xport}
\description{
Uses the \code{read.xport} and \code{lookup.xport} functions in the
\code{foreign} library to import SAS datasets.  SAS date, time, and
date/time variables are converted respectively to \code{Date}, 
POSIX, or \code{POSIXct} objects in \R, 
variable names are converted to lower case, SAS labels are associated
with variables, and (by default) integer-valued variables are converted
from storage mode \code{double} to \code{integer}.  If the user ran
\code{PROC FORMAT CNTLOUT=} in SAS and included the resulting dataset in
the SAS version 5 transport file, variables having customized formats
that do not include any ranges (i.e., variables having standard
\code{PROC FORMAT; VALUE} label formats) will have their format labels looked
up, and these variables are converted to S \code{factor}s.

For those users having access to SAS, \code{method='csv'} is preferred
when importing several SAS datasets.
Run SAS macro \code{exportlib.sas} available from
\url{https://github.com/harrelfe/Hmisc/blob/master/src/sas/exportlib.sas}
to convert all SAS datasets in a SAS data library (from any engine
supported by your system) into \code{CSV} files.  If any customized
formats are used, it is assumed that the \code{PROC FORMAT CNTLOUT=}
dataset is in the data library as a regular SAS dataset, as above.

\code{SASdsLabels} reads a file containing \code{PROC CONTENTS}
printed output to parse dataset labels, assuming that \code{PROC
CONTENTS} was run on an entire library.
}
\usage{
sasxport.get(file, lowernames=TRUE, force.single = TRUE,
             method=c('read.xport','dataload','csv'), formats=NULL, allow=NULL,
             out=NULL, keep=NULL, drop=NULL, as.is=0.5, FUN=NULL)
sasdsLabels(file)
}
\arguments{
  \item{file}{name of a file containing the SAS transport file.
	\code{file} may be a URL beginning with \code{https://}.  For
\code{sasdsLabels}, \code{file} is the name of a file containing a
\code{PROC CONTENTS} output listing.  For \code{method='csv'},
\code{file} is the name of the directory containing all the \code{CSV}
files created by running the \code{exportlib} SAS macro.
}
  \item{lowernames}{set to \code{FALSE} to keep from converting SAS
		variable names to lower case}
  \item{force.single}{set to \code{FALSE} to keep integer-valued
	variables not exceeding \eqn{2^31-1} in value from being converted to
	\code{integer} storage mode}
  \item{method}{set to \code{"dataload"} if you have the \code{dataload}
	executable installed and want to use it instead of
	\code{read.xport}.  This seems to correct some errors in which
	rarely some factor variables are always missing when read by
	\code{read.xport} when in fact they have some non-missing values.}
  \item{formats}{a data frame or list (like that created by
	\code{read.xport}) containing \code{PROC FORMAT}
	output, if such output is not stored in the main transport file.}
  \item{allow}{a vector of characters allowed by \R that should not be
converted to periods in variable names.  By default, underscores in
variable names are converted to periods as with \R before version 1.9.}
  \item{out}{a character string specifying a directory in which to write
	separate \R \code{save} files (\code{.rda} files) for each regular
	dataset.  Each file and the data frame inside it is named with the
	SAS dataset name translated to lower case and with underscores
	changed to periods.  The default \code{NULL} value of \code{out}
	results in a data frame or a list of data frames being returned.
	When \code{out} is given, \code{sasxport.get} returns only metadata (see
	below), invisibly.
	\code{out} only works with \code{methods='csv'}.  \code{out} should
	not have a trailing slash.}
  \item{keep}{a vector of names of SAS datasets to process (original SAS
  upper case names).  Must include \code{PROC FORMAT} dataset if it
  exists, and if the kept datasets use any of its value label formats.}
  \item{drop}{a vector of names of SAS datasets to ignore (original SAS
	upper case names)}
  \item{as.is}{
	SAS character variables are converted to S factor
	objects if \code{as.is=FALSE} or if \code{as.is} is a number between
	0 and 1 inclusive and the number of unique values of the variable is
	less than the number of observations (\code{n}) times \code{as.is}.
	The default if \code{as.is} is .5, so character variables are
	converted to factors only if they have fewer than \code{n/2} unique
	values.  The primary purpose of this is to keep unique
	identification variables as character values in the data frame
	instead of using more space to store both the integer factor codes
	and the factor labels.
  }
  \item{FUN}{an optional function that will be run on each data frame
	created, when \code{method='csv'} and \code{out} are specified.  The
	result of all the \code{FUN} calls is made into a list corresponding
	to the SAS datasets that are read.  This list is the \code{FUN}
	attribute of the result returned by \code{sasxport.get}.
	}
}
\value{
  If there is more than one dataset in the transport file other than the
  \code{PROC FORMAT} file, the result is a list of data frames
  containing all the non-\code{PROC FORMAT} datasets.  Otherwise the
  result is the single data frame.  There is an exception if \code{out}
  is specified; that causes separate \R \code{save} files to be written
  and the returned value to be a list corresponding to the SAS datasets,
  with key \code{PROC CONTENTS} information in a data frame making up
  each part of the list.
  \code{sasdsLabels} returns a named
  vector of dataset labels, with names equal to the dataset names.
}
\details{See \code{\link{contents.list}} for a way to print the
directory of SAS datasets when more than one was imported.}
\author{Frank E Harrell Jr}
\seealso{\code{\link[foreign]{read.xport}},\code{\link{label}},\code{\link{sas.get}},
  \code{\link{Dates}},\code{\link{DateTimeClasses}},
  \code{\link[foreign]{lookup.xport}},\code{\link{contents}},\code{\link{describe}}}
\examples{
\dontrun{
# SAS code to generate test dataset:
# libname y SASV5XPT "test2.xpt";
#
# PROC FORMAT; VALUE race 1=green 2=blue 3=purple; RUN;
# PROC FORMAT CNTLOUT=format;RUN;  * Name, e.g. 'format', unimportant;
# data test;
# LENGTH race 3 age 4;
# age=30; label age="Age at Beginning of Study";
# race=2;
# d1='3mar2002'd ;
# dt1='3mar2002 9:31:02'dt;
# t1='11:13:45't;
# output;
#
# age=31;
# race=4;
# d1='3jun2002'd ;
# dt1='3jun2002 9:42:07'dt;
# t1='11:14:13't;
# output;
# format d1 mmddyy10. dt1 datetime. t1 time. race race.;
# run;
# data z; LENGTH x3 3 x4 4 x5 5 x6 6 x7 7 x8 8;
#    DO i=1 TO 100;
#        x3=ranuni(3);
#        x4=ranuni(5);
#        x5=ranuni(7);
#        x6=ranuni(9);
#        x7=ranuni(11);
#        x8=ranuni(13);
#        output;
#        END;
#    DROP i;
#    RUN;
# PROC MEANS; RUN;
# PROC COPY IN=work OUT=y;SELECT test format z;RUN; *Creates test2.xpt;
w <- sasxport.get('test2.xpt')
# To use an existing copy of test2.xpt available on the web:
w <- sasxport.get('https://github.com/harrelfe/Hmisc/raw/master/inst/tests/test2.xpt')

describe(w$test)   # see labels, format names for dataset test
# Note: if only one dataset (other than format) had been exported,
# just do describe(w) as sasxport.get would not create a list for that
lapply(w, describe)# see descriptive stats for both datasets
contents(w$test)   # another way to see variable attributes
lapply(w, contents)# show contents of both datasets
options(digits=7)  # compare the following matrix with PROC MEANS output
t(sapply(w$z, function(x)
 c(Mean=mean(x),SD=sqrt(var(x)),Min=min(x),Max=max(x))))
}
}
\keyword{interface}
\keyword{manip}
