\name{sasxport.get}
\alias{sasxport.get}
\alias{sasdsLabels}
\title{Enhanced Importing of SAS Transport Files using read.xport}
\description{
Uses the \code{read.xport} and \code{lookup.xport} functions in the
\code{foreign} library to import SAS datasets.  SAS date, time, and
date/time variables are converted to the appropriate POSIX objects in R,
variable names are converted to lower case, SAS labels are associated
with variables, and (by default) integer-valued variables are converted
from storage mode \code{double} to \code{integer}.  If the user ran
\code{PROC FORMAT CNTLOUT=} in SAS and included the resulting dataset in
the SAS version 5 transport file, variables having customized formats
that do not include any ranges (i.e., variables having standard
\code{PROC FORMAT; VALUE} label formats) will have their format labels looked
up, and these variables are converted to S \code{factor}s.

\code{SASdsLabels} reads a file containing \code{PROC CONTENTS}
printed output to parse dataset labels, assuming that \code{PROC
CONTENTS} was run on an entire library.
}
\usage{
sasxport.get(file, force.single = TRUE,
             method=c('read.xport','dataload'), formats=NULL)

sasdsLabels(file)
}
\arguments{
  \item{file}{name of a file containing the SAS transport file.
	\code{file} may be a URL beginning with \code{http://}.  For
\code{sasdsLabels}, \code{file} is the name of a file containing a
\code{PROC CONTENTS} output listing.
}
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
}
\value{
  If there is more than one dataset in the transport file other than the
  \code{PROC FORMAT} file, the result is a list of data frames
  containing all the non-\code{PROC FORMAT} datasets.  Otherwise the
  result is the single data frame.  \code{sasdsLabels} returns a named
  vector of dataset labels, with names equal to the dataset names.
}
\details{See \code{\link{contents.list}} for a way to print the
directory of SAS datasets when more than one was imported.}
\author{Frank E Harrell Jr}
\seealso{\code{\link{read.xport}},\code{\link{label}},\code{\link{sas.get}},
  \code{\link{DateTimeClasses}},\code{\link{lookup.xport}},
  \code{\link{contents}},\code{\link{describe}}}
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
w <- sasxport.get('http://hesweb1.med.virginia.edu/biostat/s/data/sas/test2.xpt')

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
