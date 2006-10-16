\name{csv.get}
\alias{csv.get}
\title{Read Comma-Separated Text Data Files}
\description{
  Read comma-separated text data files, allowing optional translation
  to lower case for variable names after making them valid S names.
}
\usage{
csv.get(file, lowernames=FALSE, datevars=NULL, dateformat='\%F',
        fixdates=c('none','year'), comment.char="", autodates=TRUE,
        allow=NULL, charfactor=FALSE, \dots)
}
\arguments{
  \item{file}{the file name for import.}
  \item{lowernames}{set this to \code{TRUE} to change variable names to
    lower case.}
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
    exceptional format.}
  \item{comment.char}{a character vector of length one containing a
    single character or an empty string.  Use '""' to turn off the
    interpretation of comments altogether.}
  \item{autodates}{Set to true to allow function to guess at which
    variables are dates}
  \item{allow}{a vector of characters allowed by \R that should not be
    converted to periods in variable names.  By default, underscores in
    variable names are converted to periods as with \R before version
    1.9.}
  \item{charfactor}{set to \code{TRUE} to change character variables to
	factors if they have at least two characters in an observation but
	have fewer than n/2 unique values}
  \item{\dots}{arguments to pass to \code{read.csv}.}
}
\details{
  \code{csv.get} reads comma-separated text data files, allowing optional
  translation to lower case for variable names after making them valid S
  names.  Original possibly non-legal names are taken to be variable
  labels.  Character or factor variables containing dates can be converted
  to date variables.  \code{cleanup.import} is invoked to finish the
  job.
}
\value{a new data frame.}
\author{Frank Harrell, Vanderbilt University}
\seealso{
  \code{\link{sas.get}}, \code{\link{data.frame}},
  \code{\link{cleanup.import}}, \code{\link{read.csv}},
  \code{\link{strptime}}, \code{\link{POSIXct}}, \code{\link{Date}}
}
\examples{
\dontrun{
dat <- read.table('myfile.csv')
dat <- cleanup.import(dat)
}
}
\keyword{manip}
\keyword{IO}
\keyword{file}
