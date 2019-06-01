\name{mdb.get}
\alias{mdb.get}
\title{Read Tables in a Microsoft Access Database}
\description{
  Assuming the \code{mdbtools} package has been installed on your
  system and is in the system path, \code{mdb.get} imports
  one or more tables in a Microsoft Access database.  Date-time
  variables are converted to dates or \code{chron} package date-time
  variables.  The \code{csv.get} function is used to import
  automatically exported csv files.  If \code{tables} 
  is unspecified all tables in the database are retrieved.  If more than
  one table is imported, the result is a list of data frames.
}
\usage{
mdb.get(file, tables=NULL, lowernames=FALSE, allow=NULL,
        dateformat='\%m/\%d/\%y', mdbexportArgs='-b strip', ...)
}
\arguments{
  \item{file}{the file name containing the Access database}
  \item{tables}{character vector specifying the names of tables to
	import.  Default is to import all tables.  Specify
	\code{tables=TRUE} to return the list of available tables.}
  \item{lowernames}{set this to \code{TRUE} to change variable names to
    lower case}
  \item{allow}{a vector of characters allowed by \R that should not be
    converted to periods in variable names.  By default, underscores in
    variable names are converted to periods as with \R before version
    1.9.}
  \item{dateformat}{see \code{\link{cleanup.import}}.  Default is the
		usual Access format used in the U.S.}
	\item{mdbexportArgs}{command line arguments to issue to mdb-export.
		Set to \code{''} to omit \code{'-b strip'}.}
  \item{\dots}{arguments to pass to \code{csv.get}}
}
\details{
  Uses the \code{mdbtools} package executables \code{mdb-tables},
  \code{mdb-schema}, and \code{mdb-export} (with by default option
	\code{-b strip} to drop any binary output).  In Debian/Ubuntu Linux run
  \code{apt get install mdbtools}.
  \code{cleanup.import} is invoked by \code{csv.get} to transform
  variables and store them as efficiently as possible.
}
\value{a new data frame or a list of data frames}
\author{Frank Harrell, Vanderbilt University}
\seealso{
  \code{\link{data.frame}},
  \code{\link{cleanup.import}}, \code{\link{csv.get}},
  \code{\link{Date}}, \code{\link[chron]{chron}}
}
\examples{
\dontrun{
# Read all tables in the Microsoft Access database Nwind.mdb
d <- mdb.get('Nwind.mdb')
contents(d)
for(z in d) print(contents(z))
# Just print the names of tables in the database
mdb.get('Nwind.mdb', tables=TRUE)
# Import one table
Orders <- mdb.get('Nwind.mdb', tables='Orders')
}
}
\keyword{manip}
\keyword{IO}
\keyword{file}
