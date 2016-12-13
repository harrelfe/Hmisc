\name{spss.get}
\alias{spss.get}
\title{Enhanced Importing of SPSS Files}
\description{
\code{spss.get} invokes the \code{read.spss} function in the
\pkg{foreign} package to read an SPSS file, with a default output
format of \code{"data.frame"}.  The \code{label} function is used to
attach labels to individual variables instead of to the data frame as
done by \code{read.spss}.  By default, integer-valued variables are
converted to a storage mode of integer unless
\code{force.single=FALSE}.  Date variables are converted to \R \code{Date}
variables.  By default, underscores in names are converted to periods.
}
\usage{
spss.get(file, lowernames=FALSE, datevars = NULL,
         use.value.labels = TRUE, to.data.frame = TRUE,
         max.value.labels = Inf, force.single=TRUE,
         allow=NULL, charfactor=FALSE, reencode = NA)
}
\arguments{
  \item{file}{input SPSS save file.  May be a file on the WWW, indicated
  by \code{file} starting with \code{'http://'} or \code{'https://'}.}
  \item{lowernames}{set to \code{TRUE} to convert variable names to
	lower case}
  \item{datevars}{vector of variable names containing dates to be
	converted to \R internal format}
  \item{use.value.labels}{see \code{\link[foreign]{read.spss}}}
  \item{to.data.frame}{see \code{\link[foreign]{read.spss}}; default is
	\code{TRUE} for \code{spss.get}}
  \item{max.value.labels}{see \code{\link[foreign]{read.spss}}}
  \item{force.single}{set to \code{FALSE} to prevent integer-valued
	variables from being converted from storage mode \code{double} to
	\code{integer}}
  \item{allow}{a vector of characters allowed by \R that should not be
	converted to periods in variable names.  By default, underscores in
	variable names are converted to periods as with \R before version 1.9.}
  \item{charfactor}{set to \code{TRUE} to change character variables to
	factors if they have fewer than n/2 unique values.  Blanks and null
	strings are converted to \code{NA}s.}
  \item{reencode}{see \code{\link[foreign]{read.spss}}}
}
\value{
  a data frame or list
}
\author{Frank Harrell}
\seealso{\code{\link[foreign]{read.spss}},\code{\link{cleanup.import}},\code{\link{sas.get}}}

\examples{
\dontrun{
w <- spss.get('/tmp/my.sav', datevars=c('birthdate','deathdate'))
  }
}
\keyword{interface}
\keyword{manip}
\keyword{file}
\concept{SPSS data file}
