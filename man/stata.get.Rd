\name{stata.get}
\alias{stata.get}
\title{Enhanced Importing of STATA Files}
\description{
  Reads a file in Stata version 5-11 binary format format into a
  data frame.
}
\usage{
stata.get(file, lowernames = FALSE, convert.dates = TRUE,
          convert.factors = TRUE, missing.type = FALSE,
          convert.underscore = TRUE, warn.missing.labels = TRUE,
          force.single = TRUE, allow=NULL, charfactor=FALSE, \dots)
}
\arguments{
  \item{file}{input \acronym{SPSS} save file.  May be a file on the \acronym{WWW}, indicated
    by \code{file} starting with \samp{'https://'}.}
  \item{lowernames}{set to \code{TRUE} to convert variable names to
    lower case}
  \item{convert.dates}{see \code{\link[foreign]{read.dta}}}
  \item{convert.factors}{see \code{\link[foreign]{read.dta}}}
  \item{missing.type}{see \code{\link[foreign]{read.dta}}}
  \item{convert.underscore}{see \code{\link[foreign]{read.dta}}}
  \item{warn.missing.labels}{see \code{\link[foreign]{read.dta}}}
  \item{force.single}{set to \code{FALSE} to prevent integer-valued
    variables from being converted from storage mode \code{double} to
    \code{integer}}
  \item{allow}{a vector of characters allowed by \R that should not be
	converted to periods in variable names.  By default, underscores in
	variable names are converted to periods as with \R before version 1.9.}
  \item{charfactor}{set to \code{TRUE} to change character variables to
	factors if they have fewer than n/2 unique values.  Blanks and null
	strings are converted to \code{NA}s.}
  \item{\dots}{arguments passed to \code{\link[foreign]{read.dta}}.}
}
\details{
  \code{stata.get} invokes the \code{\link[foreign]{read.dta}} function in the
  \pkg{foreign} package to read an STATA file, with a default output
  format of \code{\link{data.frame}}.  The \code{\link{label}} function is used to
  attach labels to individual variables instead of to the data frame as
  done by \code{\link[foreign]{read.dta}}.  By default, integer-valued variables are
  converted to a storage mode of integer unless
  \code{force.single=FALSE}.  Date variables are converted to \R
  \code{\link{Date}} variables.  By default, underscores in names are converted to periods.
}
\value{A data frame}
\author{Charles Dupont}
\seealso{\code{\link[foreign]{read.dta}},\code{\link{cleanup.import}},\code{\link{label}},\code{\link{data.frame}},\code{\link{Date}}}
\examples{
\dontrun{
w <- stata.get('/tmp/my.dta')
}
}
\keyword{interface}
\keyword{manip}
\keyword{file}
\concept{STATA data file}
