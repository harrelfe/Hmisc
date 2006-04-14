\name{stata.get}
\alias{stata.get}
\title{Enhanced Importing of STATA Files}
\description{
  Reads a file in Stata version 5-8 or 7/SE binary format into a
  data frame.
}
\usage{
stata.get(file, lowernames = FALSE, convert.dates = TRUE,
          convert.factors = TRUE, missing.type = FALSE,
          convert.underscore = TRUE, warn.missing.labels = TRUE,
          force.single = TRUE)
}
\arguments{
  \item{file}{input SPSS save file.  May be a file on the WWW, indicated
    by \code{file} starting with \code{'http://'}.}
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
\seealso{\code{\link[foreign]{read.dta}},\code{\link{cleanup.import}},\code{\link{label}},\code{\label{data.frame}},\code{\link{Date}}}
\examples{
\dontrun{
w <- stata.get(\sQuote{/tmp/my.dta})
}
}
\keyword{interface}
\keyword{manip}
\keyword{file}
\concept{STATA data file}