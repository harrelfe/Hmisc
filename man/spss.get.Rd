\name{spss.get}
\alias{spss.get}
\title{Enhanced Importing of SPSS Files}
\description{
\code{spss.get} invokes the \code{read.spss} function in the
\code{foreign} package to read an SPSS file, with a default output
format of \code{"data.frame"}.  The \code{label} function is used to
attach labels to individual variables instead of to the data frame as
done by \code{read.spss}.  By default, integer-valued variables are
converted to a storage mode of integer unless
\code{force.single=FALSE}.  Date variables are converted to \R \code{Date}
variables.
}
\usage{
spss.get(file, datevars = NULL, use.value.labels = TRUE, to.data.frame =
TRUE, max.value.labels = Inf, force.single=TRUE)
}
\arguments{
  \item{file}{input SPSS save file}
  \item{datevars}{vector of variable names containing dates to be
	converted to \R internal format}
  \item{use.value.labels}{see \code{\link{read.spss}}}
  \item{to.data.frame}{see \code{\link{read.spss}}; default is
	\code{TRUE} for \code{spss.get}}
  \item{max.value.labels}{see \code{\link{read.spss}}}
  \item{force.single}{set to \code{FALSE} to prevent integer-valued
	variables from being converted from storage mode \code{double} to
	\code{integer}}
}
\value{
  a data frame or list
}
\author{Frank Harrell}
\seealso{\code{\link{read.spss}},\code{\link{cleanup.import}},\code{\link{sas.get}}}

\examples{
\dontrun{
w <- spss.get('/tmp/my.sav', datevars=c('birthdate','deathdate'))
  }
}
\keyword{interface}
\keyword{manip}
