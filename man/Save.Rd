\name{Save}
\alias{Save}
\alias{Load}
\title{Faciliate Use of save and load to Remote Directories}
\description{
These functions are slightly enhanced versions of \code{save} and
\code{load} that allow a target directory to be specified using
\code{options(LoadPath="pathname")}.  If the \code{LoadPath} option is
not set, the current working directory is used.
}
\usage{
# options(LoadPath='mypath')
Save(object)
Load(object)
}
\arguments{
  \item{object}{the name of an object, usually a data frame.  It must
not be quoted.}
}
\details{
\code{Save} creates a temporary version of the object under the name
given by the user, so that \code{save} will internalize this name.
Then subsequent \code{Load} or \code{load} will cause an object of the
original name to be created in the global environment.  The name of
the \R data file is assumed to be the name of the object appended with
\code{".rda"}.  For \code{Save}, compression is used.
}
\author{Frank Harrell}
\seealso{\code{\link{save}}, \code{\link{load}}}
\examples{
\dontrun{
d <- data.frame(x=1:3, y=11:13)
options(LoadPath='../data/rda')
Save(d)   # creates ../data/rda/d.rda
Load(d)   # reads   ../data/rda/d.rda
}
}
\keyword{data}
\keyword{file}
\keyword{utilities}
