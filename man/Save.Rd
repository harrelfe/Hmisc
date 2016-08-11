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
Save(object, name=deparse(substitute(object)), compress=TRUE)
Load(object)
}
\arguments{
  \item{object}{the name of an object, usually a data frame.  It must
	not be quoted.}
  \item{name}{an optional name to assign to the object and file name
		prefix, if the argument name is not used}
	\item{compress}{see \code{\link{save}}.  Default is \code{TRUE}
		which corresponds to \code{gzip}.}
}
\details{
\code{Save} creates a temporary version of the object under the name
given by the user, so that \code{save} will internalize this name.
Then subsequent \code{Load} or \code{load} will cause an object of the
original name to be created in the global environment.  The name of
the \R data file is assumed to be the name of the object (or the value
of \code{name}) appended with \code{".rda"}.
}
\author{Frank Harrell}
\seealso{\code{\link{save}}, \code{\link{load}}}
\examples{
\dontrun{
d <- data.frame(x=1:3, y=11:13)
options(LoadPath='../data/rda')
Save(d)   # creates ../data/rda/d.rda
Load(d)   # reads   ../data/rda/d.rda
Save(d, 'D')   # creates object D and saves it in .../D.rda
}
}
\keyword{data}
\keyword{file}
\keyword{utilities}
