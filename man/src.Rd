\name{src}
\alias{src}
\title{Source a File from the Current Working Directory}
\description{
\code{src} concatenates \code{".s"} to its argument, quotes the result,
and \code{source}s in the file.  It sets \code{options(last.source)} to
this file name so that \code{src()} can be issued to re-\code{source}
the file when it is edited.
}
\usage{
src(x)
}
\arguments{
  \item{x}{an unquoted file name aside from \code{".s"}.  This base file
  name must be a legal S name.}
}
\section{Side Effects}{
  Sets system option \code{last.source}
}
\author{Frank Harrell}
\seealso{\code{\link{source}}}
\examples{
\dontrun{
src(myfile)   # source("myfile.s")
src()         # re-source myfile.s
}
}
\keyword{file}
\keyword{programming}
\keyword{utilities}
