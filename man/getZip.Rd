\name{getZip}
\alias{getZip}
\title{Open a Zip File From a URL}
\description{
  Allows downloading and reading of a zip file containing one file
}
\usage{
getZip(url, password=NULL)
}
\arguments{
  \item{url}{either a path to a local file or a valid URL.}
  \item{password}{required to decode password-protected zip files}
}
\value{
  Returns a file O/I pipe.
}
\details{
  Allows downloading and reading of zip file containing one file.
  The file may be password protected.  If a password is needed then one will be requested unless given.

  Note: to make password-protected zip file z.zip, do zip -e z myfile
}
\seealso{
  \code{\link{pipe}}
}
\examples{
\dontrun{
read.csv(getZip('http://test.com/z.zip'))
}
}
\author{Frank E. Harrell}
\keyword{file}
\keyword{IO}
\concept{compressed file}
