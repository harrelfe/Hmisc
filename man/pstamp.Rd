\name{pstamp}
\alias{pstamp}
\title{Date/Time/Directory Stamp the Current Plot}
\description{
Date-time stamp the current plot in the extreme lower right
corner. Optionally add the current working directory and arbitrary other
text to the stamp.
}
\usage{
pstamp(txt, pwd = FALSE, time. = TRUE)
}
\arguments{
  \item{txt}{an optional single text string}
  \item{pwd}{set to \code{TRUE} to add the current working directory
	name to the stamp}
  \item{time.}{set to \code{FALSE} to use the date without the time}
}
\details{
  Certain functions are not supported for S-Plus under Windows.  For \R,
  results may not be satisfactory if \code{par(mfrow=)} is in effect.
}
\author{Frank Harrell}
\examples{
plot(1:20)
pstamp(pwd=TRUE, time=FALSE)
}
\keyword{aplot}
