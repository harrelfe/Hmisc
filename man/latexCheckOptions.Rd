\name{latexCheckOptions}
\alias{latexCheckOptions}
\title{Check whether the options for latex functions have been specified.}
\description{
Check whether the options for latex functions have been specified.
If any of\cr
\code{options()[c("latexcmd","dviExtension","xdvicmd")]}
are \code{NULL}, an error message is displayed.
}
\usage{
latexCheckOptions(...)
}
\arguments{
  \item{\dots}{
Any arguments are ignored.
}
}
\value{
If any \code{NULL} options are detected, the invisible text of the
error message.  If all three options have non-\code{NULL} values, NULL.
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link[Hmisc]{latex}}
}
\keyword{ utilities}
