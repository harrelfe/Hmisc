\name{sys}
\alias{sys}
\title{
Run Unix or Dos Depending on System
}
\description{
Runs \code{unix} or \code{dos} depending on the current operating system.  For
\R, just runs \code{system} with optional concatenation of first two
arguments which are assumed named \code{command} and \code{text}.
}
\usage{
sys(command, text=NULL, output=TRUE)
# S-Plus: sys(\dots, minimized=FALSE)
}
\arguments{
\item{command}{
system command to execute
}
\item{text}{
text to concatenate to system command, if any (typically options or file
names or both)
}
\item{output}{
  set to \code{FALSE} to not return output of command as a character
  vector
}
}
\value{
see \code{unix} or \code{dos}
}
\section{Side Effects}{
executes system commands
}
\seealso{
\code{\link{unix}}, \code{\link{system}}
}
\keyword{interface}
