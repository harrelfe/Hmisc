\name{prnz}
\alias{prn}
\title{
Print and Object with its Name
}
\description{
Prints an object with its name and with an optional descriptive
text string.  This is useful for annotating analysis output files and
for debugging.
}
\usage{
prn(x, txt)
}
\arguments{
\item{x}{
any object
}
\item{txt}{
optional text string
}}
\section{Side Effects}{
prints
}
\seealso{
\code{\link{print}}, \code{\link{cat}}
}
\examples{
x <- 1:5
prn(x)
# prn(fit, 'Full Model Fit')
}
\keyword{print}
