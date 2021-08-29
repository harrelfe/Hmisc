\name{na.delete}
\alias{na.delete}
\title{
Row-wise Deletion na.action
}
\description{
Does row-wise deletion as \code{na.omit}, but adds frequency of missing values
for each predictor
to the \code{"na.action"} attribute of the returned model frame.
Optionally stores further details if \code{options(na.detail.response=TRUE)}. 
}
\usage{
na.delete(frame)
}
\arguments{
\item{frame}{
a model frame
}}
\value{
a model frame with rows deleted and the \code{"na.action"} attribute added.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{fh@fharrell.com}
}
\seealso{
\code{\link{na.omit}}, \code{\link{na.keep}}, \code{\link{na.detail.response}}, \code{\link{model.frame.default}},
\code{\link{naresid}}, \code{\link{naprint}}
}
\examples{
# options(na.action="na.delete")
# ols(y ~ x)
}
\keyword{models}
% Converted by Sd2Rd version 1.21.
