\name{event.convert}
\alias{event.convert}
\title{
  Event Conversion for Time-to-Event Data
}
\description{
  Convert a two-column data matrix with event time and event code into
  multiple column event time with one event in each column
}
\usage{
event.convert(data2, event.time = 1, event.code = 2)
}
\arguments{
  \item{data2}{
    a matrix or dataframe with at least 2 columns; by default, the first
    column contains the event time and the second column contains the k
    event codes (e.g. 1=dead, 0=censord)
  }
  \item{event.time}{
    the column number in data contains the event time
  }
  \item{event.code}{
    the column number in data contains the event code
  }
}
\details{
  In the survival analysis, the data typically come  in  two
  columns: one column containing survival time and the other
  containing  censoring  indicator  or   event   code.   The
  \code{event.convert}  function  converts  this  type of data into
  multiple columns of event times, one column of each  event
  type, suitable for the \code{event.chart} function.
}
\author{
  J. Jack Lee and Kenneth R. Hess
  \cr
  Department of Biostatistics
  \cr
  University of Texas
  \cr
  M.D. Anderson Cancer Center
  \cr
  Houston, TX 77030
  \cr
  \email{jjlee@mdanderson.org}, \email{khess@mdanderson.org}


  Joel A. Dubin
  \cr
  Department of Statistics
  \cr
  University of Waterloo
  \cr
  \email{jdubin@uwaterloo.ca}
}
\seealso{
  \code{\link{event.history}}, \code{\link{Date}}, \code{\link{event.chart}}
}
\examples{
# To convert coded time-to-event data, then, draw an event chart:
surv.time <- c(5,6,3,1,2)
cens.ind   <- c(1,0,1,1,0)
surv.data  <- cbind(surv.time,cens.ind)
event.data <- event.convert(surv.data)
event.chart(cbind(rep(0,5),event.data),x.julian=TRUE,x.reference=1)
}
\keyword{hplot}
\keyword{survival}
