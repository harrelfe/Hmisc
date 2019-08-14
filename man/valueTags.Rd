\name{valueTags}
\alias{valueTags}
\alias{valueTags<-}
\alias{valueLabel}
\alias{valueLabel<-}
\alias{valueUnit}
\alias{valueUnit<-}
\alias{valueName}
\alias{valueName<-}
\title{Store Descriptive Information About an Object}
\description{
  Functions get or set useful information about the contents of the
  object for later use.
}
\usage{
valueTags(x)
valueTags(x) <- value

valueLabel(x)
valueLabel(x) <- value

valueName(x)
valueName(x) <- value

valueUnit(x)
valueUnit(x) <- value
}
\arguments{
  \item{x}{
    an object
  }
  \item{value}{
    for \code{valueTags<-} a named list of value tags.
    a character vector of length 1, or \code{NULL}.
  }
}
\value{
  \code{valueTag} returns \code{NULL} or a named list with each of the
  named values \code{name}, \code{label}, \code{unit} set if they exists
  in the object.

  For \code{valueTag<-} returns \code{list}

  For \code{valueName}, \code{valueLable}, and \code{valueUnit}  returns
  \code{NULL} or character vector of length 1.
  
  For \code{valueName<-}, \code{valueLabel<-}, and \code{valueUnit} returns \code{value}
}
\details{
  These functions store the a short name of for the contents, a longer
  label that is useful for display, and the units of the contents that
  is useful for display.

  \code{valueTag} is an accessor, and \code{valueTag<-} is a replacement
  function for all of the value's information.

  \code{valueName} is an accessor, and \code{valueName<-} is a
  replacement function for the value's name.  This name is used when a
  plot or a latex table needs a short name and the variable name is not
  useful.
  
  \code{valueLabel} is an accessor, and \code{valueLabel<-} is a
  replacement function for the value's label.  The label is used in a
  plots or latex tables when they need a descriptive name.

  \code{valueUnit} is an accessor, and \code{valueUnit<-} is a
  replacement function for the value's unit.  The unit is used to add
  unit information to the R output.
}
\seealso{
  \code{\link{names}}, \code{\link{attributes}}
}
\examples{
age <- c(21,65,43)
y   <- 1:3
valueLabel(age) <- "Age in Years"
plot(age, y, xlab=valueLabel(age))


x1 <- 1:10
x2 <- 10:1
valueLabel(x2) <- 'Label for x2'
valueUnit(x2) <- 'mmHg'
x2
x2[1:5]
dframe <- data.frame(x1, x2)
Label(dframe)


##In these examples of llist, note that labels are printed after
##variable names, because of print.labelled
a <- 1:3
b <- 4:6
valueLabel(b) <- 'B Label'
}
\author{Charles Dupont}
\keyword{attribute}
\keyword{misc}
\keyword{utilities}
