\name{label}
\alias{label}
\alias{label<-}
\alias{labelPlotmath}
\alias{[.labelled}
\alias{print.labelled}
\alias{Label}
\alias{Label.data.frame}
\alias{llist}
\alias{plotmathTranslate}
\alias{as.data.frame.labelled}
\alias{data.frame.labelled}
\alias{reLabelled}
\title{
Label Attribute of an Object
}
\description{
\code{label(x)} retrieves the \code{label} attribute of \code{x}.
\code{label(x) <- "a label"} stores the label attribute, and also puts
the class \code{labelled} as the first class of \code{x} (for S-Plus 5
and later this class is not used and methods for handling this class are
not defined so the \code{"label"} and \code{"units"} attributes are lost
upon subsetting).  The reason for having this class is so that the
subscripting method for \code{labelled}, \code{[.labelled}, can preserve
the \code{label} attribute in R and S-Plus 2000.  Also, the \code{print}
method for \code{labelled} objects prefaces the print with the object's
\code{label} (and \code{units} if there).  If the variable is also given
a \code{"units"} attribute using the \code{units} function, subsetting
the variable (using \code{[.labelled}) will also retain the
\code{"units"} attribute.

\code{label} can optionally append a \code{"units"} attribute to the
string, and it can optionally return a string or expression (for \R's
\code{plotmath} facility) suitable for plotting.  \code{labelPlotmath}
is a function that also has this function, when the input arguments are
the \code{'label'} and \code{'units'} rather than a vector having those
attributes.  When \code{plotmath} mode is used to construct labels, the
\code{'label'} or \code{'units'} may contain math expressions but they
are typed verbatim if they contain percent signs, blanks, or underscores.

\code{Label} (actually \code{Label.data.frame}) is a function which generates
S-Plus source code that makes the labels in all the variables in a data
frame easy to edit. 

\code{llist} is like \code{list} except that it preserves the names or
labels of the component variables in the variables \code{label}
attribute.  This can be useful when looping over variables or using
\code{sapply} or \code{lapply}. By using \code{llist} instead of
\code{list} one can annotate the output with the current variable's name
or label.  \code{llist} also defines a \code{names} attribute for the
list and pulls the \code{names} from the arguments' expressions for
non-named arguments.

\code{plotmathTranslate} is a simple function that translates certain
character strings to character strings that can be used as part of \R
\code{plotmath} expressions.  If the input string has a space or percent
inside, the string is surrounded by a call to \code{plotmath}'s
\code{paste} function.

\code{as.data.frame.labelled} is a utility function that is called by
\code{[.data.frame}.  It is just a copy of \code{as.data.frame.vector}.
\code{data.frame.labelled} is another utility function, that adds a
class \code{"labelled"} to every variable in a data frame that has a
\code{"label"} attribute but not a \code{"labelled"} class.

\code{reLabelled} is used to add a \code{'labelled'} class back to
variables in data frame that have a 'label' attribute but no 'labelled'
oldClass.  Useful for changing \code{cleanup.import()}'d S-Plus 6 data
frames back to general form for \R and S-Plus 2000.
}
\usage{
label(x, units=FALSE, plot=FALSE, default=NULL, grid=FALSE)

label(x) <- value

labelPlotmath(label, units=NULL, plotmath=.R., grid=FALSE)

\method{print}{labelled}(x, \dots)   ## or x - calls print.labelled

Label(object, \dots)

\method{Label}{data.frame}(object, file='', append=FALSE, \dots)

llist(\dots, labels=TRUE)

plotmathTranslate(x)

data.frame.labelled(object)

reLabelled(object)
}
\arguments{
\item{x}{
any object (for \code{plotmathTranslate} is a character string)
}
\item{units}{
  set to \code{TRUE} to append the \code{'units'} attribute (if present)
  to the returned label.  The \code{'units'} are surrounded
  by brackets.  For \code{labelPlotmath} is a character string
  containing the units of measurement.
}
\item{plot}{
  set to \code{TRUE} to return a label suitable for \R's \code{plotmath}
  facility (returns an expression instead of a character string) if R is
  in effect.  If \code{units} is also \code{TRUE}, and if both
  \code{'label'} and \code{'units'} attributes are present, the
  \code{'units'} will appear after the label but in smaller type and
  will not be surrounded by brackets.
}
\item{default}{
  if \code{x} does not have a \code{'label'} attribute and
  \code{default} (a character string) is specified, the label will be
  taken as \code{default}
  }
\item{grid}{
  Currently \R's \code{lattice} and \code{grid} functions do not support
  \code{plotmath} expressions for \code{xlab} and \code{ylab}
  arguments.  When using \code{lattice} functions in \R, set the
  argument \code{grid} to \code{TRUE} so that \code{labelPlotmath} can
  return an ordinary character string instead of an expression.
  }
\item{label}{a character string containing a variable's label}
\item{plotmath}{
  set to \code{TRUE} to have \code{labelMathplot} return an expression
  for plotting using \R's \code{plotmath} facility.  If \R is not in
  effect, an ordinary character string is returned.
  }
\item{value}{
the label of the object, or "".
}
\item{object}{
a data frame
}
\item{\dots}{
a list of variables or expressions to be formed into a \code{list}.
Ignored for \code{print.labelled}.
}
\item{file}{
the name of a file to which to write S-Plus source code.  Default is
\code{""}, meaning standard output.
}
\item{append}{
set to \code{TRUE} to append code generated by \code{Label} to file \code{file}
}
\item{labels}{
set to \code{FALSE} to make \code{llist} ignore the variables' \code{label} attribute and
use the variables' names.
}}
\value{
\code{label} returns the label attribute of x, if any; otherwise, "".  
\code{label} is used
most often for the individual variables in data frames.  The function
\code{sas.get} copies labels over from SAS if they exist.
}
\seealso{
\code{\link{sas.get}}, \code{\link{describe}}
}
\examples{
age <- c(21,65,43)
y   <- 1:3
label(age) <- "Age in Years"
plot(age, y, xlab=label(age))


x1 <- 1:10
x2 <- 10:1
label(x2) <- 'Label for x2'
units(x2) <- 'mmHg'
x2
x2[1:5]
dframe <- data.frame(x1, x2)
Label(dframe)


##In these examples of llist, note that labels are printed after
##variable names, because of print.labelled
a <- 1:3
b <- 4:6
label(b) <- 'B Label'
llist(a,b)
llist(a,b,d=0)
llist(a,b,0)


w <- llist(a, b>5, d=101:103)
sapply(w, function(x){
  hist(as.numeric(x), xlab=label(x))
  # locator(1)   ## wait for mouse click
})

# Or: for(u in w) {hist(u); title(label(u))}
}
\keyword{interface}
\keyword{misc}
\keyword{utilities}
% Converted by Sd2Rd version 1.21.
