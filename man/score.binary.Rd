\name{score.binary}
\alias{score.binary}
\title{
Score a Series of Binary Variables
}
\description{
Creates a new variable from a series of logical conditions.  The new
variable can be a hierarchical category or score derived from considering
the rightmost \code{TRUE} value among the input variables, an additive point
score, a union, or any of several others by specifying a function using the
\code{fun} argument.
}
\usage{
score.binary(\dots, fun=max, points=1:p, 
             na.rm=funtext == "max", retfactor=TRUE)
}
\arguments{
\item{...}{
a list of variables or expressions which are considered to be binary
or logical
}
\item{fun}{
a function to compute on each row of the matrix represented by
a specific observation of all the variables in \code{\dots}
}
\item{points}{
points to assign to successive elements of \code{\dots} .  The default is
\code{1, 2, \dots, p}, where \code{p} is the number of elements.  If you specify
one number for \code{points}, that number will be duplicated (i.e., equal weights
are assumed).
}
\item{na.rm}{
set to \code{TRUE} to remove \code{NA}s from consideration when processing
each row of the matrix of variables in \code{\dots} .  For \code{fun=max},
\code{na.rm=TRUE} is the default since \code{score.binary} assumes that a
hierarchical scale is based on available information.  Otherwise,
\code{na.rm=FALSE} is assumed.  For \code{fun=mean} you may want to specify
\code{na.rm=TRUE}.
}
\item{retfactor}{
applies if \code{fun=max}, in which case \code{retfactor=TRUE} makes \code{score.binary}
return a \code{factor} object since a hierarchical scale implies
a unique choice.
}}
\value{
a \code{factor} object if \code{retfactor=TRUE} and \code{fun=max} or a numeric vector
otherwise.  Will not contain NAs if \code{na.rm=TRUE} unless every variable in
a row is \code{NA}.  If a \code{factor} object
is returned, it has levels \code{"none"} followed by character
string versions of the arguments given in \code{\dots} .
}
\seealso{
\code{\link{any}}, \code{\link{sum}}, \code{\link{max}}, \code{\link{factor}}
}
\examples{
set.seed(1)
age <- rnorm(25, 70, 15)
previous.disease <- sample(0:1, 25, TRUE)
#Hierarchical scale, highest of 1:age>70  2:previous.disease
score.binary(age>70, previous.disease, retfactor=FALSE)
#Same as above but return factor variable with levels "none" "age>70" 
# "previous.disease"
score.binary(age>70, previous.disease)


#Additive scale with weights 1:age>70  2:previous.disease
score.binary(age>70, previous.disease, fun=sum)
#Additive scale, equal weights
score.binary(age>70, previous.disease, fun=sum, points=c(1,1))
#Same as saying points=1


#Union of variables, to create a new binary variable
score.binary(age>70, previous.disease, fun=any)
}
\keyword{manip}
% Converted by Sd2Rd version 1.21.
