\name{list.tree}
\alias{list.tree}
\title{
Pretty-print the Structure of a Data Object
}
\description{
This is a function to pretty-print the structure of any data object
(usually a list).  It is similar to the R function \code{str}.
}
\usage{
list.tree(struct, depth=-1, numbers=FALSE, maxlen=22, maxcomp=12, 
          attr.print=TRUE, front="", fill=". ", name.of, size=TRUE)
}
\arguments{
\item{struct}{
The object to be displayed
}
\item{depth}{
Maximum depth of recursion (of lists within lists \dots) to be printed; negative
value means no limit on depth.
}
\item{numbers}{
If TRUE, use numbers in leader  instead  of  dots  to
represent position in structure.
}
\item{maxlen}{
Approximate maximum length (in characters) allowed on each line to give the
first few values of a vector.  maxlen=0 suppresses printing any values.
}
\item{maxcomp}{
Maximum number of components of any list that will be described.
}
\item{attr.print}{
Logical flag, determining whether a description of attributes will be printed.
}
\item{front}{
Front material of a line, for internal use.
}
\item{fill}{
Fill character used for each level of indentation.
}
\item{name.of}{
Name of object, for internal use (deparsed version  of  struct  by  default). 
}
\item{size}{
Logical flag, should the size of the object in bytes be printed?


A description of the structure of struct will be printed in outline
form, with indentation
for each level of recursion, showing the internal storage mode, length,
class(es) if any, attributes, and first few elements of each data vector.
By default each level of list recursion is indicated by a "." and 
attributes by "A".
}}
\seealso{
\code{\link{str}}
}
\examples{
X <- list(a=ordered(c(1:30,30:1)),b=c("Rick","John","Allan"),
          c=diag(300),e=cbind(p=1008:1019,q=4))
list.tree(X)
# In R you can say str(X)
}
\author{
Alan Zaslavsky, \email{zaslavsk@hcp.med.harvard.edu}
}
\keyword{documentation}
% Converted by Sd2Rd version 1.21.


