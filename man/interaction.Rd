\name{interaction}
\alias{interaction}
\title{
Compute the Interaction of Several Factors
}
\description{
This is a replacement for the builtin function \code{interaction} that adds
the parameters \code{sep} and \code{left}.
}
\usage{
interaction(\dots, drop=FALSE, sep=".", left=FALSE)
}
\arguments{
\item{...}{
the arguments to \code{interaction} can be either a data frame containing
all the factors to be used
all the individual factors.
It will not understand a combination of factors and designs as arguments;
you have to pick one form or the other.
}
\item{drop}{
if \code{TRUE} the levels of the new factor not represented in the data are dropped.
}
\item{sep}{
the separator in creating the level descriptors.
}
\item{left}{
set to \code{TRUE} to left-justify factor levels when constructing label strings
}}
\value{
a new factor, whose levels are all possible combinations of the factors
supplied as arguments. If \code{drop =TRUE}, only the levels represented in the
new factor are retained.  
}
\details{
This is a slight modification of the S-supplied function.  The \code{sep} argument
has been added, a sort step has been added to properly sort the levels of
the created variable, and the \code{left} parameter is added.
}
\examples{
Temp <- c(160, 180, 160, 180, 160, 180, 160, 180)
Conc <- c(20, 20, 40, 40, 20, 20, 40, 40)
interaction(Temp, Conc)
}
\keyword{design}
% Converted by Sd2Rd version 1.21.
