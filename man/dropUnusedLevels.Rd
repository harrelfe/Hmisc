\name{dropUnusedLevels}
\alias{dropUnusedLevels}
\title{Create Temporary Factor Subsetting Function}
\description{
Calling this function makes Hmisc have its pre-version 3.0 behavior in
which the \R \code{[.factor} function was overridden by a customized
version that caused unused \code{factor} \code{level}s to be dropped
when the factor variable was subscripted (subsetted).
\code{dropUnusedLevels()} creates a temporary version of \code{[.factor}
in the global environment, which will take precedence.  To later make
this function keep unused levels on subsetting, issue
\code{options(drop.unused.levels=FALSE)} or just remove this temporary
function from the environment by issuing
\code{remove('[.factor',pos='.GlobalEnv')}.
}
\usage{
dropUnusedLevels()
}
\author{Frank Harrell and Jens Oehlschlaegel}
\seealso{\code{\link{factor}},\code{\link{[.factor}}}
\examples{
\dontrun{
x <- factor(c('a','b','c'))
x[1:2]  # keeps level c
dropUnusedLevels()
x[1:2]  # no c any more
}}
\keyword{category}
\keyword{utilities}
\keyword{programming}
\keyword{methods}

