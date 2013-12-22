\name{ynbind}
\alias{ynbind}
\alias{[.ynbind}
\alias{pBlock}
\alias{[.pBlock}
\title{Combine Variables in a Matrix}
\description{
\code{ynbind} column binds a series of related yes/no variables,
allowing for a final argument \code{label} used to label the panel
created for the group.  \code{label}s for individual variables are
collected into a vector attribute \code{"labels"} for the result;
original variable names are used in place of labels for those variables
without labels.  A positive response is taken to be \code{y, yes,
present} (ignoring case) or a \code{logical} \code{TRUE} value.  By
default, the columns are sorted be ascending order or the overall
proportion of positives.  A subsetting method is provided for objects of
class \code{"ynbind"}.

\code{pBlock} creates a matrix similarly labeled, from a general set of
variables (without special handling of binaries), and sets to \code{NA}
any observation not in \code{subset} so that when that block of
variables is analyzed it will be only for that subset.
}

\usage{
ynbind(..., label = deparse(substitute(...)),
       asna = c("unknown", "unspecified"), sort = TRUE)

pBlock(..., subset=NULL, label = deparse(substitute(...)))
}
\arguments{
  \item{\dots}{a series of vectors}
  \item{label}{a label for the group, to be attached to the resulting
		matrix as a \code{"label"} attribute, used by \code{\link{summaryP}}.}
  \item{asna}{a vector of character strings specifying levels that are
		to be treated the same as \code{NA} if present}
  \item{sort}{set to \code{FALSE} to not sort the columns by their
		proportions}
	\item{subset}{subset criteria - either a vector of logicals or subscripts}
}
\value{a matrix of class \code{"ynbind"} or
	\code{"pBlock"} with \code{"label"} and \code{"labels"} attributes.
	For \code{"pBlock"}, factor input vectors will have values converted
	to \code{character}. 
}		
\author{Frank Harrell}
\seealso{\code{\link{summaryP}}}
\examples{
x1 <- c('yEs', 'no', 'UNKNOWN', NA)
x2 <- c('y', 'n', 'no', 'present')
label(x2) <- 'X2'
X <- ynbind(x1, x2, label='x1-2')
X[1:3,]

pBlock(x1, x2, subset=2:3, label='x1-2')
}
\keyword{misc}
\keyword{utilities}
