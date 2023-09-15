\name{find.matches}
\alias{find.matches}
\alias{summary.find.matches}
\alias{print.find.matches}
\alias{matchCases}
\title{
Find Close Matches
}
\description{
Compares each row in \code{x} against all the rows in \code{y}, finding rows in
\code{y} with all columns within a tolerance of the values a given row of
\code{x}.  The default tolerance
\code{tol} is zero, i.e., an exact match is required on all columns.
For qualifying matches, a distance measure is computed.  This is
the sum of squares of differences between \code{x} and \code{y} after scaling
the columns.  The default scaling values are \code{tol}, and for columns
with \code{tol=1} the scale values are set to 1.0 (since they are ignored
anyway).  Matches (up to \code{maxmatch} of them) are stored and listed in order of 
increasing distance.
\cr
The \code{summary} method prints a frequency distribution of the
number of matches per observation in \code{x}, the median of the minimum
distances for all matches per \code{x}, as a function of the number of matches,
and the frequency of selection of duplicate observations as those having
the smallest distance.  The \code{print} method prints the entire \code{matches}
and \code{distance} components of the result from \code{find.matches}.
\cr
\code{matchCases} finds all controls that match cases on a single variable
\code{x} within a tolerance of \code{tol}.  This is intended for prospective
cohort studies that use matching for confounder adjustment (even
though regression models usually work better).
}
\usage{
find.matches(x, y, tol=rep(0, ncol(y)), scale=tol, maxmatch=10)
\method{summary}{find.matches}(object, \dots)
\method{print}{find.matches}(x, digits, \dots)

matchCases(xcase,    ycase,    idcase=names(ycase),
           xcontrol, ycontrol, idcontrol=names(ycontrol),
           tol=NULL,
           maxobs=max(length(ycase),length(ycontrol))*10,
           maxmatch=20, which=c('closest','random'))
}
\arguments{
\item{x}{
a numeric matrix or the result of \code{find.matches}
}
\item{y}{
a numeric matrix with same number of columns as \code{x}
}
\item{xcase}{numeric vector to match on for cases}
\item{xcontrol}{numeric vector to match on for controls, not necessarily
           the same length as \code{xcase}}
\item{ycase}{a vector or matrix}
\item{ycontrol}{
\code{ycase} and \code{ycontrol} are vectors or matrices, not necessarily having the same number of rows,
specifying a variable to carry along from cases and matching
controls.  If you instead want to carry along rows from a data frame,
let \code{ycase} and \code{ycontrol} be non-overlapping integer subscripts of
the donor data frame.
}
\item{tol}{
a vector of tolerances with number of elements the same as the number
of columns of \code{y}, for \code{find.matches}.  For \code{matchCases}
is a scalar tolerance.
}
\item{scale}{
a vector of scaling constants with number of elements the same as the
number of columns of \code{y}.
}
\item{maxmatch}{
maximum number of matches to allow.  For \code{matchCases},
maximum number of controls to match with a case (default is 20).  If more than
\code{maxmatch} matching controls are available, a random sample without
replacement of \code{maxmatch} controls is used (if \code{which="random"}).
}
\item{object}{an object created by \code{find.matches}}
\item{digits}{number of digits to use in printing distances}
\item{idcase}{vector the same length as \code{xcase}}
\item{idcontrol}{
\code{idcase} and \code{idcontrol} are vectors the same length as
           \code{xcase} and \code{xcontrol} respectively, 
specifying the id of cases and controls.  Defaults are integers
specifying original element positions within each of cases and
controls.
}
\item{maxobs}{
maximum number of cases and all matching controls combined (maximum
dimension of data frame resulting from \code{matchControls}).  Default is
ten times the maximum of the number of cases and number of controls.
\code{maxobs} is used to allocate space for the resulting data frame.
}
\item{which}{
set to \code{"closest"} (the default) to match cases with up to \code{maxmatch}
controls that most closely match on \code{x}.  Set \code{which="random"} to use
randomly chosen controls.  In either case, only those controls within
\code{tol} on \code{x} are allowed to be used.
}
\item{\dots}{unused}
}
\value{
\code{find.matches} returns a list of class \code{find.matches} with elements
\code{matches} and \code{distance}. 
Both elements are matrices with the number of rows equal to the number
of rows in \code{x}, and with \code{k} columns, where \code{k} is the maximum number of
matches (\code{<= maxmatch}) that occurred.  The elements of \code{matches}
are row identifiers of \code{y} that match, with zeros if fewer than
\code{maxmatch} matches are found (blanks if \code{y} had row names).
\code{matchCases} returns a data frame with variables \code{idcase} (id of case
currently being matched), \code{type} (factor variable with levels \code{"case"}
and \code{"control"}), \code{id} (id of case if case row, or id of matching
case), and \code{y}.
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
\references{
Ming K, Rosenbaum PR (2001): A note on optimal matching with variable
controls using the assignment algorithm.  J Comp Graph Stat
10:455--463.

Cepeda MS, Boston R, Farrar JT, Strom BL (2003): Optimal matching with a
variable number of controls vs. a fixed number of controls for a cohort
study: trade-offs.  J Clin Epidemiology 56:230-237.
Note: These papers were not used for the functions here but
probably should have been.

}
\seealso{
\code{\link{scale}}, \code{\link{apply}}
}
\examples{
y <- rbind(c(.1, .2),c(.11, .22), c(.3, .4), c(.31, .41), c(.32, 5))
x <- rbind(c(.09,.21), c(.29,.39))
y
x
w <- find.matches(x, y, maxmatch=5, tol=c(.05,.05))


set.seed(111)       # so can replicate results
x <- matrix(runif(500), ncol=2)
y <- matrix(runif(2000), ncol=2)
w <- find.matches(x, y, maxmatch=5, tol=c(.02,.03))
w$matches[1:5,]
w$distance[1:5,]
# Find first x with 3 or more y-matches
num.match <- apply(w$matches, 1, function(x)sum(x > 0))
j <- ((1:length(num.match))[num.match > 2])[1]
x[j,]
y[w$matches[j,],]


summary(w)


# For many applications would do something like this:
# attach(df1)
# x <- cbind(age, sex) # Just do as.matrix(df1) if df1 has no factor objects
# attach(df2)
# y <- cbind(age, sex)
# mat <- find.matches(x, y, tol=c(5,0)) # exact match on sex, 5y on age


# Demonstrate matchCases
xcase     <- c(1,3,5,12)
xcontrol  <- 1:6
idcase    <- c('A','B','C','D')
idcontrol <- c('a','b','c','d','e','f')
ycase     <- c(11,33,55,122)
ycontrol  <- c(11,22,33,44,55,66)
matchCases(xcase, ycase, idcase,
           xcontrol, ycontrol, idcontrol, tol=1)


# If y is a binary response variable, the following code
# will produce a Mantel-Haenszel summary odds ratio that 
# utilizes the matching.
# Standard variance formula will not work here because
# a control will match more than one case
# WARNING: The M-H procedure exemplified here is suspect 
# because of the small strata and widely varying number
# of controls per case.


x    <- c(1, 2, 3, 3, 3, 6, 7, 12,  1, 1:7)
y    <- c(0, 0, 0, 1, 0, 1, 1,  1,  1, 0, 0, 0, 0, 1, 1, 1)
case <- c(rep(TRUE, 8), rep(FALSE, 8))
id   <- 1:length(x)


m <- matchCases(x[case],  y[case],  id[case],
                x[!case], y[!case], id[!case], tol=1)
iscase <- m$type=='case'
# Note: the first tapply on insures that event indicators are
# sorted by case id.  The second actually does something.
event.case    <- tapply(m$y[iscase],  m$idcase[iscase],  sum)
event.control <- tapply(m$y[!iscase], m$idcase[!iscase], sum)
n.control     <- tapply(!iscase,      m$idcase,          sum)
n             <- tapply(m$y,          m$idcase,          length)
or <- sum(event.case * (n.control - event.control) / n) /
      sum(event.control * (1 - event.case) / n)
or


# Bootstrap this estimator by sampling with replacement from
# subjects.  Assumes id is unique when combine cases+controls
# (id was constructed this way above).  The following algorithms
# puts all sampled controls back with the cases to whom they were
# originally matched.


ids <- unique(m$id)
idgroups <- split(1:nrow(m), m$id)
B   <- 50   # in practice use many more
ors <- numeric(B)
# Function to order w by ids, leaving unassigned elements zero
align <- function(ids, w) {
  z <- structure(rep(0, length(ids)), names=ids)
  z[names(w)] <- w
  z
}
for(i in 1:B) {
  j <- sample(ids, replace=TRUE)
  obs <- unlist(idgroups[j])
  u <- m[obs,]
  iscase <- u$type=='case'
  n.case <- align(ids, tapply(u$type, u$idcase, 
                              function(v)sum(v=='case')))
  n.control <- align(ids, tapply(u$type, u$idcase,
                                 function(v)sum(v=='control')))
  event.case <- align(ids, tapply(u$y[iscase],  u$idcase[iscase],  sum))
  event.control <- align(ids, tapply(u$y[!iscase], u$idcase[!iscase], sum))
  n <- n.case + n.control
  # Remove sets having 0 cases or 0 controls in resample
  s             <- n.case > 0 & n.control > 0
  denom <- sum(event.control[s] * (n.case[s] - event.case[s]) / n[s])
  or <- if(denom==0) NA else 
   sum(event.case[s] * (n.control[s] - event.control[s]) / n[s]) / denom
  ors[i] <- or
}
describe(ors)
}
\keyword{math}
\keyword{multivariate}
\keyword{htest}
\concept{bootstrap}
\concept{matching}
\concept{epidemiology}
\concept{case-control}
