\name{rcspline.eval}
\alias{rcspline.eval}
\title{
Restricted Cubic Spline Design Matrix
}
\description{
Computes matrix that expands a single variable into the terms needed to
fit a restricted cubic spline (natural spline) function using the
truncated power basis. Two normalization options are given for somewhat
reducing problems of ill-conditioning.  The antiderivative function can
be optionally created. If knot locations are not given, they will be
estimated from the marginal distribution of \code{x}.
}
\usage{
rcspline.eval(x, knots, nk=5, inclx=FALSE, knots.only=FALSE, 
              type="ordinary", norm=2, rpm=NULL)
}
\arguments{
\item{x}{
a vector representing a predictor variable
}
\item{knots}{
knot locations. If not given, knots will be estimated using default
quantiles of \code{x}. For 3-5 knots, the outer quantiles used are .05 and .95.
For \code{nk>5}, the outer quantiles are .025 and .975. The knots are
equally spaced between these on the quantile scale. For fewer than 100
non-missing values of \code{x}, the outer knots are the 5th smallest and
largest \code{x}.
}
\item{nk}{
number of knots. Default is 5. The minimum value is 3.
}
\item{inclx}{
set to \code{TRUE} to add \code{x} as the first column of the returned matrix
}
\item{knots.only}{
return the estimated knot locations but not the expanded matrix
}
\item{type}{
\code{"ordinary"} to fit the function, \code{"integral"} to fit its anti-derivative.
}
\item{norm}{
\code{0} to use the terms as originally given by Devlin and Weeks (1986),
\code{1} to normalize non-linear terms by the cube of the spacing between the last two
knots, \code{2} to normalize by the square of the spacing between the first
and last knots (the default).
\code{norm=2} has the advantage of making all
nonlinear terms be on the \code{x}-scale.
}
\item{rpm}{
If given, any NAs in \code{x} will be replaced with the value \code{rpm} after
estimating any knot locations.
}}
\value{
If \code{knots.only=TRUE}, returns a vector of knot locations. Otherwise returns
a matrix with \code{x} (if \code{inclx=TRUE}) followed by \code{nk-2} nonlinear terms.
The matrix has an attribute \code{knots} which is the vector of knots used.
}
\references{
Devlin TF and Weeks BJ (1986): Spline functions for logistic regression
modeling. Proc 11th Annual SAS Users Group Intnl Conf, p. 646--651.
Cary NC: SAS Institute, Inc.
}
\seealso{
\code{\link{ns}}, \code{\link{rcspline.restate}}, \code{\link[Design]{rcs}}
}
\examples{
x <- 1:100
rcspline.eval(x, nk=4, inclx=TRUE)
#lrm.fit(rcspline.eval(age,nk=4,inclx=TRUE), death)
}
\keyword{regression}
\keyword{smooth}
% Converted by Sd2Rd version 1.21.
