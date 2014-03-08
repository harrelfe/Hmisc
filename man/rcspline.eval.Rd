\name{rcspline.eval}
\alias{rcspline.eval}
\title{
  Restricted Cubic Spline Design Matrix
}
\description{
  Computes matrix that expands a single variable into the terms needed
  to fit a restricted cubic spline (natural spline) function using the
  truncated power basis. Two normalization options are given for
  somewhat reducing problems of ill-conditioning.  The antiderivative
  function can be optionally created. If knot locations are not given,
  they will be estimated from the marginal distribution of \code{x}.
}
\usage{
rcspline.eval(x, knots, nk=5, inclx=FALSE, knots.only=FALSE, 
              type="ordinary", norm=2, rpm=NULL, pc=FALSE,
              fractied=0.05)
}
\arguments{
  \item{x}{
    a vector representing a predictor variable
  }
  \item{knots}{
    knot locations. If not given, knots will be estimated using default
    quantiles of \code{x}. For 3 knots, the outer quantiles used are 0.10
    and 0.90. For 4-6 knots, the outer quantiles used are 0.05 and
    0.95. For \eqn{\code{nk}>6}, the outer quantiles are 0.025 and 0.975. The
    knots are equally spaced between these on the quantile scale. For
    fewer than 100 non-missing values of \code{x}, the outer knots are
    the 5th smallest and largest \code{x}.
  }
  \item{nk}{
    number of knots. Default is 5. The minimum value is 3.
  }
  \item{inclx}{
    set to \code{TRUE} to add \code{x} as the first column of the
    returned matrix
  }
  \item{knots.only}{
    return the estimated knot locations but not the expanded matrix
  }
  \item{type}{
    \samp{"ordinary"} to fit the function, \samp{"integral"} to fit its
    anti-derivative.
  }
  \item{norm}{
    \samp{0} to use the terms as originally given by \cite{Devlin and
      Weeks (1986)}, \samp{1} to normalize non-linear terms by the cube
    of the spacing between the last two knots, \samp{2} to normalize by
    the square of the spacing between the first and last knots (the
    default). \code{norm=2} has the advantage of making all nonlinear
    terms beon the x-scale.
  }
  \item{rpm}{
    If given, any \code{NA}s in \code{x} will be replaced with the value
    \code{rpm} after estimating any knot locations.
  }
  \item{pc}{
    Set to \code{TRUE} to replace the design matrix with orthogonal
    (uncorrelated) principal components computed on the scaled, centered
    design matrix
  }
	\item{fractied}{
		If the fraction of observations tied at the lowest and/or highest
		values of \code{x} is greater than or equal to \code{fractied}, the
		algorithm attempts to use a different algorithm for knot finding
		based on quantiles of \code{x} after excluding the one or two values
		with excessive ties.  And if the number of unique \code{x} values
		excluding these values is small, the unique values will be used as
		the knots.  If the number of knots to use other than these exterior
		values is only one, that knot will be at the median of the
		non-extreme \code{x}.  This algorithm is not used if any interior
		values of \code{x} also have a proportion of ties equal to or
		exceeding \code{fractied}.}
}
\value{
  If \code{knots.only=TRUE}, returns a vector of knot
  locations. Otherwise returns a matrix with \code{x} (if
  \code{inclx=TRUE}) followed by \eqn{\code{nk}-2} nonlinear terms. The
  matrix has an attribute \code{knots} which is the vector of knots
  used.  When \code{pc} is \code{TRUE}, an additional attribute is
  stored: \code{pcparms}, which contains the \code{center} and
  \code{scale} vectors and the \code{rotation} matrix.
}
\references{
  Devlin TF and Weeks BJ (1986): Spline functions for logistic regression
  modeling. Proc 11th Annual SAS Users Group Intnl Conf, p. 646--651.
  Cary NC: SAS Institute, Inc.
}
\seealso{
  \code{\link[splines]{ns}}, \code{\link{rcspline.restate}},
  \code{\link[rms]{rcs}}
}
\examples{
x <- 1:100
rcspline.eval(x, nk=4, inclx=TRUE)
#lrm.fit(rcspline.eval(age,nk=4,inclx=TRUE), death)
x <- 1:1000
attributes(rcspline.eval(x))
x <- c(rep(0, 744),rep(1,6), rep(2,4), rep(3,10),rep(4,2),rep(6,6),
  rep(7,3),rep(8,2),rep(9,4),rep(10,2),rep(11,9),rep(12,10),rep(13,13),
  rep(14,5),rep(15,5),rep(16,10),rep(17,6),rep(18,3),rep(19,11),rep(20,16),
  rep(21,6),rep(22,16),rep(23,17), 24, rep(25,8), rep(26,6),rep(27,3),
  rep(28,7),rep(29,9),rep(30,10),rep(31,4),rep(32,4),rep(33,6),rep(34,6),
  rep(35,4), rep(36,5), rep(38,6), 39, 39, 40, 40, 40, 41, 43, 44, 45)
attributes(rcspline.eval(x, nk=3))
attributes(rcspline.eval(x, nk=5))
u <- c(rep(0,30), 1:4, rep(5,30))
attributes(rcspline.eval(u))
}
\keyword{regression}
\keyword{smooth}
