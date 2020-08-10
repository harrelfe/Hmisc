\name{areg}
\alias{areg}
\alias{print.areg}
\alias{predict.areg}
\alias{plot.areg}
\title{Additive Regression with Optimal Transformations on Both Sides using
Canonical Variates}
\description{
Expands continuous variables into restricted cubic spline bases and
categorical variables into dummy variables and fits a multivariate
equation using canonical variates.  This finds optimum transformations
that maximize \eqn{R^2}.  Optionally, the bootstrap is used to estimate
the covariance matrix of both left- and right-hand-side transformation
parameters, and to estimate the bias in the \eqn{R^2} due to overfitting
and compute the bootstrap optimism-corrected \eqn{R^2}.
Cross-validation can also be used to get an unbiased estimate of
\eqn{R^2} but this is not as precise as the bootstrap estimate.  The
bootstrap and cross-validation may also used to get estimates of mean
and median absolute error in predicted values on the original \code{y}
scale.  These two estimates are perhaps the best ones for gauging the
accuracy of a flexible model, because it is difficult to compare
\eqn{R^2} under different y-transformations, and because \eqn{R^2}
allows for an out-of-sample recalibration (i.e., it only measures
relative errors).

Note that uncertainty about the proper transformation of \code{y} causes
an enormous amount of model uncertainty.  When the transformation for
\code{y} is estimated from the data a high variance in predicted values
on the original \code{y} scale may result, especially if the true
transformation is linear.  Comparing bootstrap or cross-validated mean
absolute errors with and without restricted the \code{y} transform to be
linear (\code{ytype='l'}) may help the analyst choose the proper model
complexity.
}
\usage{
areg(x, y, xtype = NULL, ytype = NULL, nk = 4,
     B = 0, na.rm = TRUE, tolerance = NULL, crossval = NULL)

\method{print}{areg}(x, digits=4, \dots)

\method{plot}{areg}(x, whichx = 1:ncol(x$x), \dots)

\method{predict}{areg}(object, x, type=c('lp','fitted','x'),
                       what=c('all','sample'), \dots)
}
\arguments{
  \item{x}{
	A single predictor or a matrix of predictors.  Categorical
	predictors are required to be coded as integers (as \code{factor}
	does internally).
	For \code{predict}, \code{x} is a data matrix with the same integer
	codes that were originally used for categorical variables.
	}
  \item{y}{a \code{factor}, categorical, character, or numeric response
	variable}
  \item{xtype}{
	a vector of one-letter character codes specifying how each predictor
	is to be modeled, in order of columns of \code{x}.  The codes are
	\code{"s"} for smooth function (using restricted cubic splines),
	\code{"l"} for no transformation (linear), or \code{"c"} for
	categorical (to cause expansion into dummy variables).  Default is
	\code{"s"} if \code{nk > 0} and \code{"l"} if \code{nk=0}.
  }
  \item{ytype}{same coding as for \code{xtype}.  Default is \code{"s"}
	for a numeric variable with more than two unique values, \code{"l"}
	for a binary numeric variable, and \code{"c"} for a factor,
	categorical, or character variable.}
  \item{nk}{number of knots, 0 for linear, or 3 or more.  Default is 4
	which will fit 3 parameters to continuous variables (one linear term
  and two nonlinear terms)}
  \item{B}{number of bootstrap resamples used to estimate covariance
	matrices of transformation parameters.  Default is no bootstrapping.}
  \item{na.rm}{set to \code{FALSE} if you are sure that observations
	with \code{NA}s have already been removed}
  \item{tolerance}{singularity tolerance.  List source code for
	\code{lm.fit.qr.bare} for details.}
  \item{crossval}{set to a positive integer k to compute k-fold
	cross-validated R-squared (square of first canonical correlation)
	and mean and median absolute error of predictions on the original scale}
  \item{digits}{number of digits to use in formatting for printing}
  \item{object}{an object created by \code{areg}}
  \item{whichx}{integer or character vector specifying which predictors
	are to have their transformations plotted (default is all).  The
	\code{y} transformation is always plotted.}
  \item{type}{tells \code{predict} whether to obtain predicted
	untransformed \code{y} (\code{type='lp'}, the default) or predicted
	\code{y} on the original scale (\code{type='fitted'}), or the design
    matrix for the right-hand side (\code{type='x'}).}
  \item{what}{When the \code{y}-transform is non-monotonic you may
	specify \code{what='sample'} to \code{predict} to obtain a random
	sample of \code{y} values on the original scale instead of a matrix
	of all \code{y}-inverses.  See \code{\link{inverseFunction}}.}
  \item{\dots}{arguments passed to the plot function.}
}
\details{
\code{areg} is a competitor of \code{ace} in the \code{acepack}
package.  Transformations from \code{ace} are seldom smooth enough and
are often overfitted.  With \code{areg} the complexity can be controlled
with the \code{nk} parameter, and predicted values are easy to obtain
because parametric functions are fitted.

If one side of the equation has a categorical variable with more than
two categories and the other side has a continuous variable not assumed
to act linearly, larger sample sizes are needed to reliably estimate
transformations, as it is difficult to optimally score categorical
variables to maximize \eqn{R^2} against a simultaneously optimally
transformed continuous variable.
}
\value{
  a list of class \code{"areg"} containing many objects
}
\references{Breiman and Friedman, Journal of the American Statistical
     Association (September, 1985).} 
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{fh@fharrell.com}
}
\seealso{\code{\link{cancor}},\code{\link[acepack]{ace}}, \code{\link{transcan}}}
\examples{
set.seed(1)

ns <- c(30,300,3000)
for(n in ns) {
  y <- sample(1:5, n, TRUE)
  x <- abs(y-3) + runif(n)
  par(mfrow=c(3,4))
  for(k in c(0,3:5)) {
    z <- areg(x, y, ytype='c', nk=k)
    plot(x, z$tx)
	title(paste('R2=',format(z$rsquared)))
    tapply(z$ty, y, range)
    a <- tapply(x,y,mean)
    b <- tapply(z$ty,y,mean)
    plot(a,b)
	abline(lsfit(a,b))
    # Should get same result to within linear transformation if reverse x and y
    w <- areg(y, x, xtype='c', nk=k)
    plot(z$ty, w$tx)
    title(paste('R2=',format(w$rsquared)))
    abline(lsfit(z$ty, w$tx))
 }
}

par(mfrow=c(2,2))
# Example where one category in y differs from others but only in variance of x
n <- 50
y <- sample(1:5,n,TRUE)
x <- rnorm(n)
x[y==1] <- rnorm(sum(y==1), 0, 5)
z <- areg(x,y,xtype='l',ytype='c')
z
plot(z)
z <- areg(x,y,ytype='c')
z
plot(z)

\dontrun{		
# Examine overfitting when true transformations are linear
par(mfrow=c(4,3))
for(n in c(200,2000)) {
  x <- rnorm(n); y <- rnorm(n) + x
    for(nk in c(0,3,5)) {
    z <- areg(x, y, nk=nk, crossval=10, B=100)
    print(z)
    plot(z)
    title(paste('n=',n))
  }
}
par(mfrow=c(1,1))

# Underfitting when true transformation is quadratic but overfitting
# when y is allowed to be transformed
set.seed(49)
n <- 200
x <- rnorm(n); y <- rnorm(n) + .5*x^2
#areg(x, y, nk=0, crossval=10, B=100)
#areg(x, y, nk=4, ytype='l', crossval=10, B=100)
z <- areg(x, y, nk=4) #, crossval=10, B=100)
z
# Plot x vs. predicted value on original scale.  Since y-transform is
# not monotonic, there are multiple y-inverses
xx <- seq(-3.5,3.5,length=1000)
yhat <- predict(z, xx, type='fitted')
plot(x, y, xlim=c(-3.5,3.5))
for(j in 1:ncol(yhat)) lines(xx, yhat[,j], col=j)
# Plot a random sample of possible y inverses
yhats <- predict(z, xx, type='fitted', what='sample')
points(xx, yhats, pch=2)
}

# True transformation of x1 is quadratic, y is linear
n <- 200
x1 <- rnorm(n); x2 <- rnorm(n); y <- rnorm(n) + x1^2
z <- areg(cbind(x1,x2),y,xtype=c('s','l'),nk=3)
par(mfrow=c(2,2))
plot(z)

# y transformation is inverse quadratic but areg gets the same answer by
# making x1 quadratic
n <- 5000
x1 <- rnorm(n); x2 <- rnorm(n); y <- (x1 + rnorm(n))^2
z <- areg(cbind(x1,x2),y,nk=5)
par(mfrow=c(2,2))
plot(z)

# Overfit 20 predictors when no true relationships exist
n <- 1000
x <- matrix(runif(n*20),n,20)
y <- rnorm(n)
z <- areg(x, y, nk=5)  # add crossval=4 to expose the problem

# Test predict function
n <- 50
x <- rnorm(n)
y <- rnorm(n) + x
g <- sample(1:3, n, TRUE)
z <- areg(cbind(x,g),y,xtype=c('s','c'))
range(predict(z, cbind(x,g)) - z$linear.predictors)
}
\keyword{smooth}
\keyword{regression}
\keyword{multivariate}
\keyword{models}
\concept{bootstrap}
