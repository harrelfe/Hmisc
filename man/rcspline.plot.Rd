\name{rcspline.plot}
\alias{rcspline.plot}
\title{
Plot Restricted Cubic Spline Function
}
\description{
Provides plots of the estimated restricted cubic spline function relating
a single predictor to the response for a logistic or Cox model.
The \code{rcspline.plot} function does not allow for interactions as do
\code{lrm} and \code{cph}, but it can provide detailed output for
checking spline fits. This function uses the \code{rcspline.eval},
\code{lrm.fit}, and Therneau's \code{coxph.fit} functions
and plots the estimated spline regression and confidence limits,
placing summary statistics on the graph. If there are no
adjustment variables, \code{rcspline.plot} can also plot two alternative
estimates of the regression function when \code{model="logistic"}: 
proportions or logit
proportions on grouped data, and a nonparametric estimate. The
nonparametric regression estimate is based on smoothing the binary
responses and taking the logit transformation of the smoothed
estimates, if desired. The smoothing uses \code{supsmu}.
}
\usage{
rcspline.plot(x,y,model=c("logistic", "cox", "ols"),xrange,event,nk=5,knots=NULL,
             show=c("xbeta","prob"),adj=NULL,xlab,ylab,ylim,plim=c(0,1),plotcl=TRUE,
             showknots=TRUE,add=FALSE,subset,lty=1,noprint=FALSE,m,smooth=FALSE,bass=1,
             main="auto",statloc)
}
\arguments{
\item{x}{
a numeric predictor
}
\item{y}{
a numeric response. For binary logistic regression, \code{y} should be \code{0-1}.
}
\item{model}{
\code{"logistic"} or \code{"cox"}. For \code{"cox"}, uses the \code{coxph.fit} with
\code{method="efron"}.
function.
}
\item{xrange}{
range for evaluating \code{x}, default is \code{f} and \code{1-f} quantiles of \code{x},
where \code{f=10/max(n,200)}
}
\item{event}{
event/censoring indicator if \code{model="cox"}. If \code{event} is
present, \code{model} is assumed to be \code{"cox"}
}
\item{nk}{
number of knots
}
\item{knots}{
knot locations, default based on quantiles of \code{x} (by
\code{rcspline.eval})
}
\item{show}{
\code{"xbeta"} or \code{"prob"} - what is plotted on \code{y}-axis
}
\item{adj}{
optional matrix of adjustment variables
}
\item{xlab}{
\code{x}-axis label, default is \code{"label"} attribute of \code{x}
}
\item{ylab}{same for \code{y}}
\item{ylim}{
\code{y}-axis limits for logit or log hazard
}
\item{plim}{
\code{y}-axis limits for probability scale
}
\item{plotcl}{
plot confidence limits
}
\item{showknots}{
show knot locations with arrows
}
\item{add}{
add this plot to an already existing plot
}
\item{subset}{
subset of observations to process, e.g. \code{subset=sex=="male"}
}
\item{lty}{
line type for plotting estimated spline function
}
\item{noprint}{
suppress printing regression coefficients and standard
errors
}
\item{m}{
for \code{model="logistic"}, plot grouped estimates with triangles. Each
group contains \code{m} ordered observations on \code{x}.
}
\item{smooth}{
plot nonparametric estimate if \code{model="logistic"} and \code{adj} is
not specified
}
\item{bass}{
smoothing parameter (see \code{supsmu})
}
\item{main}{
main title, default is e.g. \code{"Estimated Spline Transformation"}
}
\item{statloc}{
location of summary statistics. Default positioning by
clicking left mouse button where upper left corner of statistics
should appear. Alternative is \code{"ll"} to place below the graph on the
lower left, or the actual \code{x} and \code{y} coordinates.
Use \code{"none"} to suppress statistics.
}}
\value{
list with components \code{knots, x, xbeta, lower, upper} which are respectively
the knot locations, design matrix, linear predictor, and lower and upper
confidence limits
}
\author{
Frank Harrell
\cr
Department of Biostatistics, Vanderbilt University
\cr
f.harrell@vanderbilt.edu
}
\seealso{
\code{\link[Design]{lrm}}, \code{\link[Design]{cph}}, \code{\link{rcspline.eval}}, \code{\link{plot}}, \code{\link{supsmu}}, \code{\link[survival]{coxph.fit}}, \code{\link[Design]{lrm.fit}}
}
\examples{
#rcspline.plot(cad.dur, tvdlm, m=150)
#rcspline.plot(log10(cad.dur+1), tvdlm, m=150)
}
\keyword{regression}
\keyword{models}
% Converted by Sd2Rd version 1.21.
