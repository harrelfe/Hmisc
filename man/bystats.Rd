\name{bystats}
\alias{bystats}
\alias{print.bystats}
\alias{latex.bystats}
\alias{bystats2}
\alias{print.bystats2}
\alias{latex.bystats2}
\title{
Statistics by Categories
}
\description{

  For any number of cross-classification variables, \code{bystats}
  returns a matrix with the sample size, number missing \code{y}, and
  \code{fun(non-missing y)}, with the cross-classifications designated
  by rows. Uses Harrell's modification of the \code{interaction}
  function to produce cross-classifications.  The default \code{fun} is
  \code{mean}, and if \code{y} is binary, the mean is labeled as
  \code{Fraction}.  There is a \code{print} method as well as a
  \code{latex} method for objects created by \code{bystats}.
  \code{bystats2} handles the special case in which there are 2
  classifcation variables, and places the first one in rows and the
  second in columns.  The \code{print} method for \code{bystats2} uses
  the \code{print.char.matrix} function to organize statistics
  for cells into boxes. }

\usage{
bystats(y, \dots, fun, nmiss, subset)
\method{print}{bystats}(x, \dots)
\method{latex}{bystats}(object, title, caption, rowlabel, \dots)
bystats2(y, v, h, fun, nmiss, subset)
\method{print}{bystats2}(x, abbreviate.dimnames=FALSE,
   prefix.width=max(nchar(dimnames(x)[[1]])), \dots)
\method{latex}{bystats2}(object, title, caption, rowlabel, \dots)
}
\arguments{
\item{y}{
a binary, logical, or continuous variable or a matrix or data frame of
such variables.  If \code{y} is a data frame it is converted to a matrix.
If \code{y} is a data frame or matrix, computations are done on subsets of
the rows of \code{y}, and you should specify \code{fun} so as to be able to operate
on the matrix.  For matrix \code{y}, any column with a missing value causes
the entire row to be considered missing, and the row is not passed to
\code{fun}.
}
\item{...}{
For \code{bystats}, one or more classifcation variables separated by commas.
For \code{print.bystats}, options passed to \code{print.default} such as \code{digits}.
For \code{latex.bystats}, and \code{latex.bystats2},
options passed to \code{latex.default} such as \code{digits}.
If you pass \code{cdec} to \code{latex.default}, keep in mind that the first one or
two positions (depending on \code{nmiss}) should have zeros since these
correspond with frequency counts. 
}
\item{v}{
vertical variable for \code{bystats2}.  Will be converted to \code{factor}.
}
\item{h}{
horizontal variable for \code{bystats2}.  Will be converted to \code{factor}.
}
\item{fun}{
a function to compute on the non-missing \code{y} for a given subset.
You must specify \code{fun=} in front of the function name or definition.
\code{fun} may return a single number or a vector or matrix of any length.
Matrix results are rolled out into a vector, with names preserved.
When \code{y} is a matrix, a common \code{fun} is \code{function(y) apply(y, 2, ff)}
where \code{ff} is the name of a function which operates on one column of
\code{y}.
}
\item{nmiss}{
A column containing a count of missing values is included if \code{nmiss=TRUE}
or if there is at least one missing value.
}
\item{subset}{
a vector of subscripts or logical values indicating the subset of
data to analyze
}
\item{abbreviate.dimnames}{set to \code{TRUE} to abbreviate
  \code{dimnames} in output}
\item{prefix.width}{see \code{\link{print.char.matrix}}}
\item{title}{
\code{title} to pass to \code{latex.default}.  Default is the first word of
the character string version of the first calling argument.
}
\item{caption}{
caption to pass to \code{latex.default}.  Default is the \code{heading}
attribute from the object produced by \code{bystats}.
}
\item{rowlabel}{
\code{rowlabel} to pass to \code{latex.default}.  Default is the \code{byvarnames}
attribute from the object produced by \code{bystats}.  For \code{bystats2} the
default is \code{""}.
}
\item{x}{an object created by \code{bystats} or \code{bystats2}}
\item{object}{an object created by \code{bystats} or \code{bystats2}}
}
\value{
for \code{bystats}, a matrix with row names equal to the classification labels and column
names \code{N, Missing, funlab}, where \code{funlab} is determined from \code{fun}.
A row is added to the end with the summary statistics computed 
on all observations combined.  The class of this matrix is \code{bystats}.
For \code{bystats}, returns a 3-dimensional array with the last dimension
corresponding to statistics being computed.  The class of the array is
\code{bystats2}.
}
\section{Side Effects}{
\code{latex} produces a \code{.tex} file.
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
\seealso{
\code{\link{interaction}}, \code{\link{cut}}, \code{\link{cut2}}, \code{\link{latex}}, \code{\link{print.char.matrix}},
\code{\link{translate}}
}
\examples{
\dontrun{
bystats(sex==2, county, city)
bystats(death, race)
bystats(death, cut2(age,g=5), race)
bystats(cholesterol, cut2(age,g=4), sex, fun=median)
bystats(cholesterol, sex, fun=quantile)
bystats(cholesterol, sex, fun=function(x)c(Mean=mean(x),Median=median(x)))
latex(bystats(death,race,nmiss=FALSE,subset=sex=="female"), digits=2)
f <- function(y) c(Hazard=sum(y[,2])/sum(y[,1]))
# f() gets the hazard estimate for right-censored data from exponential dist.
bystats(cbind(d.time, death), race, sex, fun=f)
bystats(cbind(pressure, cholesterol), age.decile, 
        fun=function(y) c(Median.pressure   =median(y[,1]),
                          Median.cholesterol=median(y[,2])))
y <- cbind(pressure, cholesterol)
bystats(y, age.decile, 
        fun=function(y) apply(y, 2, median))   # same result as last one
bystats(y, age.decile, fun=function(y) apply(y, 2, quantile, c(.25,.75)))
# The last one computes separately the 0.25 and 0.75 quantiles of 2 vars.
latex(bystats2(death, race, sex, fun=table))
}
}
\keyword{category}
\concept{grouping}
