\name{summaryP}
\alias{summaryP}
\title{Multi-way Graphical Summary of Proportions}
\description{
\code{summaryP} computes proportions stratified by a series of
variables, and displays them as a multi-panel dot chart using the
\code{lattice} package's \code{dotplot} function with a special
\code{panel} function.  Numerators and denominators of proportions are
also included as text, in the same colors as used by an optional \code{groups}
variable.  A special capability to group a series of related yes/no
variables is included through the internal \code{yn} function, for which
the user specials a final argument \code{label} used to label the panel
created for that group of related variables.  For \code{yn}, a positive
response is taken to be \code{y, yes, present} (ignoring case) or a
\code{logical} \code{TRUE} value.  The \code{formula} argument used in
the \code{dotplot} call is constructed, but the user can easily reorder
the variables by specifying \code{gformula}, with elements named
\code{val} (category levels), \code{var} (classification variable name),
\code{freq} (calculated result) plus the overall cross-classification
variables).
}
\usage{
summaryP(formula, groups = NULL, data = NULL, subset = NULL,
         na.action = na.retain, xlim = c(0, 1), col = 1:2, pch = 1:2,
         cex.values = 0.5, ydelta = 0.04,
         key = list(columns = length(levels(groups)), x = 0.75,
                    y = -0.05, cex = 0.9, col = col), sort = TRUE,
         asna = c("unknown", "unspecified"), gformula = NULL)
}
\arguments{
  \item{formula}{a formula with the variables for whose levels
		proportions are computed on the left hand side, and major
		classification variables on the right.
}
  \item{groups}{an optional superpositioning variable for obtaining
		further stratification within a horizontal line in the dot chart}
  \item{data}{an optional data frame}
  \item{subset}{an optional subsetting expression or vector}
  \item{na.action}{function specifying how to handle \code{NA}s.  The
		default is to keep all \code{NA}s in the analysis frame.}
  \item{xlim}{\code{x}-axis limits.  Default is \code{c(0,1)}.}
  \item{col}{vector of color codes or names corresponding to levels of \code{groups}}
  \item{pch}{symbol codes corresponding to \code{groups}}
  \item{cex.values}{character size to use for plotting numerators and denominators}
  \item{ydelta}{fraction of a panel height to use for positioning
		numerators and denominators below the horizontal line for a group}
  \item{key}{a list to pass to the \code{auto.key} argument of
		\code{dotplot}.  To place a key above the entire chart use
		\code{auto.key=list(columns=2)} for example.}
  \item{sort}{set to \code{FALSE} to not sort category levels in
		descending order of global proportions}
  \item{asna}{character vector specifying level names to consider the
		same as \code{NA}.  Set \code{asna=NULL} to not consider any.}
  \item{gformula}{a substitute formula for passing to \code{dotplot}}
}
\value{a \code{lattice} object of class \code{"trellis"}}
\author{Frank Harrell
  \cr
  Department of Biostatistics
  \cr
  Vanderbilt University
  \cr
  \email{f.harrell@vanderbilt.edu}}
\seealso{\code{\link{bpplotM}}, \code{\link{summaryM}}}
\examples{
n <- 100
f <- function(na=FALSE) {
  x <- sample(c('N', 'Y'), n, TRUE)
  if(na) x[runif(100) < .1] <- NA
  x
}
set.seed(1)
d <- data.frame(x1=f(), x2=f(), x3=f(), x4=f(), x5=f(), x6=f(), x7=f(TRUE),
                age=rnorm(n, 50, 10),
                race=sample(c('Asian', 'Black/AA', 'White'), n, TRUE),
                sex=sample(c('Female', 'Male'), n, TRUE),
                treat=sample(c('A', 'B'), n, TRUE),
                region=sample(c('North America','Europe'), n, TRUE))
d <- upData(d, labels=c(x1='MI', x2='Stroke', x3='AKI', x4='Migraines',
                 x5='Pregnant', x6='Other event', x7='MD withdrawal',
                 race='Race', sex='Sex'))
dasna <- subset(d, region=='North America')
with(dasna, table(race, treat))
z=summaryP(race + sex + yn(x1, x2, x3, x4, x5, x6, x7, label='Exclusions') ~
         region, groups=treat, data=d, col=c('black', 'blue'),
  gformula=val ~ freq | region * var)
z   # default lattice output
\dontrun{
require(latticeExtra)
useOuterStrips(z)   # looks much better

summaryP(race + sex ~ region, data=d, col='green')
summaryP(race + sex ~ region, data=d, gformula=val ~ freq | region * var)
summaryP(race + sex ~ region, groups = treat, data=d)
}
}
\keyword{hplot}
\keyword{category}
\keyword{manip}
\concept{grouping}
\concept{stratification}
\concept{aggregation}
