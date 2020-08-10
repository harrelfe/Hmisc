\name{ciapower}
\alias{ciapower}
\title{
Power of Interaction Test for Exponential Survival
}
\description{
Uses the method of Peterson and George to compute the power of an
interaction test in a 2 x 2 setup in which all 4 distributions are
exponential.  This will be the same as the power of the Cox model
test if assumptions hold.  The test is 2-tailed.  
The duration of accrual is specified
(constant accrual is assumed), as is the minimum follow-up time.
The maximum follow-up time is then \code{accrual + tmin}.  Treatment
allocation is assumed to be 1:1.
}
\usage{
ciapower(tref, n1, n2, m1c, m2c, r1, r2, accrual, tmin, 
         alpha=0.05, pr=TRUE)
}
\arguments{
\item{tref}{
time at which mortalities estimated
}
\item{n1}{
total sample size, stratum 1
}
\item{n2}{
total sample size, stratum 2
}
\item{m1c}{
tref-year mortality, stratum 1 control
}
\item{m2c}{
tref-year mortality, stratum 2 control
}
\item{r1}{
\% reduction in \code{m1c} by intervention, stratum 1
}
\item{r2}{
\% reduction in \code{m2c} by intervention, stratum 2
}
\item{accrual}{
duration of accrual period
}
\item{tmin}{
minimum follow-up time
}
\item{alpha}{
type I error probability
}
\item{pr}{
set to \code{FALSE} to suppress printing of details
}}
\value{
power
}
\section{Side Effects}{
prints
}
\section{AUTHOR}{
Frank Harrell


Department of Biostatistics


Vanderbilt University


\email{fh@fharrell.com}
}
\references{
Peterson B, George SL: Controlled Clinical Trials 14:511--522; 1993.
}
\seealso{
\code{\link{cpower}}, \code{\link{spower}}
}
\examples{
# Find the power of a race x treatment test.  25\% of patients will
# be non-white and the total sample size is 14000.  
# Accrual is for 1.5 years and minimum follow-up is 5y.
# Reduction in 5-year mortality is 15\% for whites, 0\% or -5\% for
# non-whites.  5-year mortality for control subjects if assumed to
# be 0.18 for whites, 0.23 for non-whites.
n <- 14000
for(nonwhite.reduction in c(0,-5)) {
  cat("\n\n\n\% Reduction in 5-year mortality for non-whites:",
      nonwhite.reduction, "\n\n")
  pow <- ciapower(5,  .75*n, .25*n,  .18, .23,  15, nonwhite.reduction,  
                  1.5, 5)
  cat("\n\nPower:",format(pow),"\n")
}
}
\keyword{survival}
\keyword{htest}
\concept{power}
\concept{study design}
