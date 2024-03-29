% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r2describe.r
\name{r2describe}
\alias{r2describe}
\title{r2describe}
\usage{
r2describe(x, nvmax = 10)
}
\arguments{
\item{x}{numeric matrix with 2 or more columns}

\item{nvmax}{maxmum number of columns of x to use in predicting a given column}
}
\value{
nothing
}
\description{
Summarize Strength of Relationships Using R-Squared From Linear Regression
}
\details{
Function to use \code{leaps::regsubsets()} to briefly describe which variables more strongly predict another variable.  Variables are in a numeric matrix and are assumed to be transformed so that relationships are linear (e.g., using \code{redun()} or \code{transcan()}.)
}
\examples{
\dontrun{
r <- redun(...)
r2describe(r$scores)
}
}
\author{
Frank Harrell
}
