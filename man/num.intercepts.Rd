\name{num.intercepts}
\alias{num.intercepts}
\title{Extract number of intercepts}
\description{
  Extract the number of intercepts from a model
}
\usage{
num.intercepts(fit, type=c('fit', 'var', 'coef'))
}
\arguments{
  \item{fit}{a model fit object}
  
  \item{type}{
    the default is to return the formal number of intercepts used when fitting
    the model.  Set \code{type='var'} to return the actual number of
    intercepts stored in the \code{var} object, or \code{type='coef'} to
    return the actual number in the fitted coefficients.  The former will be
    less than the number fitted for \code{\link[rms]{orm}} fits, and the
    latter for \code{orm} fits passed through
    \code{\link{fit.mult.impute}}
  }
}
\value{
  \code{num.intercepts} returns an integer with the number of intercepts
  in the model.
}
\seealso{
  \code{\link[rms]{orm}}, \code{\link{fit.mult.impute}}
}
