\name{prselect}
\Rdversion{1.1}
\alias{prselect}
\title{Selectively Print Lines of a Text Vector}
\description{
  Given one or two regular expressions or exact text matches, removes
  elements of the input vector that match these specifications. Omitted
  lines are replaced by \ldots.  This is useful for selectively
  suppressing some of the printed output of R functions such as
  regression fitting functions, especially in the context of making
  statistical reports using Sweave or Odfweave.
}
\usage{
prselect(x, start = NULL, stop = NULL, i = 0, j = 0, pr = TRUE)
}
\arguments{
  \item{x}{
    input character vector
  }
  \item{start}{
    text or regular expression to look for starting line to omit.  If
    omitted, deletions start at the first line.
  }
  \item{stop}{
    text or regular expression to look for ending line to omit. If
    omitted, deletions proceed until the last line.
  }
  \item{i}{
    increment in number of first line to delete after match is found
  }
  \item{j}{
    increment in number of last line to delete after match is found
  }
  \item{pr}{
    set to \code{FALSE} to suppress printing
  }
}
\value{an invisible vector of retained lines of text}
\author{Frank Harrell}
\seealso{
  \code{\link[utils]{Sweave}}
}
\examples{
x <- c('the','cat','ran','past','the','dog')
prselect(x, 'big','bad')     # omit nothing- no match
prselect(x, 'the','past')    # omit first 4 lines
prselect(x,'the','junk')     # omit nothing- no match for stop
prselect(x,'ran','dog')      # omit last 4 lines
prselect(x,'cat')            # omit lines 2-
prselect(x,'cat',i=1)        # omit lines 3-
prselect(x,'cat','past')     # omit lines 2-4
prselect(x,'cat','past',j=1) # omit lines 2-5
prselect(x,'cat','past',j=-1)# omit lines 2-3
prselect(x,'t$','dog')       # omit lines 2-6; t must be at end

# Example for Sweave: run a regression analysis with the rms package
# then selectively output only a portion of what print.ols prints.
# (Thanks to \email{romain.francois@dbmail.com})
# <<z,eval=FALSE,echo=T>>=
# library(rms)
# y <- rnorm(20); x1 <- rnorm(20); x2 <- rnorm(20)
# ols(y ~ x1 + x2)
# <<echo=F>>=
# z <- capture.output( {
# <<z>>
#    } )
# prselect(z, 'Residuals:') # keep only summary stats; or:
# prselect(z, stop='Coefficients', j=-1)  # keep coefficients, rmse, R^2; or:
# prselect(z, 'Coefficients', 'Residual standard error', j=-1) # omit coef
# @
}
\keyword{manip}
\keyword{utilities}
