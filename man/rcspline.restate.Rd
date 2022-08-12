\name{rcspline.restate}
\alias{rcspline.restate}
\alias{rcsplineFunction}
\title{
  Re-state Restricted Cubic Spline Function
}
\description{
  This function re-states a restricted cubic spline function in
  the un-linearly-restricted form. Coefficients for that form are
  returned, along with an \R functional representation of this function
  and a LaTeX character representation of the function.
  \code{rcsplineFunction} is a fast function that creates a function to
  compute a restricted cubic spline function with given coefficients and
  knots, without reformatting the function to be pretty (i.e., into
  unrestricted form).
}
\usage{
rcspline.restate(knots, coef,
                 type=c("ordinary","integral"),
                 x="X", lx=nchar(x),
                 norm=2, columns=65, before="& &", after="\\\\",
                 begin="", nbegin=0, digits=max(8, .Options$digits))

rcsplineFunction(knots, coef, norm=2, type=c('ordinary', 'integral'))
}
\arguments{
  \item{knots}{
    vector of knots used in the regression fit
  }
  \item{coef}{
    vector of coefficients from the fit. If the length of \code{coef} is
    \eqn{k-1}, where k is equal to the \code{length(knots)}, the
    first coefficient must be for the linear term and remaining
    \eqn{k-2} coefficients must be for the constructed terms (e.g., from
    \code{rcspline.eval}). If the length of \code{coef} is k, an
    intercept is assumed to be in the first element (or a zero is
    prepended to \code{coef} for \code{rcsplineFunction}).
  }
  \item{type}{
    The default is to represent the cubic spline function corresponding
    to the coefficients and knots.  Set \code{type = "integral"} to
    instead represent its anti-derivative.
  }
  \item{x}{
    a character string to use as the variable name in the LaTeX expression
    for the formula.
  }
  \item{lx}{
    length of \code{x} to count with respect to \code{columns}. Default
    is length of character string contained by \code{x}. You may want to
    set \code{lx} smaller than this if it includes non-printable LaTeX
    commands.
  }
  \item{norm}{
    normalization that was used in deriving the original nonlinear terms
    used in the fit. See \code{rcspline.eval} for definitions.
  }
  \item{columns}{
    maximum number of symbols in the LaTeX expression to allow before
    inserting a newline (\samp{\\\\}) command. Set to a very large
    number to keep text all on one line.
  }
  \item{before}{
    text to place before each line of LaTeX output. Use \samp{"& &"}
    for an equation array environment in LaTeX where you want to have a
    left-hand prefix e.g. \samp{"f(X) & = &"} or using
    \samp{"\\lefteqn"}.
  }
  \item{after}{
    text to place at the end of each line of output.
  }
  \item{begin}{
    text with which to start the first line of output. Useful when
    adding LaTeX output to part of an existing formula
  }
  \item{nbegin}{
    number of columns of printable text in \code{begin}
  }
  \item{digits}{
    number of significant digits to write for coefficients and knots
  }
}
\value{
  \code{rcspline.restate} returns a vector of coefficients. The
  coefficients are un-normalized and two coefficients are added that are
  linearly dependent on the other coefficients and knots. The vector of
  coefficients has four attributes. \code{knots} is a vector of knots,
  \code{latex} is a vector of text strings with the LaTeX
  representation of the formula. \code{columns.used} is the number of
  columns used in the output string since the last newline command.
  \code{function} is an \R function, which is also return in character
  string format as the \code{text} attribute.  \code{rcsplineFunction}
  returns an \R function with arguments \code{x} (a user-supplied
  numeric vector at which to evaluate the function), and some
  automatically-supplied other arguments. 
}
\author{
  Frank Harrell
  \cr
  Department of Biostatistics, Vanderbilt University
  \cr
  \email{fh@fharrell.com}
}
\seealso{
  \code{\link{rcspline.eval}}, \code{\link{ns}}, \code{\link[rms]{rcs}},
  \code{\link{latex}}, \code{\link{Function.transcan}} 
}
\examples{
set.seed(1)
x <- 1:100
y <- (x - 50)^2 + rnorm(100, 0, 50)
plot(x, y)
xx <- rcspline.eval(x, inclx=TRUE, nk=4)
knots <- attr(xx, "knots")
coef <- lsfit(xx, y)$coef
options(digits=4)
# rcspline.restate must ignore intercept
w <- rcspline.restate(knots, coef[-1], x="{\\\\rm BP}")
# could also have used coef instead of coef[-1], to include intercept
cat(attr(w,"latex"), sep="\n")


xtrans <- eval(attr(w, "function"))
# This is an S function of a single argument
lines(x, coef[1] + xtrans(x), type="l")
# Plots fitted transformation

xtrans <- rcsplineFunction(knots, coef)
xtrans
lines(x, xtrans(x), col='blue')


#x <- blood.pressure
xx.simple <- cbind(x, pmax(x-knots[1],0)^3, pmax(x-knots[2],0)^3,
                       pmax(x-knots[3],0)^3, pmax(x-knots[4],0)^3)
pred.value <- coef[1] + xx.simple \%*\% w
plot(x, pred.value, type='l')   # same as above
}
\keyword{regression}
\keyword{interface}
\keyword{character}
% Converted by Sd2Rd version 1.21.
