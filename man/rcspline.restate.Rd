\name{rcspline.restate}
\alias{rcspline.restate}
\title{
Re-state Restricted Cubic Spline Function
}
\description{
This function re-states a restricted cubic spline function in
the un-linearly-restricted form. Coefficients for that form are
returned, along with an S functional representation of this function
and a LaTeX character representation of the function.
}
\usage{
rcspline.restate(knots, coef,
                 type=c("ordinary","integral"),
                 x="X", lx=nchar(x),
                 norm=2, columns=65, before="& &", after="\\",
                 begin="", nbegin=0, digits=max(8, .Options$digits))
}
\arguments{
\item{knots}{
vector of knots used in the regression fit
}
\item{coef}{
vector of coefficients from the fit. If the length of \code{coef} is
\code{k-1}, where \code{k=length(knots)}, the first coefficient must be
for the linear term and remaining \code{k-2} coefficients
must be for the constructed terms (e.g., from \code{rcspline.eval}).
If the length of \code{coef} is \code{k}, an intercept is assumed to be in
the first element.
}
\item{type}{
The default is to represent the cubic spline function corresponding
to the coefficients and knots.  Set \code{type="integral"} to instead represent
its anti-derivative.
}
\item{x}{
a character string to use as the variable name in the LaTeX expression
for the formula.
}
\item{lx}{
length of \code{x} to count with respect to \code{columns}. Default is length
of character string contained by \code{x}. You may want to set \code{lx}
smaller than this if it includes non-printable LaTeX commands.
}
\item{norm}{
normalization that was used in deriving the original nonlinear terms
used in the fit. See \code{rcspline.eval} for definitions.
}
\item{columns}{
maximum number of symbols in the LaTeX expression to allow before
inserting a newline (\\\\) command. Set to a very large number to
keep text all on one line.
}
\item{before}{
text to place before each line of LaTeX output. Use \code{"& &"} for an equation
array environment in LaTeX where you want to have a left-hand prefix
e.g. \code{f(X) & = &} or using \code{\\lefteqn}.
}
\item{after}{
text to place at the end of each line of output.
}
\item{begin}{
text with which to start the first line of output. Useful when adding
LaTeX output to part of an existing formula
}
\item{nbegin}{
number of columns of printable text in \code{begin}
}
\item{digits}{
number of significant digits to write for coefficients and knots
}}
\value{
a vector of coefficients. The coefficients are un-normalized
and two coefficients are added that are linearly dependent on the
other coefficients and knots. The vector of coefficients has four
attributes. \code{knots} is a vector of knots, \code{latex} is a vector of text strings
with the LaTeX representation of the formula.
\code{columns.used} is the number of columns used in the output string
since the last newline command.  \code{function} is an S function, which is
also return in character string format as the \code{text} attribute.
}
\author{
Frank Harrell
\cr
Department of Biostatistics, Vanderbilt University
\cr
f.harrell@vanderbilt.edu
}
\seealso{
\code{\link{rcspline.eval}}, \code{\link[splines]{ns}}, \code{\link[Design]{rcs}}, \code{\link{latex}}, \code{\link{Function.transcan}}
}
\examples{
set.seed(1)
x <- 1:100
y <- x + rnorm(100, 0, 5)
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
plot(x, xtrans(x), type="l")
# Plots fitted transformation


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
