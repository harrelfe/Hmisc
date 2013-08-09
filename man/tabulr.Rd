\name{tabulr}
\alias{tabulr}
\alias{table_trio}
\alias{table_pc}
\alias{table_latexdefs}
\alias{nFm}
\title{Interface to Tabular Function}
\description{
\code{\link{tabulr}} is a front-end to the \code{tables} package's
\code{\link[tables]{tabular}} function so that the user can take
advantage of variable 
annotations used by the \code{Hmisc} package, particular those created
by the \code{\link{label}}, \code{\link{units}}, and
\code{\link{upData}} functions.  When \code{label(x)} or
\code{labeln(x)} appear in a \code{\link[tables]{tabular}} function, the
variable \code{x} is found in the \code{data} argument or in the parent
environment, and the \code{\link{labelLatex}} function is used to create
a LaTeX label.  By default any units of measurement are right justified
in the current LaTeX tabular field using \code{hfill}; use \code{labeln}
to not use the horizontal fill after the \code{label}.  Once the label
is constructed, \code{label(x)} or \code{labeln(x)} are replaced with
\code{Heading("LaTeX label")} in the formula before it is passed to
\code{\link[tables]{tabular}}.  \code{tabulr} also replaces \code{trio}
with \code{table_trio} in the formula.

\code{table_trio} is a function that takes a numeric vector and computes
the three quartiles and optionally the mean and standard deviation, and
outputs a LaTeX-formatted character string representing the results.  By
default, calculated statistics are formatted with 3 digits to the left
and 1 digit to the right of the decimal point.  Running
\code{\link[tables]{table_options}(left=l, right=r)} will use \code{l}
and \code{r} digits instead.  Other options that can be given to
\code{table_options} are \code{prmsd=TRUE} to add mean +/- standard
deviation to the result, \code{pn=TRUE} to add the sample size, and
\code{bold=TRUE} to set the median in bold face.

\code{nFm} is a function that calls \code{\link{sprintf}} to format
numeric values to have a specific number of digits to the \code{left}
and to the \code{right} of the point.

\code{table_latexdefs} writes (by default) to the console a set of LaTeX
definitions that can be invoked at any point thereafter in a \code{knitr} or
\code{sweave} document by naming the macro, preceeded by a single
slash.  The \code{blfootnote} macro is called with a single LaTeX
argument which will appear as a footnote without a number.
\code{keytrio} invokes \code{blfootnote} to define the output of
\code{table_trio} if mean and SD are not included.  If mean and SD are
included, use \code{keytriomsd}.
}
\usage{
tabulr(formula, data = NULL, \dots)
table_trio(x)
nfm(x, left, right, neg=FALSE, pad=FALSE)
table_latexdefs(file='')
}
\arguments{
  \item{formula}{a formula suitable for \code{\link[tables]{tabular}}
		except for the addition of \code{label(variable name)},
		\code{labeln()}, \code{trio}.}
  \item{data}{a data frame or list.  If omitted, the parent environment
		is assumed to contain the variables.}
  \item{\dots}{other arguments to \code{tabular}}
  \item{x}{a numeric vector}
	\item{left, right}{number of places to the left and right of the
		decimal point, respectively}
	\item{pad}{set to \code{TRUE} to replace blanks with the LaTeX tilde
		placeholder}
	\item{file}{location of output of \code{table_latexdefs}}
}
\value{\code{tabulr} returns an object of class \code{"tabular"}}
\author{Frank Harrell}
\seealso{\code{\link[tables]{tabular}}, \code{\link{label}},
	\code{\link{latex}}}
\examples{
\dontrun{
n <- 400
set.seed(1)
d <- data.frame(country=factor(sample(c('US','Canada','Mexico'), n, TRUE)),
                sex=factor(sample(c('Female','Male'), n, TRUE)),
                age=rnorm(n, 50, 10),
                sbp=rnorm(n, 120, 8))
d <- upData(d,
            preghx=ifelse(sex=='Female', sample(c('No','Yes'), n, TRUE), NA),
            labels=c(sbp='Systolic BP', age='Age', preghx='Pregnancy History'),
            units=c(sbp='mmHg', age='years'))
contents(d)
require(tables)
invisible(booktabs())  # use booktabs LaTeX style for tabular
g <- function(x) {
  x <- x[!is.na(x)]
  if(length(x) == 0) return('')
  paste(latexNumeric(nFm(mean(x), 3, 1)),
        ' \\hfill{\\smaller[2](', length(x), ')}', sep='')
}
tab <- tabulr((label(age)*age +
               Heading('Females')*(sex == 'Female')*
               labeln(sbp)*sbp)*
              Heading()*g + (age + sbp)*Heading()*trio ~ 
              Heading()*country*Heading()*sex, data=d)
# Formula after interpretation by tabulr:
# (Heading('Age\\hfill {\\smaller[2] years}') * age + Heading("Females")
# * (sex == "Female") * Heading('Systolic BP {\\smaller[2] mmHg}') * sbp)
# * Heading() * g + (age + sbp) * Heading() * table_trio ~ Heading()
# * country * Heading() * sex
cat('\\begin{landscape}\n')
cat('\\begin{minipage}{\\textwidth}\n')
cat('\\keytrio\n')
latex(tab)
cat('\\end{minipage}\\end{landscape}\n')
}
}
\keyword{utilities}
\keyword{interface}
