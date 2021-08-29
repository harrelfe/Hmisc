\name{first.word}
\alias{first.word}
\title{First Word in a String or Expression}
\description{
\code{first.word} finds the first word in an expression.  A word is defined by
unlisting the elements of the expression found by the S parser and then
accepting any elements whose first character is either a letter or period.
The principal intended use is for the automatic generation of temporary
file names where it is important to exclude special characters from
the file name. For Microsoft Windows, periods in names are deleted and
only up to the first 8 characters of the word is returned.
}
\usage{
first.word(x, i=1, expr=substitute(x))
}
\arguments{
\item{x}{
any scalar character string
}
\item{i}{
word number, default value = 1.  Used when the second or \code{i}th word is
wanted.  Currently only the \code{i=1} case is implemented.
}
\item{expr}{
any S object of mode \code{expression}.
}
}
\value{
a character string
}
\author{
Frank E. Harrell, Jr.,
\cr
Department of Biostatistics,
\cr
Vanderbilt University,
\cr
\email{fh@fharrell.com}


Richard M. Heiberger,
\cr
Department of Statistics,
\cr
Temple University, Philadelphia, PA.
\cr
\email{rmh@temple.edu}
}
\examples{
first.word(expr=expression(y ~ x + log(w)))
}
\keyword{character}
\keyword{manip}
