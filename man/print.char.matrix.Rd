\name{print.char.matrix}
\alias{print.char.matrix}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Function to print a matrix with stacked cells }
\description{
   Prints a dataframe or matrix in stacked cells.  Line break charcters
   in a matrix element will result in a line break in that cell, but tab
   characters are not supported.
}
\usage{
\method{print}{char.matrix}(x, file = "", col.name.align = "cen", col.txt.align = "right", 
    cell.align = "cen", hsep = "|", vsep = "-", csep = "+", row.names = TRUE, 
    col.names = FALSE, append = FALSE,
    top.border = TRUE, left.border = TRUE, \dots)
}

\arguments{
  \item{x}{a matrix or dataframe}
  \item{file}{name of file if file output is desired.  If left empty,
    output will be to the screen}
  \item{col.name.align}{if column names are used, they can be aligned
    right, left or centre. Default \code{"cen"} results in names centred
    between the sides of the columns they name. If the width of the text
    in the columns is less than the width of the name, \code{col.name.align}
    will have no effect. Other options are \code{"right"} and \code{"left"}.}
  \item{col.txt.align}{how character columns are aligned.  Options
    are the same as for \code{col.name.align} with no effect when the width of
    the column is greater than its name.}
  \item{cell.align}{how numbers are displayed in columns}
  \item{hsep}{character string to use as horizontal separator,
    i.e. what separates columns}
  \item{vsep}{character string to use as vertical separator,
    i.e. what separates rows.  Length cannot be more than one.}

  \item{csep}{character string to use where vertical and horizontal
    separators cross.  If \code{hsep} is more than one character,
    \code{csep} will need to be the same length.  There is no provision
    for multiple vertical separators}

  \item{row.names}{logical: are we printing the names of the rows?}
  \item{col.names}{logical: are we printing the names of the columns?}
  \item{append}{logical: if \code{file} is not \code{""}, are we appending to
    the file or overwriting?}
  \item{top.border}{logical: do we want a border along the top above the
    columns?}
  \item{left.border}{logical: do we want a border along the left of the
    first column?}
  \item{\dots}{unused}
  }

\details{
  If any column of \code{x} is a mixture of character and numeric, the
  distinction between character and numeric columns will be lost. This
  is especially so if the matrix is of a form where you would not want
  to print the column names, the column information being in the rows at
  the beginning of the matrix.

  Row names, if not specified in the making of the matrix will simply be
  numbers. To prevent printing them, set \code{row.names = FALSE}.}

\value{
  No value is returned.  The matrix or dataframe will be printed to file
  or to the screen.
}
\author{Patrick Connolly \email{p.connolly@hortresearch.co.nz}}

\seealso{\code{write},  \code{write.table}}

\examples{
data(HairEyeColor)
print.char.matrix(HairEyeColor[ , , "Male"], col.names = TRUE)
print.char.matrix(HairEyeColor[ , , "Female"], col.txt.align = "left", col.names = TRUE)


z <- rbind(c("", "N", "y"),
           c("[ 1.34,40.3)\n[40.30,48.5)\n[48.49,58.4)\n[58.44,87.8]",
             " 50\n 50\n 50\n 50",
             "0.530\n0.489\n0.514\n0.507"),
           c("female\nmale", " 94\n106", "0.552\n0.473"  ),
           c("", "200", "0.510"))
dimnames(z) <- list(c("", "age", "sex", "Overall"),NULL)

print.char.matrix(z)
}
\keyword{print}
\keyword{array}
