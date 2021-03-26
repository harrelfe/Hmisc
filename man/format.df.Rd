\name{format.df}
\alias{format.df}
\title{
  Format a Data Frame or Matrix for LaTeX or HTML
}
\description{
  \code{format.df} does appropriate rounding and decimal alignment, and outputs
  a character matrix containing the formatted data.  If \code{x} is a
  \code{data.frame}, then do each component separately.
  If \code{x} is a matrix, but not a data.frame, make it a data.frame
  with individual components for the columns.
  If a component \code{x$x} is a matrix, then do all columns the same.
}

\usage{
format.df(x, digits, dec=NULL, rdec=NULL, cdec=NULL,
          numeric.dollar=!dcolumn, na.blank=FALSE, na.dot=FALSE,
          blank.dot=FALSE, col.just=NULL, cdot=FALSE,
          dcolumn=FALSE, matrix.sep=' ', scientific=c(-4,4),
          math.row.names=FALSE, already.math.row.names=FALSE,
          math.col.names=FALSE, already.math.col.names=FALSE,
          double.slash=FALSE, format.Date="\%m/\%d/\%Y",
          format.POSIXt="\%m/\%d/\%Y \%H:\%M:\%OS", \dots)
}
\arguments{
  \item{x}{
    a matrix (usually numeric) or data frame
  }
  \item{digits}{
    causes all values in the table to be formatted to \code{digits} significant
    digits.  \code{dec} is usually preferred.
  }
  \item{dec}{
    If \code{dec} is a scalar, all elements of the matrix will be rounded
    to \code{dec} 
    decimal places to the right of the decimal. \code{dec} can also be a matrix 
    whose elements correspond to \code{x}, for customized rounding of each element.
    A matrix \code{dec} must have number of columns equal to number of columns
    of input \code{x}.
    A scalar \code{dec} is expanded to a vector \code{cdec} with number of
    items equal to number of columns of input \code{x}.
  }
  \item{rdec}{
    a vector specifying the number of decimal places to the right for each row 
    (\code{cdec} is more commonly used than \code{rdec})
    A vector \code{rdec} must have number of items equal to number of rows of input \code{x}.
    \code{rdec} is expanded to matrix \code{dec}.
  }
  \item{cdec}{
    a vector specifying the number of decimal places for each column.
    The vector must have number of items equal to number of columns or components
    of input x.
  }
  \item{cdot}{
    Set to \code{TRUE} to use centered dots rather than ordinary periods in numbers.
    The output uses a syntax appropriate for \code{latex}.
  }
  \item{na.blank}{
    Set to \code{TRUE} to use blanks rather than \code{NA} for missing values.
    This usually looks better in \code{latex}.
  }
  \item{dcolumn}{
    Set to \code{TRUE} to use David Carlisle's dcolumn style for
    decimal alignment in \code{latex}.
    Default is \code{FALSE}. You will probably want to
    use \code{dcolumn} if you use \code{rdec}, as a column may then contain varying
    number of places to the right of the decimal. \code{dcolumn} can line up
    all such numbers on the decimal point, with integer values right
    justified at the decimal point location of numbers that actually
    contain decimal places.  When you use \code{dcolumn = TRUE}, 
    \code{numeric.dollar} is set by default to \code{FALSE}.  When you
    use \code{dcolumn = TRUE}, the
    object attribute \code{"style"} set to \samp{dcolumn} as the
		\code{latex} \code{usepackage} must reference \code{[dcolumn]}.
    The three files \file{dcolumn.sty}, \file{newarray.sty}, and
    \file{array.sty} will 
    need to be in a directory in your \env{TEXINPUTS} path.
    When you use \code{dcolumn=TRUE}, \code{numeric.dollar} should be set to \code{FALSE}.
  }
  \item{numeric.dollar}{
    logical, default \code{!dcolumn}.  Set to \code{TRUE} to place dollar
    signs around numeric values when \code{dcolumn = FALSE}.  This 
    assures that \code{latex} will use minus signs rather than hyphens to indicate
    negative numbers.  Set to \code{FALSE} when \code{dcolumn = TRUE}, as
    \code{dcolumn.sty} automatically uses minus signs.
  }
  \item{math.row.names}{
    logical, set true to place dollar signs around the row names.
  }
	\item{already.math.row.names}{set to \code{TRUE} to prevent any math
		mode changes to row names}
  \item{math.col.names}{
    logical, set true to place dollar signs around the column names.
  }
	\item{already.math.col.names}{set to \code{TRUE} to prevent any math
		mode changes to column names}
  \item{na.dot}{
    Set to \code{TRUE} to use periods rather than \code{NA} for missing
    numeric values. 
    This works with the \acronym{SAS} convention that periods indicate missing values.
  }
  \item{blank.dot}{
    Set to \code{TRUE} to use periods rather than blanks for missing character values.
    This works with the \acronym{SAS} convention that periods indicate missing values.
  }
  \item{col.just}{
    Input vector \code{col.just} must have number of columns equal to
    number of columns of the output matrix.  When \code{NULL}, the
    default, the \code{col.just} attribute of the result is set to
    \samp{l} for character columns and to \samp{r} for numeric
    columns.  The user can override the default by an argument vector
    whose length is equal to the number of columns of the result matrix.
    When \code{format.df} is called by \code{latex.default}, the
    \code{col.just} is used as the \code{cols} argument to the
    \code{tabular} environment and the letters \samp{l}, \samp{r},
    and \samp{c} are valid values.  When \code{format.df} is called by
    \acronym{SAS}, the \code{col.just} is used to determine whether a
    \samp{\$} is needed on the \samp{input} line of the \file{sysin} file,
    and the letters \samp{l} and \samp{r} are valid values.  You can
		pass specifications other than \code{l,r,c} in \code{col.just},
		e.g., \code{"p{3in}"} to get paragraph-formatted columns from
		\code{latex()}. 
  }
  \item{matrix.sep}{
    When \code{x} is a data frame containing a matrix, so that new column names
    are constructed from the name of the matrix object and the names of
    the individual columns of the matrix, \code{matrix.sep} specifies the
    character to use to separate object names from individual column
    names.
  }
  \item{scientific}{
    specifies ranges of exponents (or a logical vector) specifying values
    not to convert to scientific notation.  See \code{format.default} for details.
  }
  \item{double.slash}{
    should escaping backslashes be themselves escaped.
  }
  \item{format.Date}{
    String used to format objects of the Date class.
  }
  \item{format.POSIXt}{
    String used to format objects of the POSIXt class.
  }
  \item{\dots}{
    other arguments are accepted and passed to \code{format.default}.
    For \code{latexVerbatim} these arguments are passed to the
    \code{print} function.
  }
}
\value{
  a character matrix with character images of properly rounded \code{x}.
  Matrix components of input \code{x} are now just sets of columns of
  character matrix.
  Object attribute\code{"col.just"} repeats the value of the argument \code{col.just} when provided,
  otherwise, it includes the recommended justification for columns of output.
  See the discussion of the argument \code{col.just}.
  The default justification is \samp{l} for characters and factors,
  \samp{r} for numeric.
  When \code{dcolumn==TRUE}, numerics will have \samp{.} as the justification character.
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
\seealso{
  \code{\link{latex}}
}
\examples{
\dontrun{
x <- data.frame(a=1:2, b=3:4)
x$m <- 10000*matrix(5:8,nrow=2)
names(x)
dim(x)
x
format.df(x, big.mark=",")
dim(format.df(x))
}
}
\keyword{utilities}
\keyword{interface}
\keyword{methods}
\keyword{file}
\keyword{character}
\keyword{manip}
