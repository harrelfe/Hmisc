\name{format.df}
\alias{format.df}
\title{
  Format a Data Frame or Matrix for LaTeX or HTML
}
\description{
  \code{format.df} does appropriate rounding and decimal alignment, and outputs
  a character matrix containing the formatted data.  If \code{x} is a
  data.frame, then do each component separately.
  If \code{x} is a matrix, but not a data.frame, make it a data.frame
  with individual components for the columns.
  If a component \code{x$x} is a matrix, then do all columns the same.
}

\usage{
format.df(x,
          digits, dec=NULL, rdec=NULL, cdec=NULL,
          numeric.dollar=cdot,
          na.blank=FALSE, na.dot=FALSE, blank.dot=FALSE,
          col.just=NULL, cdot=FALSE, dcolumn=FALSE, matrix.sep=' ', 
          scientific=c(-4,4), math.row.names, math.col.names, \dots)
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
    Set to \code{TRUE} to use David Carlisle's \code{dcolumn} style for
    decimal alignment in \code{latex}.
    Default is \code{FALSE}. You will probably want to
    use \code{dcolumn} if you use \code{rdec}, as a column may then contain varying
    number of places to the right of the decimal. \code{dcolumn} can line up
    all such numbers on the decimal point, with integer values right
    justified at the decimal point location of numbers that actually
    contain decimal places.  When you use \code{dcolumn=TRUE}, 
    \code{numeric.dollar} is set by default to \code{FALSE}.  When you use \code{dcolumn=TRUE}, the
    \code{"style"} element is set to \code{"dcolumn"} as the \code{latex} \code{\\usepackage}
    must reference \code{[dcolumn]}.
    The three files \code{dcolumn.sty}, \code{newarray.sty}, and
    \code{array.sty} will 
    need to be in a directory in your \code{$TEXINPUTS} path.
    When you use \code{dcolumn=TRUE}, \code{numeric.dollar} should be set to \code{FALSE}.
  }
  \item{numeric.dollar}{
    logical, default \code{!dcolumn}.  Set to \code{TRUE} to place dollar
    signs around numeric values when \code{dcolumn=FALSE}.  This 
    assures that \code{latex} will use minus signs rather than hyphens to indicate
    negative numbers.  Set to \code{FALSE} when \code{dcolumn=TRUE}, as
    \code{dcolumn.sty} automatically uses minus signs.
  }
  \item{math.row.names}{
    logical, set true to place dollar signs around the row names.
  }
  \item{math.col.names}{
    logical, set true to place dollar signs around the column names.
  }
  \item{na.dot}{
    Set to \code{TRUE} to use periods rather than \code{NA} for missing
    numeric values. 
    This works with the \code{sas} convention that periods indicate missing values.
  }
  \item{blank.dot}{
    Set to \code{TRUE} to use periods rather than blanks for missing character values.
    This works with the \code{sas} convention that periods indicate missing values.
  }
  \item{col.just}{
    Input vector \code{col.just} must have number of columns equal to
    number of columns of the output matrix.  When \code{NULL}, the
    default, the \code{col.just} attribute of the result is set to
    \code{"l"} for character columns and to \code{"r"} for numeric
    columns.  The user can override the default by an argument vector
    whose length is equal to the number of columns of the result matrix.
    When \code{format.df} is called by \code{latex.default}, the
    \code{col.just} is used as the \code{cols} argument to the
    \code{\tabular} environment and the letters \code{"l"}, \code{"r"},
    and \code{"c"} are valid values.  When \code{format.df} is called by
    \code{sas}, the \code{col.just} is used to determine whether a
    \code{$} is needed on the \code{input} line of the \code{sysin} file,
    and the letters \code{"l"} and \code{"r"} are valid values.  }
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
  \item{...}{
    other arguments are accepted and ignored.  For \code{latexVerbatim} these
    arguments are passed to the \code{print} function.
  }
}
\value{
  a character matrix with character images of properly rounded \code{x}.
  Matrix components of input \code{x} are now just sets of columns of
  character matrix.
  \code{attr(,col.just)} repeats the input \code{col.just} when provided,
  otherwise, it includes the recommended justification for columns of output.
  See the discussion of the argument \code{col.just}.
  The default justification is \code{"l"} for characters and factors,
  \code{"r"} for numeric.
  When \code{dcolumn==TRUE}, numerics will have \code{"."} as the justification character.
}

\author{
  Frank E. Harrell, Jr.,
  \cr
  Department of Biostatistics,
  \cr
  Vanderbilt University,
  \cr
  \code{f.harrell@vanderbilt.edu}


  Richard M. Heiberger,
  \cr
  Department of Statistics,
  \cr
  Temple University, Philadelphia, PA.
  \cr
  \code{rmh@astro.ocis.temple.edu}


}
\seealso{
  \code{\link{latex}}
}
\examples{
x <- data.frame(a=1:2, b=3:4)
x$m <- matrix(5:8,nrow=2)
names(x)
dim(x)
x
format.df(x)
dim(format.df(x))
}
\keyword{utilities}
\keyword{interface}
\keyword{methods}
\keyword{file}
\keyword{character}
\keyword{manip}
