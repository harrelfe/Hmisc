\name{reShape}
\alias{reShape}
\title{Reshape Matrices and Serial Data}
\description{
  If the first argument is a matrix, \code{reShape} strings out its values
  and creates row and column vectors specifying the row and column each
  element came from.  This is useful for sending matrices to Trellis
  functions, for analyzing or plotting results of \code{table} or
  \code{crosstabs}, or for reformatting serial data stored in a matrix (with
  rows representing multiple time points) into vectors.  The number of
  observations in the new variables will be the product of the number of
  rows and number of columns in the input matrix.  If the first
  argument is a vector, the \code{id} and \code{colvar} variables are used to
  restructure it into a matrix, with \code{NA}s for elements that corresponded
  to combinations of \code{id} and \code{colvar} values that did not exist in the
  data.  When more than one vector is given, multiple matrices are
  created.  This is useful for restructuring irregular serial data into
  regular matrices.  It is also useful for converting data produced by
  \code{expand.grid} into a matrix (see the last example).  The number of
  rows of the new matrices equals the number of unique values of \code{id},
  and the number of columns equals the number of unique values of
  \code{colvar}.

  When the first argument is a vector and the \code{id} is a data frame
  (even with only one variable),
  \code{reShape} will produce a data frame, and the unique groups are
  identified by combinations of the values of all variables in \code{id}.
  If a data frame \code{constant} is specified, the variables in this data
  frame are assumed to be constant within combinations of \code{id}
  variables (if not, an arbitrary observation in \code{constant} will be
  selected for each group).  A row of \code{constant} corresponding to the
  target \code{id} combination is then carried along when creating the
  data frame result.

  A different behavior of \code{reShape} is achieved when \code{base} and \code{reps}
  are specified.  In that case \code{x} must be a list or data frame, and
  those data are assumed to contain one or more non-repeating
  measurements (e.g., baseline measurements) and one or more repeated
  measurements represented by variables named by pasting together the
  character strings in the vector \code{base} with the integers 1, 2, \dots,
  \code{reps}.  The input data are rearranged by repeating each value of the
  baseline variables \code{reps} times and by transposing each observation's
  values of one of the set of repeated measurements as \code{reps}
  observations under the variable whose name does not have an integer
  pasted to the end.  if \code{x} has a \code{row.names} attribute, those
  observation identifiers are each repeated \code{reps} times in the output
  object.  See the last example.
}
\usage{
reShape(x, \dots, id, colvar, base, reps, times=1:reps,
        timevar='seqno', constant=NULL)
}
\arguments{
  \item{x}{
    a matrix or vector, or, when \code{base} is specified, a list or data frame
  }
  \item{\dots}{
    other optional vectors, if \code{x} is a vector
  }
  \item{id}{
    A numeric, character, category, or factor variable containing subject
    identifiers, or a data frame of such variables that in combination form
    groups of interest.  Required if \code{x} is a vector, ignored otherwise.
  }
  \item{colvar}{
    A numeric, character, category, or factor variable containing column
    identifiers.  \code{colvar} is using a "time of data collection" variable.
    Required if \code{x} is a vector, ignored otherwise.
  }
  \item{base}{
    vector of character strings containing base names of repeated
    measurements
  }
  \item{reps}{
    number of times variables named in \code{base} are repeated.  This must be
    a constant.
  }
  \item{times}{
    when \code{base} is given, \code{times} is the vector of times to create
    if you do not want to use consecutive integers beginning with 1.
  }
  \item{timevar}{
    specifies the name of the time variable to create if \code{times} is
    given, if you do not want to use \code{seqno}
  }
  \item{constant}{
    a data frame with the same number of rows in \code{id} and \code{x},
    containing auxiliary information to be merged into the resulting data
    frame.  Logically, the rows of \code{constant} within each group
    should have the same value of all of its variables.
  }
}
\value{
  If \code{x} is a matrix, returns a list containing the row variable, the
  column variable, and the \code{as.vector(x)} vector, named the same as the
  calling argument was called for \code{x}.  If \code{x} is a vector and no other
  vectors were specified as \code{\dots}, the result is a matrix.  If at least
  one vector was given to \code{\dots}, the result is a list containing \code{k}
  matrices, where \code{k} one plus the number of vectors in \code{\dots}.  If \code{x}
  is a list or data frame, the same type of object is returned.  If
  \code{x} is a vector and \code{id} is a data frame, a data frame will be
  the result.
}
\details{
  In converting \code{dimnames} to vectors, the resulting variables are
  numeric if all elements of the matrix dimnames can be converted to
  numeric, otherwise the corresponding row or column variable remains
  character.  When the \code{dimnames} if \code{x} have a \code{names} attribute, those
  two names become the new variable names.  If \code{x} is a vector and
  another vector is also given (in \code{\dots}), the matrices in the resulting
  list are named the same as the input vector calling arguments.  You
  can specify customized names for these on-the-fly by using
  e.g. \code{reShape(X=x, Y=y, id= , colvar= )}.  The new names will then be
  \code{X} and \code{Y} instead of \code{x} and \code{y}.   A new variable named \code{seqnno} is
  also added to the resulting object.  \code{seqno} indicates the sequential
  repeated measurement number.  When \code{base} and \code{times} are
  specified, this new variable is named the character value of \code{timevar} and the values
  are given by a table lookup into the vector \code{times}.
}
\author{
Frank Harrell\cr
Department of Biostatistics\cr
Vanderbilt University School of Medicine\cr
\email{fh@fharrell.com}
}
\seealso{
  \code{\link[stats]{reshape}}, \code{\link[base:vector]{as.vector}},
  \code{\link[base]{matrix}}, \code{\link[base]{dimnames}},
  \code{\link[base]{outer}}, \code{\link[base]{table}}
}
\examples{
set.seed(1)
Solder  <- factor(sample(c('Thin','Thick'),200,TRUE),c('Thin','Thick'))
Opening <- factor(sample(c('S','M','L'),  200,TRUE),c('S','M','L'))

tab <- table(Opening, Solder)
tab
reShape(tab)
# attach(tab)  # do further processing

# An example where a matrix is created from irregular vectors
follow <- data.frame(id=c('a','a','b','b','b','d'),
                     month=c(1, 2,  1,  2,  3,  2),
                     cholesterol=c(225,226, 320,319,318, 270))
follow
attach(follow)
reShape(cholesterol, id=id, colvar=month)
detach('follow')
# Could have done :
# reShape(cholesterol, triglyceride=trig, id=id, colvar=month)

# Create a data frame, reshaping a long dataset in which groups are
# formed not just by subject id but by combinations of subject id and
# visit number.  Also carry forward a variable that is supposed to be
# constant within subject-visit number combinations.  In this example,
# it is not constant, so an arbitrary visit number will be selected.
w <- data.frame(id=c('a','a','a','a','b','b','b','d','d','d'),
             visit=c(  1,  1,  2,  2,  1,  1,  2,  2,  2,  2),
                 k=c('A','A','B','B','C','C','D','E','F','G'),
               var=c('x','y','x','y','x','y','y','x','y','z'),
               val=1:10)
with(w,
     reShape(val, id=data.frame(id,visit),
             constant=data.frame(k), colvar=var))

# Get predictions from a regression model for 2 systematically
# varying predictors.  Convert the predictions into a matrix, with
# rows corresponding to the predictor having the most values, and
# columns corresponding to the other predictor
# d <- expand.grid(x2=0:1, x1=1:100)
# pred <- predict(fit, d)
# reShape(pred, id=d$x1, colvar=d$x2)  # makes 100 x 2 matrix


# Reshape a wide data frame containing multiple variables representing
# repeated measurements (3 repeats on 2 variables; 4 subjects)
set.seed(33)
n <- 4
w <- data.frame(age=rnorm(n, 40, 10),
                sex=sample(c('female','male'), n,TRUE),
                sbp1=rnorm(n, 120, 15),
                sbp2=rnorm(n, 120, 15),
                sbp3=rnorm(n, 120, 15),
                dbp1=rnorm(n,  80, 15),
                dbp2=rnorm(n,  80, 15),
                dbp3=rnorm(n,  80, 15), row.names=letters[1:n])
options(digits=3)
w


u <- reShape(w, base=c('sbp','dbp'), reps=3)
u
reShape(w, base=c('sbp','dbp'), reps=3, timevar='week', times=c(0,3,12))
}
\keyword{manip}
\keyword{array}
\concept{trellis}
\concept{lattice}
\concept{repeated measures}
\concept{longitudinal data}
