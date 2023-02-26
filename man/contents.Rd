\name{contents}
\alias{contents}
\alias{contents.data.frame}
\alias{print.contents.data.frame}
\alias{html.contents.data.frame}
\alias{contents.list}
\alias{print.contents.list}
\title{Metadata for a Data Frame}
\description{
  \code{contents} is a generic method for which \code{contents.data.frame}
  is currently the only method.  \code{contents.data.frame} creates an
  object containing the following attributes of the variables 
  from a data frame: names, labels (if any), units (if any), number of
  factor levels (if any), factor levels,
  class, storage mode, and number of NAs.  \code{print.contents.data.frame}
  will print the results, with options for sorting the variables.
  \code{html.contents.data.frame} creates HTML code for displaying the
  results.  This code has hyperlinks so that if the user clicks on the
  number of levels the browser jumps to the correct part of a table of
  factor levels for all the \code{factor} variables.  If long labels are
  present (\code{"longlabel"} attributes on variables), these are printed
  at the bottom and the \code{html} method links to them through the
  regular labels.  Variables having the same \code{levels} in the same
  order have the levels factored out for brevity.

  \code{contents.list} prints a directory of datasets when
  \code{\link{sasxport.get}} imported more than one SAS dataset.

	If \code{options(prType='html')} is in effect, calling \code{print} on
	an object that is the contents of a data frame will result in
	rendering the HTML version.  If run from the console a browser window
	will open.
}
\usage{
contents(object, \dots)
\method{contents}{data.frame}(object, sortlevels=FALSE, id=NULL,
  range=NULL, values=NULL, \dots)
\method{print}{contents.data.frame}(x,
    sort=c('none','names','labels','NAs'), prlevels=TRUE, maxlevels=Inf,
    number=FALSE, \dots) 
\method{html}{contents.data.frame}(object,
           sort=c('none','names','labels','NAs'), prlevels=TRUE, maxlevels=Inf,
           levelType=c('list','table'),
           number=FALSE, nshow=TRUE, \dots)
\method{contents}{list}(object, dslabels, \dots)
\method{print}{contents.list}(x,
    sort=c('none','names','labels','NAs','vars'), \dots)
}
\arguments{
  \item{object}{
    a data frame.  For \code{html} is an object created by
    \code{contents}.  For \code{contents.list} is a list of data frames.
  }
  \item{sortlevels}{set to \code{TRUE} to sort levels of all factor
  variables into alphabetic order.  This is especially useful when two
  variables use the same levels but in different orders.  They will
  still be recognized by the \code{html} method as having identical
  levels if sorted.}
  \item{id}{an optional subject ID variable name that if present in
		\code{object} will cause the number of unique IDs to be printed in
    the contents header}
	\item{range}{an optional variable name that if present in \code{object}
    will cause its range to be printed in the contents header}
	\item{values}{an optional variable name that if present in
    \code{object} will cause its unique values to be printed in the
    contents header}
  \item{x}{
    an object created by \code{contents}
  }
  \item{sort}{
    Default is to print the variables in their original order in the
    data frame.  Specify one of 
    \code{"names"}, \code{"labels"}, or \code{"NAs"} to sort the variables by,
    respectively, alphabetically by names, alphabetically by labels, or by
    increaseing order of number of missing values.  For
    \code{contents.list}, \code{sort} may also be the value
    \code{"vars"} to cause sorting by the number of variables in the dataset.
  }
  \item{prlevels}{
    set to \code{FALSE} to not print all levels of \code{factor} variables
  }
	\item{maxlevels}{maximum number of levels to print for a \code{factor} variable}
  \item{number}{
	set to \code{TRUE} to have the \code{print} and \code{latex} methods
	number the variables by their order in the data frame
  }
  \item{nshow}{set to \code{FALSE} to suppress outputting number of
   observations and number of \code{NA}s; useful when these counts
   would unblind information to blinded reviewers}
  \item{levelType}{
    By default, bullet lists of category levels are
    constructed in html.  Set \code{levelType='table'} to put levels in
    html table format.
  }
  \item{\dots}{
    arguments passed from \code{html} to \code{format.df},
    unused otherwise
  }
  \item{dslabels}{
    named vector of SAS dataset labels, created for
    example by \code{\link{sasdsLabels}}
  }
}
\value{
  an object of class \code{"contents.data.frame"} or
  \code{"contents.list"}.  For the \code{html} method is an \code{html}
  character vector object.
}

\author{
  Frank Harrell
  \cr
  Vanderbilt University
  \cr
  \email{fh@fharrell.com}
}
\seealso{
  \code{\link{describe}}, \code{\link{html}}, \code{\link{upData}},
  \code{\link{extractlabs}}, \code{\link{hlab}}
}
\examples{
set.seed(1)
dfr <- data.frame(x=rnorm(400),y=sample(c('male','female'),400,TRUE),
                  stringsAsFactors=TRUE)
contents(dfr)
dfr <- upData(dfr, labels=c(x='Label for x', y='Label for y'))
attr(dfr$x, 'longlabel') <-
 'A very long label for x that can continue onto multiple long lines of text'

k <- contents(dfr)
print(k, sort='names', prlevels=FALSE)
\dontrun{
html(k)
html(contents(dfr))            # same result
latex(k$contents)              # latex.default just the main information
}
}
\keyword{data}
\keyword{interface}
\concept{html}

