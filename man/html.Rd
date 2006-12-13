\name{html}
\alias{html}
\alias{html.latex}
\alias{html.data.frame}
\alias{html.default}
\alias{show.html}
\alias{print.html}
\title{
Convert an S object to HTML
}
\description{
\code{html} is a generic function, for which only two methods are currently
implemented, \code{html.latex} and a rudimentary
\code{html.data.frame}.  The former uses the \code{HeVeA} LaTeX to HTML 
translator by Maranget to create an HTML file from a LaTeX file like
the one produced by \code{latex}.  The resulting HTML file may be
displayed using a \code{show} or a \code{print} method.  The browser
specified in \code{options(browser=)} for R (\code{help.browser} for
S-Plus) is launched to display the HTML file.   \code{html.default} just
runs \code{html.data.frame}.
}
\usage{
html(object, \dots)
\method{html}{latex}(object, file, ...)
\method{html}{data.frame}(object,
  file=paste(first.word(deparse(substitute(object))),'html',sep='.'),
     append=FALSE, link=NULL, linkCol=1, linkType=c('href','name'), \dots)
\method{html}{default}(object,
     file=paste(first.word(deparse(substitute(object))),'html',sep='.'),
     append=FALSE, link=NULL, linkCol=1, linkType=c('href','name'), \dots)
\method{print}{html}(x, ...)
\method{show}{html}(object)
}
\arguments{
\item{object}{a data frame or an object created by \code{latex}.  For
  \code{show} is an object created by \code{html}.  For the generic
  \code{html} is any object for which an \code{html} method exists.}
\item{file}{
name of the file to create.  The default file
name is \code{object.html} where \code{object} is the first word in
the name of the argument for \code{object}.
}
\item{append}{set to \code{TRUE} to append to an existing file}
\item{link}{character vector specifying hyperlink names to attach to
  selected elements of the matrix or data frame.  No hyperlinks are used
  if \code{link} is omitted or for elements of \code{link} that are
  \code{""}.  To allow multiple links per link, \code{link} may also be
  a character matrix shaped as \code{object} in which case
  \code{linkCol} is ignored.}
\item{linkCol}{column number of \code{object} to which hyperlinks are
  attached.  Defaults to first column.}
\item{linkType}{defaults to \code{"href"}}
\item{...}{arguments passed to \code{format.df}}
\item{x}{an object created by \code{html}}
}
\section{Side Effects}{
\code{print} or \code{show} launch a browser
}
\author{
Frank E. Harrell, Jr.
\cr
Department of Biostatistics,
\cr
Vanderbilt University,
\cr
\email{f.harrell@vanderbilt.edu}
}
\references{
Maranget, Luc.  HeVeA: a LaTeX to HTML translater.
URL: http://para.inria.fr/~maranget/hevea/
}
\seealso{
\code{\link{latex}}
}
\examples{
\dontrun{
x <- matrix(1:6, nrow=2, dimnames=list(c('a','b'),c('c','d','e')))
w <- latex(x)
h <- html(w) # run HeVeA to convert .tex to .html
h <- html(x) # convert x directly to html
options(browser='konqueror')  # use help.browser for S-Plus
h            # launch html browser by running print.html
w <- html(x, link=c('','B'))   # hyperlink first row first col to B
}
}
\keyword{utilities}
\keyword{interface}
\keyword{methods}
\keyword{file}
\keyword{character}
\keyword{manip}
