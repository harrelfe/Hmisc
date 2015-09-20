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
\method{html}{latex}(object, file, where=c('cwd', 'tmp'),
  method=c('htlatex', 'hevea'),
  rmarkdown=FALSE, cleanup=TRUE, ...)
\method{html}{data.frame}(object,
  file=paste(first.word(deparse(substitute(object))),'html',sep='.'),
     append=FALSE, link=NULL, linkCol=1, linkType=c('href','name'), \dots)
\method{html}{default}(object,
     file=paste(first.word(deparse(substitute(object))),'html',sep='.'),
     append=FALSE, link=NULL, linkCol=1, linkType=c('href','name'), \dots)
\method{print}{html}(x, \dots)
\method{show}{html}(object)
}
\arguments{
\item{object}{a data frame or an object created by \code{latex}.  For
  \code{show} is an object created by \code{html}.  For the generic
  \code{html} is any object for which an \code{html} method exists.}
\item{file}{
name of the file to create.  The default file
name is \code{object.html} where \code{object} is the first word in
the name of the argument for \code{object}.  For \code{html.latex}
specify \code{file=''} or \code{file=character(0)} to print html code to
the console, as when using \code{knitr}. 
}
\item{where}{for \code{html}.  Default is to put output files in current
	working directory.  Specify \code{where='tmp'} to put in a system
	temporary directory area.}
\item{method}{default is to use system command \code{htlatex} to convert
	from LaTeX to html (this is from the \code{tex4ht} package).  Specify
	\code{method='hevea'} to use system command \code{hevea}, assuming the
	system package \code{hevea} is installed.}
\item{rmarkdown}{set to \code{TRUE} if using RMarkdown (usually under
	\code{knitr} and RStudio).  This causes html to be packaged for
	RMarkdown and output to go into the console stream.  \code{file} is
	ignored when \code{rmarkdown=TRUE}.}
\item{cleanup}{if using \code{method='htlatex'} set to \code{FALSE} if
	\code{where='cwd'} to	prevent deletion of auxiliary files created by
	\code{htlatex} that are not needed when using the final \code{html}
	document (only the \code{.css} file is needed in addition to \code{.html})}
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
\item{\dots}{ignored}
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
h            # launch html browser by running print.html
w <- html(x, link=c('','B'))   # hyperlink first row first col to B

# Assuming system package tex4ht is installed, easily convert advanced
# LaTeX tables to html
getHdata(pbc)
s <- summaryM(bili + albumin + stage + protime + sex + age + spiders ~ drug,
              data=pbc, test=TRUE)
w <- latex(s, npct='slash', file='s.tex')
z <- html(w)
browseURL(z$file)

d <- describe(pbc)
w <- latex(d, file='d.tex')
z <- html(w)
browseURL(z$file)
}
}
\keyword{utilities}
\keyword{interface}
\keyword{methods}
\keyword{file}
\keyword{character}
\keyword{manip}
