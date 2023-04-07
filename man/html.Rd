\name{html}
\alias{html}
\alias{html.latex}
\alias{html.data.frame}
\alias{html.default}
\alias{htmlVerbatim}
\title{Convert an S object to HTML}
\description{
\code{html} is a generic function, for which only two methods are currently
implemented, \code{html.latex} and a rudimentary
\code{html.data.frame}.  The former uses the \code{HeVeA} LaTeX to HTML 
translator by Maranget to create an HTML file from a LaTeX file like
the one produced by \code{latex}.  \code{html.default} just runs
\code{html.data.frame}. 
\code{htmlVerbatim} prints all of its arguments to the console in an
html verbatim environment, using a specified percent of the prevailing
character size.  This is useful for R Markdown with \code{knitr}.

Most of the html-producing functions in the Hmisc and rms packages
return a character vector passed through \code{htmltools::HTML} so that
\code{kintr} will correctly format the result without the need for the
user putting \code{results='asis'} in the chunk header.
}
\usage{
html(object, \dots)
\method{html}{latex}(object, file, where=c('cwd', 'tmp'),
  method=c('hevea', 'htlatex'),
  rmarkdown=FALSE, cleanup=TRUE, \dots)
\method{html}{data.frame}(object,
  file=paste(first.word(deparse(substitute(object))),'html',sep='.'), header,
     caption=NULL, rownames=FALSE, align='r', align.header='c',
     bold.header=TRUE, col.header='Black',
     border=2, width=NULL, size=100, translate=FALSE,
     append=FALSE, link=NULL, linkCol=1,
     linkType=c('href','name'), disableq=FALSE, \dots) 
\method{html}{default}(object,
     file=paste(first.word(deparse(substitute(object))),'html',sep='.'),
     append=FALSE, link=NULL, linkCol=1, linkType=c('href','name'), \dots)
htmlVerbatim(\dots, size=75, width=85, scroll=FALSE, rows=10, cols=100,
             propts=NULL, omit1b=FALSE)
}
\arguments{
\item{object}{a data frame or an object created by \code{latex}.
  For the generic \code{html} is any object for which an \code{html}
	method exists.} 
\item{file}{
name of the file to create.  The default file
name is \code{object.html} where \code{object} is the first word in
the name of the argument for \code{object}.  For \code{html.latex}
specify \code{file=''} or \code{file=character(0)} to print html code to
the console, as when using \code{knitr}.  For the \code{data.frame}
method, \code{file} may be set to \code{FALSE} which causes a character
vector enclosed in \code{htmltools::HTML} to be returned instead of
writing to the console. 
}
\item{where}{for \code{html}.  Default is to put output files in current
	working directory.  Specify \code{where='tmp'} to put in a system
	temporary directory area.}
\item{method}{default is to use system command \code{hevea} to convert
	from LaTeX to html.  Specify\code{method='htlatex'} to use system
	command \code{htlatex}, assuming the 
	system package \code{TeX4ht} is installed.}
\item{rmarkdown}{set to \code{TRUE} if using RMarkdown (usually under
	\code{knitr} and RStudio).  This causes html to be packaged for
	RMarkdown and output to go into the console stream.  \code{file} is
	ignored when \code{rmarkdown=TRUE}.}
\item{cleanup}{if using \code{method='htlatex'} set to \code{FALSE} if
	\code{where='cwd'} to	prevent deletion of auxiliary files created by
	\code{htlatex} that are not needed when using the final \code{html}
	document (only the \code{.css} file is needed in addition to
	\code{.html}).  If using \code{method='hevea'}, \code{cleanup=TRUE}
	causes deletion of the generated \code{.haux} file.}
\item{header}{vector of column names.  Defaults to names in
	\code{object}.  Set to \code{NULL} to suppress column names.}
\item{caption}{a character string to be used as a caption before the
	table}
\item{rownames}{set to \code{FALSE} to ignore row names even if they are
	present}
\item{align}{alignment for table columns (all are assumed to have the
	same if is a scalar).  Specify \code{"c", "r", "l"} for center, right, or left
	alignment.}
\item{align.header}{same coding as for \code{align} but pertains to
	header}
\item{bold.header}{set to \code{FALSE} to not bold face column headers}
\item{col.header}{color for column headers}
\item{border}{set to 0 to not include table cell borders, 1 to include
	only outer borders, or 2 (the default) to put borders around cells too}
\item{translate}{set to \code{TRUE} to run header and table cell text
	through the \code{htmlTranslate} function}
\item{width}{optional table width for \code{html.data.frame}.  For full
	page width use \code{width="100\%"}, for use in \code{options()} for
	printing objects.}
\item{size}{a number between 0 and 100 representing the percent of the
	prevailing character size to be used by \code{htmlVerbatim} and the
	data frame method.}
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
\item{disableq}{set to \code{TRUE} to add code to the html table tag
	that makes Quarto not use its usual table style}
\item{\dots}{ignored except for \code{htmlVerbatim} - is a list of
	objects to \code{print()}}
\item{scroll}{set to \code{TRUE} to put the html in a scrollable
	\code{textarea}}
\item{rows,cols}{the number of rows and columns to devote to the visable
	part of the scrollable box}
\item{propts}{options, besides \code{quote=FALSE} to pass to the
	\code{print} method, for \code{htmlVerbatim}}
\item{omit1b}{for \code{htmlVerbatim} if \code{TRUE} causes an initial
	and a final line of output that is all blank to be deleted}
}
\author{
Frank E. Harrell, Jr.
\cr
Department of Biostatistics,
\cr
Vanderbilt University,
\cr
\email{fh@fharrell.com}
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
