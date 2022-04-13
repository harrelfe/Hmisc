\name{knitrSet}
\alias{knitrSet}
\alias{plotlySave}
\title{knitr Setup and plotly Service Function}
\description{
	\code{knitrSet} sets up knitr to use better default parameters for base graphics,
	better code formatting, and to allow several arguments to be passed
	from code chunk headers, such as \code{bty}, \code{mfrow}, \code{ps},
	\code{bot} (extra bottom margin for base graphics), \code{top} (extra
	top margin), \code{left} (extra left margin), \code{rt} (extra right
	margin), \code{lwd}, \code{mgp}, \code{las}, \code{tcl}, \code{axes},
	\code{xpd}, \code{h} (usually \code{fig.height} in knitr),
	\code{w} (usually \code{fig.width} in knitr), \code{wo}
	(\code{out.width} in knitr), \code{ho} (\code{out.height} in knitr),
	\code{cap} (character
	string containing figure caption), \code{scap} (character string
	containing short figure caption for table of figures).  The
	\code{capfile} argument facilities auto-generating a table of figures
	for certain Rmarkdown report themes.  This is done by the addition of
	a hook function that appends data to the \code{capfile} file each time
	a chunk runs that has a long or short caption in the chunk header.

	\code{plotlySave} saves a plotly graphic with name \code{foo.png}
	where \code{foo} is the name of the current chunk.  You must have a
	free \code{plotly} account from \code{plot.ly} to use this function,
	and you must have run 
  \code{Sys.setenv(plotly_username="your_plotly_username")} and
  \code{Sys.setenv(plotly_api_key="your_api_key")}. The API key can be
	found in one's profile settings.
}
\usage{
knitrSet(basename=NULL, w=if(! bd) 4, h=if(! bd) 3, wo=NULL, ho=NULL,
         fig.path=if(length(basename)) basename else '',
         fig.align=if(! bd) 'center', fig.show='hold',
         fig.pos=if(! bd) 'htbp',
         fig.lp    = if(! bd) paste('fig', basename, sep=':'),
         dev=switch(lang, latex='pdf', markdown='png',
                    blogdown=NULL, quarto=NULL),
         tidy=FALSE, error=FALSE,
         messages=c('messages.txt', 'console'),
         width=61, decinline=5, size=NULL, cache=FALSE,
         echo=TRUE, results='markup', capfile=NULL,
         lang=c('latex','markdown','blogdown','quarto'))

plotlySave(x, \dots)
}
\arguments{
  \item{basename}{base name to be added in front of graphics file
		names. \code{basename} is followed by a minus sign.}
	\item{w,h}{default figure width and height in inches}
	\item{wo,ho}{default figure rendering width and height, in integer
		pixels or percent as a character string, e.g. \code{'40\%'}}
	\item{fig.path}{path for figures.  To put figures in a subdirectory
		specify e.g. \code{fig.path='folder/'}.  Ignored for blogdown.}
	\item{fig.align,fig.show,fig.pos,fig.lp,tidy,cache,echo,results,error,size}{see knitr documentation} 
	\item{dev}{graphics device, with default figured from \code{lang}}
	\item{messages}{By default warning and other messages such as those
		from loading packages are sent to file \code{'messages.txt'} in the
		current working directory.  You can specify
		\code{messages='console'} to send them directly to the console.}
	\item{width}{text output width for R code and output}
	\item{decinline}{number of digits to the right of the decimal point to
		round numeric values appearing inside Sexpr}
	\item{capfile}{the name of a file in the current working directory
		that is used to accumulate chunk labels, figure cross-reference
		tags, and figure short captions (long captions if no short caption
		is defined) for the purpose of using
		\code{markupSpecs$markdown$tof()} to insert a table of figures in a
		report.  The file as appended to, which is useful if
		\code{cache=TRUE} is used since this will keep some chunks from
		running.  The \code{tof} function will remove earlier duplicated
		figure tags if this is the case.  If not \code{cache}ing, the user
		should initialize the file to empty at the top of the script.}
	\item{lang}{Default is \code{'latex'} to use LaTeX.  Set to
		\code{'markdown'} when using R Markdown or \code{'blogdown'} or
		\code{'quarto'}.  For 
		\code{'blogdown'} and \code{'quarto'}, \code{par} and \code{knitr}
		graphics-related hooks are not called as this would prevent
		writing graphics files in the correct directory
		for the blog system.}
	\item{x}{a \code{plotly} graphics object or a named list of such
		objects.  The resulting \code{png} file will go in the file path
		given by the \code{knitr} \code{fig.path} value, and have a base
		name equal to the current \code{knitr} chunk name.  If \code{x} is a
		list, a minus sign followed by the chunk name are inserted before
		\code{.png}.}
	\item{\dots}{additional arguments passed to \code{plotly::plotly_IMAGE}}
}
\author{Frank Harrell}
\seealso{\code{\link[knitr]{knit}}}
\examples{
\dontrun{
# Typical call (without # comment symbols):
# <<echo=FALSE>>=
# require(Hmisc)
# knitrSet()
# @

knitrSet()    # use all defaults and don't use a graphics file prefix
knitrSet('modeling')   # use modeling- prefix for a major section or chapter
knitrSet(cache=TRUE, echo=FALSE)  # global default to cache and not print code
knitrSet(w=5,h=3.75)   # override default figure width, height

# ```{r chunkname}
# p <- plotly::plot_ly(...)
# plotlySave(p)   # creates fig.path/chunkname.png
}
}
\keyword{interface}
