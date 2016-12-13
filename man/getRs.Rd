\name{getRs}
\alias{getRs}
\title{Interact with github rscripts Project}
\description{
	The github rscripts project at
	\url{https://github.com/harrelfe/rscripts} contains R scripts that are
	primarily analysis templates for teaching with RStudio.  This function
	allows the user to print an organized list of available scripts, to
	download a script and load it into an RStudio script editor window, to
	list scripts whose major category contains a given string (ignoring
	case), or to list all major and mino categories.
}
\usage{
getRs(file=NULL, guser='harrelfe', grepo='rscripts', gdir='raw/master',
      dir=NULL, browse=c('local', 'browser'), cats=FALSE,
      put=c('rstudio', 'source'))
}
\arguments{
  \item{file}{a character string containing a script file name.
    Omit \code{file} to obtain a list of available scripts with major
    and minor categories.}
	\item{guser}{GitHub user name, default is \code{'harrelfe'}}
	\item{grepo}{Github repository name, default is \code{'rscripts'}}
	\item{gdir}{Github directory under which to find retrievable files}
	\item{dir}{directory under \code{grepo} in which to find files}
  \item{browse}{When showing the rscripts contents directory, the
      default is to list in tabular form in the console.  Specify
      \code{browse='browser'} to open the online contents in a web
      browser.}
	\item{cats}{Leave at the default (\code{FALSE}) to list whole contents
      or download a script.  Specify \code{cats=TRUE} to list major and
      minor categories available.  Specify a character string to list
      all scripts whose major category contains the string (ignoring
			case).}
	\item{put}{Leave at the default (\code{'rstudio'}) to load \code{file}
		into the RStudio script editor window.  Use \code{put='source'} to
		\code{source()} the file.  This is useful when the file just defines
		a function you want to use in the session.}
}
\value{a data frame or list, depending on arguments}
\author{Frank Harrell and Cole Beck}
\seealso{\code{\link{download.file}}}
\examples{
\dontrun{
getRs()             # list available scripts
scripts <- getRs()  # likewise, but store in an object that can easily
                    # be viewed on demand in RStudio
getRs('introda.r')  # download introda.r and put in script editor
getRs(cats=TRUE)    # list available major and minor categories
categories <- getRs(cats=TRUE)
# likewise but store results in a list for later viewing
getRs(cats='reg')   # list all scripts in a major category containing 'reg'
getRs('importREDCap.r', put='source')   # source() to define a function
# source() a new version of the Hmisc package's cut2 function:
getRs('cut2.s', grepo='Hmisc', dir='R', put='source')
}
}
\keyword{interface}
