\name{getHdata}
\alias{getHdata}
\title{
  Download and Install Datasets for \pkg{Hmisc}, \pkg{rms}, and Statistical
  Modeling
}
\description{
  This function downloads and makes ready to use datasets from the main
  web site for the \pkg{Hmisc} and \pkg{rms} libraries.  For \R, the
  datasets were stored in compressed \code{\link{save}} format and
  \code{getHdata} makes them available by running \code{\link{load}}
  after download.  For S-Plus, the datasets were stored in
  \code{data.dump} format and are made available by running
  \code{data.restore} after import.  The dataset is run through the
  \code{\link{cleanup.import}} function to reduce multiple inheritance
  problems for SV4 (S-Plus 5 or later).  Calling \code{getHdata} with no
  \code{file} argument provides a character vector of names of available
  datasets that are currently on the web site.  For \R, \R's default
  browser can optionally be launched to view \verb{html} files that were
  already prepared using the \pkg{Hmisc} command
  \code{html(contents())} or to view \file{.txt} or \file{.html} data
  description files when available.
}
\usage{
getHdata(file, what = c("data", "contents", "description", "all"),
         where="http://biostat.mc.vanderbilt.edu/twiki/pub/Main/DataSets")
}
\arguments{
  \item{file}{
    an unquoted name of a dataset on the web site, e.g. \samp{prostate}.
    Omit \code{file} to obtain a list of available datasets.
  }
  \item{what}{
    specify \code{what="contents"} to browse the contents (metadata) for
    the dataset rather than fetching the data themselves.  Specify
    \code{what="description"} to browse a data description file if
    available.  Specify \code{what="all"} to retrieve the data and see
    the metadataand description.
  }
  \item{where}{
    \acronym{URL} containing the data and metadata files
  }
}
\details{
  For S-Plus, Hmisc defines a function \code{download.file} that is used
  by \code{getHdata}.  This is a stripped-down version of the \R
  \code{\link{download.file}} function that uses the system
  \command{wget} executable for fetching files from the Internet.  For
  Unix and Linux systems, \command{wget} will be pre-installed usually.
  For windows S-Plus systems, get \command{wget} from
  \url{ftp://sunsite.dk/projects/wget/windows}.  Once you unzip the file
  from there, move \command{wget.exe} to the same Windows directory that
  contains \command{ftp.exe}.
}
\value{
  \code{getHdata()} without a \code{file} argument returns a character
  vector of dataset base names.  When a dataset is downloaded, the data
  frame is placed in search position one and is not returned as value of
  \code{getHdata}.
}
\author{Frank Harrell}
\seealso{
  \code{\link{download.file}}, \code{\link{cleanup.import}},
  \code{\link[foreign:read.S]{data.restore}}, \code{\link{load}}
}
\examples{
\dontrun{
getHdata()          # download list of available datasets
getHdata(prostate)  # downloads, load( ) or data.restore( )
                    # runs cleanup.import for S-Plus 6
getHdata(valung, "contents")   # open browser (options(browser="whatever"))
                    # after downloading valung.html
                    # (result of html(contents()))
getHdata(support, "all")  # download and open one browser window
datadensity(support)
attach(support)     # make individual variables available
getHdata(plasma,  "all")  # download and open two browser windows
                          # (description file is available for plasma)
}
}
\keyword{interface}
\keyword{data}
