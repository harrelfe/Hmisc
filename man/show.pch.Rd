\name{show.pch}
\alias{show.pch}
\alias{show.col}
\alias{character.table}
\title{Display Colors, Plotting Symbols, and Symbol Numeric Equivalents}
\description{
\code{show.pch} plots the definitions of the \code{pch} parameters.
\code{show.col} plots definitions of integer-valued colors.
\code{character.table} draws numeric equivalents of all latin
characters; the character on line \code{xy} and column \code{z} of the
table has numeric code \code{"xyz"}, which you would surround in quotes
and preceed by a backslash.
}
\usage{
show.pch(object = par("font"))
show.col(object=NULL)
character.table(font=1)
}
\arguments{
  \item{object}{font for \code{show.pch}, ignored for \code{show.col}.}
  \item{font}{font}
}
\author{Pierre Joyet \email{pierre.joyet@bluewin.ch}, Frank Harrell}
\seealso{\code{\link{points}}, \code{\link{text}}}
\examples{
\dontrun{
show.pch()
show.col()
character.table()
}}
\keyword{aplot}
