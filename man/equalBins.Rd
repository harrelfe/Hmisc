\name{equalBins}
\alias{equalBins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multicolumn Formating}
\description{
  Expands the width either supercolumns or the subcolumns so that the
  the sum of the supercolumn widths is the same as the sum of the
  subcolumn widths.
}
\usage{
equalBins(widths, subwidths)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{widths}{widths of the supercolumns.}
  \item{subwidths}{list of widths of the subcolumns for each supercolumn.}
}
\details{
  This determins the correct subwidths of each of various columns in a table
  for printing.  The correct width of the multicolumns is deterimed by
  summing the widths of it subcolumns.
}
\value{
  widths of the the columns for a table.
}
\author{Charles Dupont}
\seealso{\code{\link{nchar}}, \code{\link{stringDims}}}
\examples{
mcols <- c("Group 1", "Group 2")
mwidth <- nchar(mcols, type="width")
spancols <- c(3,3)
ccols <- c("a", "deer", "ad", "cat", "help", "bob")
cwidth <- nchar(ccols, type="width")

subwidths <- partition.vector(cwidth, spancols)

equalBins(mwidth, subwidths)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
