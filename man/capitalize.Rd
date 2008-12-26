\name{capitalize}
\alias{capitalize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ capitalize the first letter of a string}
\description{
Capitalizes the first letter of each element of the string vector.
}
\usage{
capitalize(string)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{ String to be capitalized }
}
\value{
  Returns a vector of charaters with the first letter capitalized
}
\author{ Charles Dupont }
\examples{
capitalize(c("Hello", "bob", "daN"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ character }% __ONLY ONE__ keyword per line
