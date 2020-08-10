\name{upFirst}
\alias{upFirst}
\title{Change First Letters to Upper Case}
\usage{
upFirst(txt, lower = FALSE, alllower = FALSE)
}
\arguments{
\item{txt}{a character vector}

\item{lower}{set to \code{TRUE} to make only the very first letter of the string upper case, and to keep words with at least 2 capital letters in their original form}

\item{alllower}{set to \code{TRUE} to make every word start with lower case unless it has at least 2 caps}
}
\description{
Changes the first letter of each word in a string to upper case, keeping selected words in lower case.  Words containing at least 2 capital letters are kept as-is.
}
\examples{
upFirst(c('this and that','that is Beyond question'))
}
\references{
\url{https://en.wikipedia.org/wiki/Letter_case#Headings_and_publication_titles}
}
