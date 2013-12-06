\name{translate}
\alias{translate}
\title{
Translate Vector or Matrix of Text Strings
}
\description{
Uses the UNIX tr command to translate any character in \code{old} in
\code{text} to the corresponding character in \code{new}.  If multichar=T
or \code{old} and \code{new} have more than one element, or each have one element
but they have different numbers of characters,
uses the UNIX \code{sed} command to translate the series of characters in
\code{old} to the series in \code{new} when these characters occur in \code{text}.
If \code{old} or \code{new} contain a backslash, you sometimes have to quadruple
it to make the UNIX command work. If they contain a forward slash,
preceed it by two backslashes.  Invokes the builtin chartr function if
\code{multichar=FALSE}. 
}
\usage{
translate(text, old, new, multichar=FALSE)
}
\arguments{
\item{text}{
scalar, vector, or matrix of character strings to translate.
}
\item{old}{
vector old characters
}
\item{new}{
corresponding vector of new characters
}
\item{multichar}{See above.}
}
\value{
an object like text but with characters translated
}
\seealso{grep}
\examples{
translate(c("ABC","DEF"),"ABCDEFG", "abcdefg")
translate("23.12","[.]","\\\\cdot ") # change . to \cdot
translate(c("dog","cat","tiger"),c("dog","cat"),c("DOG","CAT"))
# S-Plus gives  [1] "DOG"   "CAT"   "tiger" - check discrepency
translate(c("dog","cat2","snake"),c("dog","cat"),"animal")
# S-Plus gives  [1] "animal"  "animal2" "snake" 
}
\keyword{character}
