\name{sedit}
\alias{sedit}
\alias{substring.location}
\alias{substring2}
\alias{substring2<-}
\alias{replace.substring.wild}
\alias{numeric.string}
\alias{all.digits}
\title{
  Character String Editing and Miscellaneous Character Handling Functions
}
\description{
  This suite of functions was written to implement many of the features
  of the UNIX \code{sed} program entirely within S (function \code{sedit}).
  The \code{substring.location} function returns the first and last position
  numbers that a sub-string occupies in a larger string.  The \code{substring2<-}
  function does the opposite of the builtin function \code{substring}.
  It is named \code{substring2} because for S-Plus there is a built-in
  function \code{substring}, but it does not handle multiple replacements in
  a single string.
  \code{replace.substring.wild} edits character strings in the fashion of
  "change xxxxANYTHINGyyyy to aaaaANYTHINGbbbb", if the "ANYTHING"
  passes an optional user-specified \code{test} function.  Here, the
  "yyyy" string is searched for from right to left to handle
  balancing parentheses, etc.  \code{numeric.string}
  and \code{all.digits} are two examples of \code{test} functions, to check,
  respectively if each of a vector of strings is a legal numeric or if it contains only
  the digits 0-9.  For the case where \code{old="*$" or "^*"}, or for
  \code{replace.substring.wild} with the same values of \code{old} or with
  \code{front=TRUE} or \code{back=TRUE}, \code{sedit} (if \code{wild.literal=FALSE}) and
  \code{replace.substring.wild} will edit the largest substring
  satisfying \code{test}.

  \code{substring2} is just a copy of \code{substring} so that
  \code{substring2<-} will work.
}
\usage{
sedit(text, from, to, test, wild.literal=FALSE)
substring.location(text, string, restrict)
# substring(text, first, last) <- setto   # S-Plus only
replace.substring.wild(text, old, new, test, front=FALSE, back=FALSE)
numeric.string(string)
all.digits(string)
substring2(text, first, last)
substring2(text, first, last) <- value
}
\arguments{
  \item{text}{
    a vector of character strings for \code{sedit, substring2, substring2<-}
    or a single character string for \code{substring.location,
      replace.substring.wild}.
  }
  \item{from}{
    a vector of character strings to translate from, for \code{sedit}.
    A single asterisk wild card, meaning allow any sequence of characters
    (subject to the \code{test} function, if any) in place of the \code{"*"}.
    An element of \code{from} may begin with \code{"^"} to force the match to
    begin at the beginning of \code{text}, and an element of \code{from} can end with
    \code{"$"} to force the match to end at the end of \code{text}.
  }
  \item{to}{
    a vector of character strings to translate to, for \code{sedit}.
    If a corresponding element in \code{from} had an \code{"*"}, the element
    in \code{to} may also have an \code{"*"}.  Only single asterisks are allowed.
    If \code{to} is not the same length as \code{from}, the \code{rep} function
    is used to make it the same length.
  }
  \item{string}{
    a single character string, for \code{substring.location}, \code{numeric.string},
    \code{all.digits}
  }
  \item{first}{
    a vector of integers specifying the first position to replace for
    \code{substring2<-}.  \code{first} may also be a vector of character strings
    that are passed to \code{sedit} to use as patterns for replacing
    substrings with \code{setto}.  See one of the last examples below.
  }
  \item{last}{
    a vector of integers specifying the ending positions of the character
    substrings to be replaced.  The default is to go to the end of
    the string.  When \code{first} is character, \code{last} must be
    omitted.
  }
  \item{setto}{
    a character string or vector of character strings used as replacements,
    in \code{substring2<-}
  }
  \item{old}{
    a character string to translate from for \code{replace.substring.wild}.
    May be \code{"*$"} or \code{"^*"} or any string containing a single \code{"*"} but
    not beginning with \code{"^"} or ending with \code{"$"}.
  }
  \item{new}{
    a character string to translate to for \code{replace.substring.wild}
  }
  \item{test}{
    a function of a vector of character strings returning a logical vector
    whose elements are \code{TRUE} or \code{FALSE} according
    to whether that string element qualifies as the wild card string for
    \code{sedit, replace.substring.wild}
  }
  \item{wild.literal}{
    set to \code{TRUE} to not treat asterisks as wild cards and to not look for
    \code{"^"} or \code{"$"} in \code{old}
  }
  \item{restrict}{
    a vector of two integers for \code{substring.location} which specifies a
    range to which the search for matches should be restricted
  }
  \item{front}{
    specifying \code{front = TRUE} and \code{old = "*"} is the same as
    specifying \code{old = "^*"}
  }
  \item{back}{
    specifying \code{back = TRUE} and \code{old = "*"} is the same as
    specifying \code{old = "*$"}
  }
  \item{value}{a character vector}
}
\value{
  \code{sedit} returns a vector of character strings the same length as \code{text}.
  \code{substring.location} returns a list with components named \code{first}
  and \code{last}, each specifying a vector of character positions corresponding
  to matches.  \code{replace.substring.wild} returns a single character string.
  \code{numeric.string} and \code{all.digits} return a single logical value.
}
\section{Side Effects}{
  \code{substring2<-} modifies its first argument
}
\author{
  Frank Harrell
  \cr
  Department of Biostatistics
  \cr
  Vanderbilt University School of Medicine
  \cr
  \email{fh@fharrell.com}
}
\seealso{
  \code{\link{grep}}, \code{\link{substring}}
}
\examples{
x <- 'this string'
substring2(x, 3, 4) <- 'IS'
x
substring2(x, 7) <- ''
x


substring.location('abcdefgabc', 'ab')
substring.location('abcdefgabc', 'ab', restrict=c(3,999))


replace.substring.wild('this is a cat','this*cat','that*dog')
replace.substring.wild('there is a cat','is a*', 'is not a*')
replace.substring.wild('this is a cat','is a*', 'Z')


qualify <- function(x) x==' 1.5 ' | x==' 2.5 '
replace.substring.wild('He won 1.5 million $','won*million',
                       'lost*million', test=qualify)
replace.substring.wild('He won 1 million $','won*million',
                       'lost*million', test=qualify)
replace.substring.wild('He won 1.2 million $','won*million',
                       'lost*million', test=numeric.string)


x <- c('a = b','c < d','hello')
sedit(x, c('=','he*o'),c('==','he*'))


sedit('x23', '*$', '[*]', test=numeric.string)
sedit('23xx', '^*', 'Y_{*} ', test=all.digits)


replace.substring.wild("abcdefabcdef", "d*f", "xy")


x <- "abcd"
substring2(x, "bc") <- "BCX"
x
substring2(x, "B*d") <- "B*D"
x
}
\keyword{manip}
\keyword{character}
% Converted by Sd2Rd version 1.21.
