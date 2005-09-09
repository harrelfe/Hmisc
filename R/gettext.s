###  These are function that are designed to compatibility with S-plus
###  for R internationalization.  They are named with a prefix of
###  "Splus".
###
###  These functions contain representations of sprintf, gettext,
###  gettextf, and ngettext


if(!exists("sprintf")) sprintf <- function(fmt, ...) {
  ldots <- list(...)

  text <- vector("character")
  vars <- vector("character")
  i <- 1; j <- 1;
  temp <- fmt
  while (nchar(temp)) {
    ne <- regexpr('(?<!%)%[^%]*?[dixXfeEgGs]', temp, perl=TRUE)
    if( ne < 0 ) {
      text[i] <- gsub('%%', '%', temp)
      temp <- ""
    } else {
      text[i] <- gsub('%%', '%', substr(temp, 0, ne-1))
      i <- i + 1
      vars[j] <- substr(temp, ne+1, ne+attr(ne, "match.length")-1)
      j <- j + 1
      temp <- substr(temp, ne+attr(ne, "match.length"), nchar(temp))
    }
  }

  output <- NULL
  j <- 1
  for( i in 1:(length(text) - 1)) {
    output <- paste(output, text[i], sep='')
    if(regexpr('^\\d+\\$', vars[i], perl=TRUE) > 0){
      arg <- sub('^(\\d+)\\$.*$', '\\1', vars[i], perl=TRUE)
      if(arg > 0 && arg < length(ldots)) {
        val <- as.integer(arg)
      }
      else
        stop("Error")
    }
    else {
      val <- j
      j <- j + 1
    }
    output <- paste(output, ldots[[val]], sep='')
  }
  return(paste(output, text[length(text)], sep=''))
}

if(!exists("gettext")) gettext <- function(..., domain=NULL)
    return(unlist(list(...)))


if(!exists("gettextf")) gettextf <- function(fmt, ..., domain=NULL) {
  return(sprintf(fmt, ...))
}

if(!exists("ngettext")) ngettext <- function(n, msg1, msg2, domain = NULL) {
  if(n == 1)
    return(msg1)
  return(msg2)
}
