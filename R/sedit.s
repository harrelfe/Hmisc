sedit <- function(text, from, to, test=NULL, wild.literal=FALSE)
{
  to <- rep(to, length.out=length(from))
  for(i in seq_along(text)) {
    s <- text[i]
    if(length(s))
      for(j in 1:length(from)) {
        old <- from[j]
        front <- back <- FALSE
        if(!wild.literal) {
          if(substring(old,1,1) == '^') {
            front <- TRUE;
            old <- substring(old,2)
          }

          if(substring(old,nchar(old)) == '$') { 
            back <- TRUE; old <- substring(old, 1, nchar(old)-1)
          }
        }

        new <- to[j]

        lold <- nchar(old)
        if(lold > nchar(s))
          next

        ex.old <- substring(old, 1:lold, 1:lold)
        if(!wild.literal && any(ex.old == '*')) 
          s <- replace.substring.wild(s, old, new, test=test, front=front, back=back)
        else {
          l.s <- nchar(s)
          is <- 1:(l.s-lold+1)
          if(front)
            is <- 1

          ie <- is + lold - 1
          if(back)
            ie <- l.s

          ss <- substring(s, is, ie)
          k <- ss == old
          if(!any(k))
            next

          k <- is[k]
          substring2(s, k, k+lold-1) <- new
        }
      }

    text[i] <- s
  }

  text
}


substring.location <- function(text, string, restrict)
{
  if(length(text) > 1)
    stop('only works with a single character string')
  
  l.text <- nchar(text)
  l.string <- nchar(string)
  if(l.string > l.text)
    return(list(first=0,last=0))
  
  if(l.string == l.text)
    return(if(text == string)
             list(first=1,last=l.text)
           else 
             list(first=0,last=0))

  is <- 1:(l.text-l.string+1)
  ss <- substring(text, is, is+l.string-1)
  k <- ss == string
  if(!any(k))
    return(list(first=0,last=0))
  
  k <- is[k]
  if(!missing(restrict))
    k <- k[k>=restrict[1] & k<=restrict[2]]
  
  if(length(k) == 0)
    return(list(first=0,last=0))
  
  list(first=k, last=k+l.string-1)
}


## if(version$major < 5)  14Sep00
substring2 <- function(text, first, last=100000L)
  base::substring(text, first, last)

'substring2<-' <- function(text, first, last=100000, value)
{
  if(is.character(first)) {
    if(!missing(last))
      stop('wrong # arguments')
    
    return(sedit(text, first, value))  ## value was setto 25May01
  }

  lf <- length(first)

  if(length(text) == 1 && lf > 1) {
    if(missing(last))
      last <- nchar(text)

    last <- rep(last, length.out=lf)
    for(i in 1:lf) {
      text <- paste(if(first[i]>1) 
                      substring(text, 1, first[i]-1),
                    value,
                    substring(text, last[i]+1), sep='')

      if(i < lf) {
        j <- (i+1):lf
        w <- nchar(value) - (last[i]-first[i]+1)
        first[j] <- first[j] + w  
        last[j] <- last[j] +  w
      }
    }

    return(text)
  }
  text <- paste(ifelse(first>1,substring(text, 1, first-1),''), value,
                substring(text, last+1), sep='')
  text
}


replace.substring.wild <- function(text, old, new, test=NULL, 
                                   front=FALSE, back=FALSE)
{
  if(length(text)>1)
    stop('only works with a single character string')

  if(missing(front) && missing(back)) {
    if(substring(old,1,1) == '^') {
      front <- TRUE;
      old <- substring(old,2)
    }

    if(substring(old, nchar(old)) == '$') {
      back <- TRUE
      old <- substring(old, 1, nchar(old)-1)
    }
  }
  if((front || back) && old!='*') 
    stop('front and back (^ and $) only work when the rest of old is *')

  star.old <- substring.location(old,'*')
  if(length(star.old$first)>1)
    stop('does not handle > 1 * in old')
  
  if(sum(star.old$first) == 0)
    stop('no * in old')
  
  star.new <- substring.location(new,'*')
  if(length(star.new$first)>1)
    stop('cannot have > 1 * in new')

  if(old == '*' && (front | back)) {
    if(front && back)
      stop('may not specify both front and back (or ^ and $) with old=*')
    
    if(length(test) == 0)
      stop('must specify test= with old=^* or *$')
    
    et <- nchar(text)
    if(front) {
      st <- rep(1, et);
      en <- et:1
    } else {
      st <- 1:et;
      en <- rep(et, et)
    }

    qual <- test(substring(text, st, en))
    if(!any(qual))
      return(text)
    
    st <- (st[qual])[1]
    en <- (en[qual])[1]
    text.before <- if(st == 1)''
                   else substring(text, 1, st-1)
    
    text.after  <- if(en == et)''
                   else substring(text, en+1, et)
    
    text.star   <- substring(text, st, en)
    new.before.star <-
      if(star.new$first>1) 
        substring(new, 1, star.new$first-1)
      else ''

    new.after.star <- if(star.new$last == length(new))''
                      else substring(new, star.new$last+1)

    return(paste(text.before, new.before.star, text.star, new.after.star,
                 text.after, sep=''))
  }

  old.before.star <- if(star.old$first == 1)''
                     else substring(old, 1, star.old$first-1)
  
  old.after.star  <- if(star.old$last == nchar(old))''
                     else substring(old, star.old$first+1)

  if(old.before.star == '')
    loc.before <- list(first=0, last=0)
  else {
    loc.before <- substring.location(text, old.before.star)
    loc.before <- list(first=loc.before$first[1], last=loc.before$last[1])
  }

  if(sum(loc.before$first+loc.before$last) == 0)
    return(text)

  loc.after <- if(old.after.star == '') list(first=0, last=0)
               else {
                 la <- substring.location(text, old.after.star, 
                                          restrict=c(loc.before$last+1,1e10))
                 lastpos <- length(la$first)
                 la <- list(first=la$first[lastpos], last=la$last[lastpos])
                 if(la$first+la$last == 0)
                   return(text)

                 la
               }

  loc.star <- list(first=loc.before$last+1, 
                   last=if(loc.after$first == 0) nchar(text)
                        else loc.after$first-1)
  
  star.text <- substring(text, loc.star$first, loc.star$last)
  if(length(test) && !test(star.text))
    return(text)

  if(star.new$first == 0)
    return(paste(if(loc.before$first>1)substring(text,1,loc.before$first-1),
                 new, sep=''))

  new.before.star <- if(star.new$first == 1)''
                     else substring(new, 1, star.new$first-1)
  new.after.star  <- if(star.new$last == nchar(new)) ''
                     else substring(new, star.new$first+1)

  paste(if(loc.before$first>1)substring(text,1,loc.before$first-1),
        new.before.star,
        substring(text,loc.star$first,loc.star$last),
        new.after.star,
        if(loc.after$last<nchar(text) && loc.after$last>0) 
          substring(text,loc.after$last+1),
        sep='')
}


## Some functions useful as test= arguments to replace.substring.wild, sedit
numeric.string <- function(string) suppressWarnings(!is.na(as.numeric(string)))

all.digits <- function(string)
{
  k <- length(string)
  result <- logical(k)
  for(i in 1:k) {
    st <- string[i]
    ls <- nchar(st)
    ex <- substring(st, 1:ls, 1:ls)
    result[i] <- all(match(ex,c('0','1','2','3','4','5','6','7','8','9'),nomatch=0)>0)
  }
  
  result
}
