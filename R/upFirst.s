upFirst <- function(txt, lower=FALSE, alllower=FALSE) {
  f <- function(x) {
  notcap <- c('a', 'about', 'above', 'across', 'after', 'against',
                'along', 'among', 'an', 'and', 'around', 'as', 'at',
                'before', 'behind', 'below', 'beneath', 'beside',
                'besides', 'between', 'beyond', 'but', 'by', 'despite',
                'down', 'during', 'except', 'following', 'for', 'from',
                'in', 'inside', 'into', 'like', 'mid', 'minus', 'near',
                'next', 'nor', 'of', 'off', 'on', 'onto', 'opposite',
                'or', 'out', 'outside', 'over', 'past', 'per', 'plus',
                'regarding', 'round', 'save', 'since', 'so', 'than',
                'the', 'through', 'throughout', 'till', 'times',
                'to', 'toward', 'towards', 'under', 'underneath',
                'unlike', 'until', 'up', 'upon', 'via', 'vs.', 'when',
                'with', 'within', 'without', 'worth', 'yet')
  s <- strsplit(x, " ")[[1]]
  ## Find words that have more than one upper case letter; assume these
  ## are acronyms that need capitalization preserved
  a <- grepl('[A-Z]{1,}.*[A-Z]{1,}', s)
  s <- if(alllower)
         ifelse(a, s, tolower(s))
  else if(lower)
         ifelse(a, s, ifelse((1 : length(s)) == 1,
                             paste(toupper(substring(s, 1, 1)),
                                   tolower(substring(s, 2)), sep=''),
                             tolower(s)))
       else
         ifelse(a, s, ifelse((1 : length(s)) == 1 | s %nin% notcap,
                             paste(toupper(substring(s, 1, 1)),
                                   tolower(substring(s, 2)), sep=''),
                             tolower(s)))
  paste(s, collapse=' ')
}
  for(i in 1 : length(txt)) txt[i] <- f(txt[i])
  txt
}
