tabulr <- function(formula, data=NULL, nolabel=NULL, nofill=NULL, ...) {
  ## require(gsubfn) || stop('package gsubfn not installed')
  if (!requireNamespace("tables", quietly = TRUE))
    stop("This function requires the 'tables' package.")
  if(!length(data)) data <- environment(formula)
  else if(is.list(data)) data <- list2env(data, parent=environment(formula))

  ##  f <- as.character(deparse(formula))
  # lab <- function(x, hfill=TRUE) {
    ## x <- gsub('^ +', '', x)
    ## x <- gsub(' +$', '', x)
  #   l <- labelLatex(get(x, envir=data), default=x, double=TRUE, hfill=hfill)
  #   paste("Heading('", l, "')*", x, sep='')
  # }
  lab <- function(x) {
    x <- deparse(x)
    if(x == 'trio') return('table_trio')
    if(x == 'freq') return('table_freq')
    if(x == 'N')    return('Heading()*table_N')
    if(! (exists(x, envir=data, mode='numeric') |
          exists(x, envir=data, mode='character'))) return(x)
    if(length(nolabel) && x %in% all.vars(nolabel)) return(x)
    xval <- get(x, envir=data)
    if(label(xval) == '') return(x)
    l <- labelLatex(xval, double=FALSE,
                    hfill=!length(nofill) || x %nin% all.vars(nofill))
    paste("Heading('", l, "')*", x, sep='')
  }
       
#  f <-  gsubfn("\\.\\((.*?)\\)", ~ lab(x), f)
#  f <- gsubfn("\\.n\\((.*?)\\)", ~ lab(x,  hfill=FALSE), f)
#  f <- gsubfn("\\.n\\((.*?)\\)", ~ lab(x,  hfill=FALSE), f)
#  f <- gsubfn('([ \\(]+)l \\* *([A-Za-z\\_\\.][A-Z0-9a-z\\_\\.]*?)',
#              ~ paste(x, lab(y), sep=''), f)
#  f <- gsubfn('([ \\(]+)l\\. +\\* *([A-Za-z\\_\\.][A-Z0-9a-z\\_\\.]*?)',
#              ~ paste(x, lab(y, hfill=FALSE), sep=''), f)
  ## A variable is a string of characters, _, . not starting with 0-9
  ## delimited by
#  f <- gsubfn('[ \\(\\*\\+ ]*([A-Za-z\\_\\.]+[A-Za-z0-9\\_\\.]*)[ \\(\\*\\+]*', ~ paste('#',x,'#',sep=''), '1a+b')
#  gsubfn('[ \\(\\*\\+ ]*([A-Za-z\\_\\.]+[A-Za-z0-9\\_\\.]*)[ \\(\\*\\+]*', ~ paste('#',x,'#',sep=''), '1a+b*dd + f==h' 
#  f <- gsubfn( "([a-zA-Z_\\.][a-zA-Z0-9_\\.]*)((?=\\s*[-+~)*])|\\s*$)", 
#              ~ paste0(toupper(x),'z'), f, perl=TRUE ) 
# From Bill Dunlap

  ff <- function(expr, convertName) { 
    if (is.call(expr) && is.name(expr[[1]]) &&
        is.element(as.character(expr[[1]]),
                   c("~","+","-","*","/","%in%","%nin%","(", ":"))) { 
      for(i in seq_along(expr)[-1])
        expr[[i]] <- Recall(expr[[i]], convertName = convertName) 
    } else if (is.name(expr)) expr <- as.name(convertName(expr)) 
    expr 
  } 

  f <- ff(formula, lab)
  f <- as.formula(gsub("`", "", as.character(deparse(f))))
  result <- tables::tabular(f, data=data, ...)
  attr(result, 'originalformula') <- formula
  result
}

table_trio <- function(x) {
  if (!requireNamespace("tables", quietly = TRUE))
    stop("This function requires the 'tables' package.")
  o <- tables::table_options()
  s <- function(x, default) if(length(x)) x else default
  left     <- s(o$left,  3)
  right    <- s(o$right, 1)
  prmsd    <- s(o$prmsd, FALSE)
  pn       <- s(o$pn,    FALSE)
  pnformat <- s(o$pnformat, "n")
  pnwhen   <- s(o$pnwhen,   "all")
  bold     <- s(o$bold,  FALSE)

  isna <- is.na(x)
  x <- x[!isna]
  if(!length(x)) return('')
  qu <- quantile(x, (1:3)/4)
  w <- paste('{\\smaller ', nFm(qu[1], left, right), '} ',
             if(bold) '\\textbf{', nFm(qu[2], left, right), if(bold) '}',
             ' {\\smaller ', nFm(qu[3], left, right), '}', sep='')
  if(pnwhen == 'ifna' && !any(isna)) pn <- FALSE
  if(prmsd || pn) {
    w <- paste(w, '~{\\smaller (', sep='')
    if(prmsd) w <- paste(w, nFm(mean(x), left, right), '$\\pm$',
                            nFm(sd(x),   left, right), sep='')
    if(pn)    w <- paste(w, if(prmsd)' ', '$',
                         if(pnformat == 'n') 'n=', length(x), '$', sep='')
    w <- paste(w,  ')}', sep='')
  }
  w
}

table_N <- function(x) paste('{\\smaller $n=', length(x), '$}', sep='')

nFm <- function(x, left, right, neg=FALSE, pad=FALSE, html=FALSE) {
  tot <- if(right == 0) left + neg else left + right + neg + 1
  fmt <- paste('%', tot, '.', right, 'f', sep='')
  x <- sprintf(fmt, x)
  if(pad) x <- gsub(' ', if(html) '' else '~', x)
  x
}

table_freq <- function(x) {
  if(!length(x) || all(is.na(x))) return('')
  if (!requireNamespace("tables", quietly = TRUE))
    stop("This function requires the 'tables' package.")
  w   <- table(x)
  den <- sum(w)
  to <- tables::table_options()
  showfreq <- to$showfreq
  if(!length(showfreq)) showfreq <- 'all'
  pctdec <- to$pctdec
  if(!length(pctdec)) pctdec <- 0
  
  i <- switch(showfreq,
              all  = 1:length(w),
              high = which(w == max(w)),
              low  = which(w == min(w)))
  m <- w[i]
  fpct <- table_formatpct(m, den)
  if(showfreq == 'all') {
    z <- paste(names(m), '\\hfill', fpct, sep='')
    z <- paste(z, collapse='\\\\', sep='')
    len <- max(nchar(names(m))) + 9 + pctdec + 1 * (pctdec > 0)
    z <- paste('\\parbox{', len, 'ex}{\\smaller ', z, '}', sep='')
    return(z)
  }
  lab <- paste(names(m), collapse=', ')
  num <- m[1]
  paste(lab, ':', table_formatpct(num, den), sep='')
}

table_pc <- function(x, y) {
  maxn   <- max(length(x), length(y))
  maxdig <- 1L + floor(log10(maxn))
  num <- if(all(is.na(x))) length(x) else
    if(is.logical(x)) sum(x) else sum(x %in% c('yes','Yes'))
  den <- if(all(is.na(y))) length(y) else sum(!is.na(y))
  prn(c(num,den)); prn(table(x, exclude=NULL)); prn(table(y, exclude=NULL))
  table_formatpct(num, den)
}

table_formatpct <- function(num, den) {
  if(den == 0 | all(is.na(num + den))) return('')
  if (!requireNamespace("tables", quietly = TRUE))
    stop("This function requires the 'tables' package.")
  to     <- tables::table_options()
  npct   <- to$npct
  pctdec <- to$pctdec
  if(!length(pctdec)) pctdec <- 0
  if(!length(npct))   npct <- 'both'
  poss <- c('numerator', 'denominator', 'both', 'none')
  i <- charmatch(npct, poss)
  if(is.na(i)) stop('in table_options(npct=) npct must be "numerator", "denominator", "both", or "none"')
  npct <- poss[i]
  z <- paste(nFm(100 * num / den, 3, pctdec), '\\%', sep='')
  if(npct == 'none') return(z)
  if(npct == 'both')
    return(paste(z, '{\\smaller[2] $\\frac{', num, '}{', den, '}$}', sep=''))
  paste(z, '{\\smaller (', if(npct == 'numerator') num else den, ')}', sep='')
}
  
  
table_latexdefs <- function(file='') {
  ct <- function(...) cat(..., file=file)

  ct('\\makeatletter\n',
     '\\def\\blfootnote{\\xdef\\@thefnmark{}\\@footnotetext}\n',
     '\\makeatother\n')
  ct('\\def\\keytrio{\\blfootnote{Numbers in parentheses are the number of non-missing values.  {\\smaller $a$} \\textbf{$b$}{\\smaller $c$} represents the first quartile $a$, the median $b$, and the third quartile $c$.}}\n')
  ct('\\def\\keytriomsd{\\blfootnote{Numbers in parentheses are the number of non-missing values.  {\\smaller $a$} \\textbf{$b$}{\\smaller $c$} represents the first quartile $a$, the median $b$, and the third quartile $c$.  $x \\pm s$ represents the mean and standard deviation.}}\n')
  
  invisible()
}
