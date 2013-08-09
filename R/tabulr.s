tabulr <- function(formula, data=NULL, ...) {
  require(gsubfn) || stop('package gsubfn not installed')
  if(!length(data)) data <- environment(formula)
  else if(is.list(data)) data <- list2env(data, parent=environment(formula))
  f <- as.character(deparse(formula))
  lab <- function(x, hfill=TRUE) {
    x <- gsub('^ +', '', x)
    x <- gsub(' +$', '', x)
    l <- labelLatex(get(x, envir=data), default=x, double=TRUE, hfill=hfill)
    paste("Heading('", l, "')", sep='')
  }
  f <-  gsubfn("label\\((.*?)\\)", ~ lab(x), f)
  f <- gsubfn("labeln\\((.*?)\\)", ~ lab(x,  hfill=FALSE), f)
  ## Translate trio to table_trio
  f <- gsub('trio', 'table_trio', f)
  cat(f, file='/tmp/z')
  f <- as.formula(f)
  tabular(f, data=data, ...)
}

table_trio <- function(x) {
  o <- table_options()
  s <- function(x, default) if(length(x)) x else default
  left  <- s(o$left,  3)
  right <- s(o$right, 1)
  prmsd <- s(o$prmsd, FALSE)
  pn    <- s(o$pn,    FALSE)
  bold  <- s(o$bold,  FALSE)
  
  x <- x[!is.na(x)]
  if(!length(x)) return('')
  qu <- quantile(x, (1:3)/4)
  w <- paste('{\\smaller ', nFm(qu[1], left, right), '} ',
             if(bold) '\\textbf{', nFm(qu[2], left, right), if(bold) '}',
             ' {\\smaller ', nFm(qu[3], left, right), '}', sep='')
  if(prmsd || pn) {
    w <- paste(w, '~{\\smaller (', sep='')
    if(prmsd) w <- paste(w, nFm(mean(x), left, right), '$\\pm$',
                            nFm(sd(x),   left, right), sep='')
    if(pn)    w <- paste(w, if(prmsd)' ', length(x), sep='')
    w <- paste(w,  ')}', sep='')
  }
  w
}

nFm <- function(x, left, right, neg=FALSE, pad=FALSE) {
  tot <- if(right == 0) left + neg else left + right + neg + 1
  fmt <- paste('%', tot, '.', right, 'f', sep='')
  x <- sprintf(fmt, x)
  if(pad) x <- gsub(' ', '~', x)
  x
}

table_pc <- function(x, y) {
  maxn   <- max(length(x), length(y))
  maxdig <- 1L + floor(log10(maxn))
  num <- if(all(is.na(x))) length(x) else
    if(is.logical(x)) sum(x) else sum(x %in% c('yes','Yes'))
  den <- if(all(is.na(y))) length(y) else sum(!is.na(y))
  prn(c(num,den)); prn(table(x, exclude=NULL)); prn(table(y, exclude=NULL))
  paste(nFm(100 * num / den, 3, 1),
        '\\% {\\scriptsize$\\frac{',
        nFm(num, maxdig, 0, pad=TRUE), '}{', 
        nFm(den, maxdig, 0, pad=TRUE), '}$}', sep='')
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
