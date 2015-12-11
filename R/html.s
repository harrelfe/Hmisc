html <- function(object, ...) UseMethod('html')

html.latex <- function(object, file, where=c('cwd', 'tmp'),
                       method=c('hevea', 'htlatex'), rmarkdown=FALSE,
                       cleanup=TRUE, ...)
{
  where  <- match.arg(where)
  method <- match.arg(method)
  if(where == 'tmp') cleanup <- FALSE
  if(rmarkdown && ! missing(file))
    warning('do not specify file when rmarkdown=TRUE')
  fi  <- object$file
  fibase <- gsub('\\.tex', '', fi)
  if(missing(file)) file <- paste(fibase, 'html', sep='.')
  if(rmarkdown) file <- character(0)
  toConsole <- ! length(file) || file == ''

  ehtml = function(content) {   # Thanks to Yihui
    if(! requireNamespace('htmltools', quietly=TRUE))
      stop('htmltools package not installed')
    
    content = htmltools::HTML(gsub('^.*?<body\\s*>|</body>.*$', '', content))
    ss  <- paste(fibase, '-enclosed.css', sep='')
    src <- switch(where, cwd=getwd(), tmp=tempdir())
    d = htmltools::htmlDependency(
      'TeX4ht', '1.0.0', src = src, stylesheet = ss)
    htmltools::attachDependencies(content, d)
  }
  
  sty <- object$style
  if(length(sty))
    sty <- paste('\\usepackage{', unique(sty), '}', sep='')

  tmp    <- switch(where,
                   cwd = paste(fibase, 'enclosed', sep='-'),
                   tmp = tempfile())
  tmptex <- paste(tmp, 'tex', sep='.')
  infi   <- readLines(fi)

  cat('\\documentclass{report}', sty,
      if(method == 'hevea') '\\def\\tabularnewline{\\\\}',
      '\\begin{document}', infi,
      '\\end{document}\n', file=tmptex, sep='\n')
  sc <- if(.Platform$OS.type == 'unix') ';' else '&'

  ## Create system call to convert enclosed latex file to html.
  cmd <-
    if(missing(file) || ! length(file) || file == '') 
      paste(optionsCmds(method), shQuote(tmptex))
    else 
      paste(optionsCmds(method), '-o', file, shQuote(tmptex))
    
  ## perform system call
  sys(cmd)

  if(method == 'hevea' && ! toConsole) {
    ## Remove 2 bottom lines added by HeVeA
    infi <- readLines(file)
    i <- grep('<hr style="height:2"><blockquote class="quote"><em>This document was translated from L<sup>A</sup>T<sub>E</sub>X by', infi)
    i <- c(i, grep('</em><a href="http://hevea.inria.fr/index.html"><em>H</em><em><span style="font-size:small"><sup>E</sup></span></em><em>V</em><em><span style="font-size:small"><sup>E</sup></span></em><em>A</em></a><em>.</em></blockquote></body>', infi))
    if(length(i)) {
      infi <- infi[- i]
      writeLines(infi, file)
    }
    if(cleanup) {
      bf <- gsub('\\.html', '', file)
      unlink(c(paste(bf, 'haux', sep='.'),
               paste(bf, 'enclosed.tex', sep='-')))
    }
    return(structure(list(file=file), class='html'))
    
  }

  if(cleanup && method == 'htlatex')
    unlink(paste(tmp, c('tex', 'tmp','idv','lg','4tc','aux','dvi','log',
                        'xref','4ct'), sep='.'))
  if(rmarkdown || toConsole) {
    w <- readLines(paste(tmp, 'html', sep='.'))
    if(rmarkdown) return(ehtml(w))
    if(! length(file)) return(paste(w, collapse='\n'))
    cat(w, sep='\n')
    return(invisible())
  }

  structure(list(file=file), class='html')
}


html.data.frame <-
  function(object,
           file=paste(first.word(deparse(substitute(object))),
                      'html',sep='.'),
           append=FALSE, link=NULL, linkCol=1,
           linkType=c('href','name'), ...)
{
  linkType <- match.arg(linkType)
  
  x   <- as.matrix(object)
  for(i in 1:ncol(x))
    {
      xi <- x[,i]
      if(is.numeric(object[,i]))
        x[,i] <- paste('<div align=right>',xi,'</div>',sep='')
    }
  if(length(r <- dimnames(x)[[1]]))
    x <- cbind(Name=as.character(r), x)
  
  cat('<TABLE BORDER>\n', file=file, append=append)
  cat('<tr>', paste('<td><h3>', dimnames(x)[[2]], '</h3></td>',sep=''), '</tr>\n',
      sep='', file=file, append=file!='')
  
  if(length(link)) {
    if(is.matrix(link)) 
      x[link!=''] <- paste('<a ',linkType,'="', link[link!=''],'">',
                           x[link!=''],'</a>',sep='') else
    x[,linkCol] <- ifelse(link == '',x[,linkCol],
                          paste('<a ',linkType,'="',link,'">',
                                x[,linkCol],'</a>',sep=''))
  }

  for(i in 1:nrow(x))
    cat('<tr>',paste('<td>',x[i,],'</td>',sep=''),'</tr>\n',
        sep='', file=file, append=file!='')

  cat('</TABLE>\n', file=file, append=file!='')
  structure(list(file=file), class='html')
}


html.default <- function(object,
                         file=paste(first.word(deparse(substitute(object))),
                                    'html',sep='.'),
                         append=FALSE,
                         link=NULL, linkCol=1, linkType=c('href','name'),
                         ...)
{
  html.data.frame(object, file=file, append=append, link=link,
                  linkCol=linkCol, linkType=linkType, ...)
}

show.html <- function(object)
{
  browser <- .Options$help.browser
  if(!length(browser))
    browser <- .Options$browser
  
  if(!length(browser))
    browser <- 'netscape'
  
  sys(paste(browser, object, if(.Platform$OS.type == 'unix') '&'))
  invisible()
}

print.html <- function(x, ...) show.html(x)
