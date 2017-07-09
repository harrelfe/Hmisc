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
           header, caption=NULL, rownames=FALSE,
           align='r', align.header='c', bold.header=TRUE,
           col.header='Black', border=2,
           width=NULL, size=100, translate=FALSE,
           append=FALSE, link=NULL, linkCol=1,
           linkType=c('href','name'), ...)
{
  linkType <- match.arg(linkType)
  mu <- markupSpecs$html

  tr <- c(c='center', l='left', r='right')
  align        <- tr[align]
  align.header <- tr[align.header]

  trans <- if(translate) htmlTranslate else function(x) x
  
  x   <- as.matrix(object)
#  for(i in 1:ncol(x)) {
#    xi <- x[,i]
#    if(is.numeric(object[,i]))
#      x[,i] <- paste0('<div align=right>', xi, '</div>')
#  }
  if(rownames && length(r <- rownames(x)))
    x <- cbind(Name=as.character(r), x)

  b <- c('border: 1px solid gray;', 'border-collapse: collapse;')
  ## Give style a name hmisctablexxx where xxx is a random 6-digit integer
  ## because if you reuse the same style in the same document, style
  ## elements will affect tables that preceeded this one
  sn  <- paste0('hmisctable', floor(runif(1, 100000, 999999)))
  psn <- paste0('.', sn)
  
  ## Duplicate specifications because can't get any single one to work
  lua <- length(unique(align))
  sty <- c('<style>',
           paste0(psn, ' {'),
           if(border == 0) 'border: none;' else b,
           paste0('font-size: ', size, '%;'),
           '}',
           paste0(psn, ' td {'),
           if(lua == 1) paste0('text-align: ', align, ';'),
           'padding: 0 1ex 0 1ex;',   ## top left bottom right
           '}',
           paste0(psn, ' th {'),
           paste0('color: ', col.header, ';'),
           paste0('text-align: ', align.header, ';'),
           'padding: 0 1ex 0 1ex;',
           if(bold.header) 'font-weight: bold;' else 'font-weight: normal;',
           '}',
           '</style>')
          
  R <- c(sty, paste0('<table class="', sn, '"',
                     if(length(width) == 1)
                       paste0(' width="', width, '"'),
                     if(border == 1) ' border="0"',
                     if(border == 2) ' border="1"', '>'))

  if(length(caption))
    R <- c(R, paste0('<caption>', mu$lcap(caption), '</caption>'))
  if(missing(header)) header <- colnames(x)
  if(length(header)) {
    head <- trans(header)
    head <- paste0('<th>', head, '</th>')
    head <- paste0('<tr>', paste(head, collapse=''), '</tr>')
    R <- c(R, head)
    }

  if(length(link)) {
    if(is.matrix(link))
      for(j in 1 : ncol(x))
        x[, j] <- ifelse(link[, j] == '', x[, j],
                         paste0('<a ', linkType, '="', link[, j], '">',
                              trans(x[, j]), '</a>'))
#      x[link != ''] <- paste('<a ',linkType,'="', link[link!=''],'">',
#                              trans(x[link != '']), '</a>', sep='')
    else
      x[,linkCol] <- ifelse(link == '', trans(x[, linkCol]),
                            paste0('<a ',linkType, '="', link, '">',
                                   trans(x[, linkCol]), '</a>'))
  }

  for(i in 1 : nrow(x)) {
    rowt <- if(lua == 1) paste0('<td>', x[i, ], '</td>')
            else
              paste0('<td style="text-align:', align, ';">',
                    x[i, ], '</td>')
    R <- c(R, paste0('<tr>', paste(rowt, collapse=''), '</tr>'))
    }

  R <- c(R, '</table>')
  if(is.logical(file) && ! file)
    return(htmltools::HTML(paste0(R, '\n')))

  cat(R, file=file, append=append && file != '', sep='\n')
  structure(list(file=file), class='html')
}


html.default <- function(object,
                         file=paste(first.word(deparse(substitute(object))),
                                    'html', sep='.'),
                         append=FALSE,
                         link=NULL, linkCol=1, linkType=c('href','name'),
                         ...)
  html.data.frame(object, file=file, append=append, link=link,
                  linkCol=linkCol, linkType=linkType, ...)


if(FALSE) {
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
}


htmlVerbatim <- function(..., size = 75, width = 85,
                         scroll=FALSE, rows=10, cols=100,
                         propts=NULL, omit1b=FALSE) {
  if(scroll) {
    nam <- as.character(sys.call()[2])
    w <- paste0('<textarea class="scrollabletextbox" rows=', rows,
                ' cols=', cols, ' style="font-size:', size,
                '%; font-family:Courier New;" name="', nam, '">')
    }
  else w <- paste0('<pre style="font-size:', size, '%;">')
  op <- options(width=width)
  propts <- c(propts, list(quote=FALSE))
  for(x in list(...)) {
    z <- capture.output(do.call('print', c(list(x), propts)))
    if(omit1b && gsub(' ', '', z[1]) == '') z <- z[-1]
    w <- c(w, z)
    }
  options(op)
  w <- c(w, if(scroll) '</textarea>' else '</pre>')
  w <- paste0(w, '\n')
  htmltools::HTML(w)
}

htmlGreek <- function(x, mult=FALSE, unicode=FALSE) {
  orig <- c('alpha','beta','gamma','delta','epsilon','varepsilon',
            'zeta', 'eta',
            'theta','vartheta','iota','kappa','lambda','mu','nu',
            'xi','pi','varpi','rho','varrho','sigma','varsigma','tau',
            'upsilon','phi','chi','psi','omega','Gamma','Delta',
            'Theta','Lambda','Xi','Pi','Sigma','Upsilon','Phi','Psi','Omega')
  
  l <- length(orig)
  
  new <- if(unicode)
           substring('\u3B1\u3B2\u3B3\u3B4\u3B5\u3F5\u3B6\u3B7\u3B8\u3D1\u3B9\u3BA\u3BB\u3BC\u3BD\u3BE\u3C0\u3D6\u3C1\u3F1\u3C3\u3C2\u3C4\u3C5\u3C6\u3C7\u3C8\u3C9\u393\u394\u398\u39B\u39E\u3A0\u3A3\u3A5\u3A6\u3A8\u3A9',
                     1 : l, 1 : l)
         else
           paste0('&#',
         c(945,946,947,948,949,1013,950,951,952,977,953,954,955,956,957,958,
           960,982,961,1009,963,962,964,965,966,967,968,969,915,916,920,923,
           926,928,931,933,934,936,937),
         ';')
  names(new) <- orig

  if(mult) {
    for(j in 1 : l) x <- gsub(paste0('\\<', orig[j], '\\>'), new[j], x)
    return(x)
  }
  if(! all(x %in% orig)) stop(paste0('illegal Greek letter name:', x))
  new[x]
}

htmlSpecial <- function(x, unicode=FALSE) {
  orig <- c('nbsp', 'thinsp', 'emsp', 'ensp', 'plusmn', 'times', 'caret',
            'frasl', 'half')
  
  l <- length(orig)
  
  new <- if(unicode)
           substring('\u00A0\u2009\u2003\u2002\u00B1\u00D7\u005E\u2044\u00BD', 
                     1 : l, 1 : l)
         else
           paste0('&#', c(160,8201,8195,8194,177,215,94,8260,189), ';')
  names(new) <- orig

  if(! all(x %in% orig)) stop(paste0('illegal character name:', x))
  new[x]
}


markupSpecs <- list(html=list(
  ## <span> needed for text in plotly graphics
  bold     = function(x, span=TRUE)
    if(span) paste0('<span style="font-weight:bold">', x, '</span>')
      else   paste0('<strong>', x, '</strong>'),
  italics  = function(x) paste0('<i>', x, '</i>'),
  math     = function(x) paste0('<i>', x, '</i>'),
  code     = function(x) paste0('<code style="font-size:0.8em">', x, '</code>'),
  sup      = function(x, ...) paste0('<sup>', x, '</sup>'),
  sub      = function(x, ...) paste0('<sub>', x, '</sub>'),
  size     = function(x, pct) paste0('<span style="font-size: ', pct,
                                     '%;">', paste(x, collapse=' '),
                                     '</span>'),
  smaller  = function(x) paste0('<span style="font-size: 80%;">', x,
                                '</span>'),
  larger   = function(x) paste0('<span style="font-size: 125%;">', x,
                                '</span>'),
  smaller2 = function(x) paste0('<span style="font-size: 64%;">', x,
                                 '</span>'),
  larger2  = function(x) paste0('<span style="font-size: 156%;">', x,
                                 '</span>'),
  center   = function(x) paste0('<div align=center>', x, '</div>'),
  color    = function(x, col) paste0('<font color="', col, '">', x,
                                     '</font>'),
  subtext  = function(..., color='blue')
    paste0('<br><font size=1 color="', color, '">',
           paste(unlist(list(...)), collapse=' '),
           '</font>'),

  cap      = function(..., symbol='&#8735;') { # figure caption formatting
  ## alternative: symbol='Figure:'; default is right angle
  ## use symbol='&#9638;' for grid graph paper symbol
    lcap <- paste(unlist(list(...)), collapse=' ')
    paste0('<span style="font-family:Verdana;font-size:10px;">', symbol,
           ' </span><span style="font-family:Verdana;font-size:12px;color:MidnightBlue;">',
             lcap, '</span>')
  },
  
  lcap     = function(...) # for continuation of figure caption
    paste0('<span style="font-family:Verdana;font-size:12px;color:MidnightBlue;">',
           paste(unlist(list(...)), collapse=' '), '</span>'),

  tcap      = function(..., symbol='&#9707;') { # table caption formatting
    # alt: symbol='Table:'; default is white square w/vertical bisecting line
    lcap <- paste(unlist(list(...)), collapse=' ')
    paste0('<span style="font-family:Verdana;font-size:10px;">', symbol,
           ' </span><span style="font-family:Verdana;font-size:12px;color:MidnightBlue;">',
             lcap, '</span>')
  },
  
  ltcap     = function(...) # for continuation of table caption
    paste0('<span style="font-family:Verdana;font-size:12px;color:MidnightBlue;">',
           paste(unlist(list(...)), collapse=' '), '</span>'),

  expcoll = function(vis, invis) {
      id <- floor(runif(1, 100000, 999999))  # unique html id
      paste0('<br><a href="#', id, '" id="', id,
             '_earrows" class="earrows" onclick="expand_collapse(\'',
             id, '\');">&#9660;</a>',
             vis, '<span id="', id,
             '" style="display:none;">',
             invis, '</span>')
  },

  uncover = function(before, options, envir) {
    ## https://stackoverflow.com/questions/44866287
    ## usage: knitrSet(lang='markdown')  # issues knit_hooks$set(uncover=uncover)
    ##        <script>
    ##        function uncover(id) {
    ##            var x = document.getElementById(id);
    ##            x.style.display = 'block';
    ##        }
    ##        </script>
    ##
    ##        ```{r, uncover=TRUE, label='text for button', id='script'}
    ##        1 + 1
    ##        ```

        if (before) {
          id <- options$id
          label <- options$label
          if(! length(label)) label <- 'Uncover'
          button_string <- paste0("<button onclick=\"uncover('", 
                                  id, "')\">", label, "</button>")
          div_string <- paste0("<div id = '", id, 
                               "', style = 'display:none'>")
          paste0(button_string, "\n", div_string)
        }
        else {
          "</div>"
        }
  },
  
  session  = function(cite=TRUE, loadedOnly=FALSE) {
    si <- sessionInfo()
    if(! loadedOnly) si$loadedOnly <- NULL
    w <- c('<pre>',
           capture.output(print(si, locale=FALSE)),
           '</pre>',
           if(cite) 'To cite R in publication use:',
           if(cite) capture.output(print(citation(), style='html')))
    w <- paste0(w, '\n')
    htmltools::HTML(w)
  },
  installcsl = function(cslname, rec=FALSE) {
    if(rec) {
      cat('Shows URLs:', 'american-medical-association',
                '',
                'Does not show URLs:', 'council-of-science-editors',
          'american-medical-association-no-url', sep='\n')
      return(invisible())
      }
    if(missing(cslname))
      browseURL('https://www.zotero.org/styles')
    else
      download.file(paste0('https://raw.githubusercontent.com/citation-style-language/styles/master/',
                           cslname, '.csl'), paste0(cslname, '.csl'))
  },
  citeulikeShow = function(user, bibkeys=NULL, tags=NULL, file=NULL) {
    if(length(file)) {
      x <- readLines(file)
      ## See http://stackoverflow.com/questions/8613237
      bibkeys <- unlist(regmatches(x, gregexpr("(?<=\\[@).*?(?=\\])",
                                               x, perl=TRUE)))
      }
    if(length(bibkeys)) {
      keys <- paste(paste0('bibkey%3A+', bibkeys), collapse='+OR+')
      browseURL(paste0('http://www.citeulike.org/search/username?q=',
                       keys, '&search=Search+library&username=', user))
    } else browseURL(paste0('http://www.citeulike.org/user/',
                            user, '/tag/', tags))
    invisible(bibkeys)
    },
  widescreen = function(width='4000px')
    htmltools::HTML(paste0('<style>div.main-container {max-width:',
                           width, ';}</style>')),
  tocsize = function(width = '20%', maxwidth = '260px', maxheight='85%')
    htmltools::HTML(paste0('<style>div.tocify {width: ', width,
                           '; max-width: ', maxwidth, '; max-height: ',
                           maxheight, ';}</style>')),
  scroll   = function(x, size=75, rows=10, cols=100,
                      font='', name='') {
    w <- paste0('<div style="width: ', cols, 'ex; overflow: auto; height: ',
                rows, 'ex;">')
    c(w, x, '</div>')
    },
  chisq    = function(x, ...)
#    paste0('\u03C7&<span class="xscript" style="font-size: 75%;"><sup>2</sup><sub>', x,
#           '</sub></span>')
                 if(missing(x)) paste0(htmlGreek('chi'), '<sup>2</sup>')
                 else
                   paste0(htmlGreek('chi'), markupSpecs$html$subsup(x, '2')),
  fstat    = function(x, ...) paste0('<i>F</i><sub><span style="font-size: 80%;">',
                                     x[1], htmlSpecial('thinsp'),
                                     x[2], '</span></sub>'),
  frac     = function(a, b, size=82, ...)
    paste0('<span style="font-size: ', size, '%;"><sup>',
           a, '</sup>', htmlSpecial('frasl'), '<sub>', b, '</sub></span>'),
  half     = function(...) htmlSpecial('half'),
  subsup   = function(a, b) paste0("<sup><span style='font-size: 70%;'>", b,
                                   "</span></sup><sub style='position: relative; left: -.47em; bottom: -.4em;'><span style='font-size: 70%;'>",
                                   a, "</span></sub>"),
  varlabel = function(label, units='', size=75, hfill=FALSE) {
    if(units=='') label
    else
      if(hfill) paste0("<div style='float: left; text-align: left;'>", label,
                       "</div><div style='float: right; text-align: right; font-family: Verdana; font-size:", size, "%;'>", units, "</div>")
    else
      paste0(label, htmlSpecial('emsp'),
             "<span style='font-family:Verdana;font-size:", size, "%;'>",
             units, "</span>") },
  rightAlign  = function(x)
    paste0("<div style='float: right; text-align: right;'>",
           x, "</div>"),
  space    = htmlSpecial('nbsp'), 
  lspace   = htmlSpecial('emsp'),
  sspace   = htmlSpecial('thinsp'),
  smallskip= '<br><br>',
  medskip  = '<br><br><br>',
  bigskip  = '<br><br><br><br>',
  lineskip = function(n) paste0('\n<p style="padding-top:', n, 'em;">'),
  br       = '<br>',
  hrule    = '<hr>',
  hrulethin= '<hr class="thinhr">',
  plminus  = htmlSpecial('plusmn'),
  times    = htmlSpecial('times'),
  xbar     = '<span style="text-decoration: overline">X</span>',
  overbar  = function(x) paste0('<span style="text-decoration: overline">',
                                x, '</span>'),
  unicode  = '<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />',
  styles   = function(...) htmltools::HTML('
<script type="text/javascript">
<!--
    function expand_collapse(id) {
       var e = document.getElementById(id);
       var f = document.getElementById(id+"_earrows");
       if(e.style.display == \'none\'){
          e.style.display = \'block\';
          f.innerHTML = \'&#9650\';
       }
       else {
          e.style.display = \'none\';
          f.innerHTML = \'&#9660\';
       }
    }
//-->
</script>
<style>
.earrows {color:silver;font-size:11px;}

fcap {
 font-family: Verdana;
 font-size: 12px;
 color: MidnightBlue
 }

smg {
 font-family: Verdana;
 font-size: 10px;
 color: &#808080;
}

hr.thinhr { margin-top: 0.15em; margin-bottom: 0.15em; }

span.xscript {
position: relative;
}
span.xscript sub {
position: absolute;
left: 0.1em;
bottom: -1ex;
}
</style>
'),

## The following is to create html file so that can copy and paste unicode chars
## To do actual conversions best to use http://www.endmemo.com/unicode/unicodeconverter.php
unicodeshow = function(x, surr=TRUE, append=FALSE) {
  if(surr) x <- paste0('&', x, ';')
  cat('<meta charset="utf-8">', paste(x, collapse=''), '<br>', file='/tmp/z.html', append=append)
}

),

latex = list(
  bold     = function(x) paste0('\\textbf{', x, '}'),
  italics  = function(x) paste0('\\emph{', x, '}'),
  math     = function(x) paste0('$', x, '$'),
  code     = function(x) paste0('\\texttt{\\smaller ', x, '}'),
  sup      = function(x, add='$') paste0(add, '^{',x, '}', add),
  sub      = function(x, add='$') paste0(add, '_{',x, '}', add),
  smaller  = function(x) paste0('{\\smaller ',     x, '}' ),
  larger   = function(x) paste0('{\\smaller[-1]{', x, '}' ),
  smaller2 = function(x) paste0('{\\smaller[2]{',  x, '}' ),
  larger2  = function(x) paste0('{\\smaller[-2]{', x, '}' ),
  center   = function(x) paste0('\\centerline{', x,   '}' ),
  color    = function(x, col) {
    colcmd <- if(col == 'MidnightBlue') '\\textcolor[rgb]{0.1,0.1,0.44}'
              else
                paste0('\\textcolor{', col, '}')
    paste0(colcmd, '{', x, '}')
    },
  chisq    = function(x, add='$') if(missing(x)) paste0(add, '\\chi^{2}', add)
                                  else paste0(add, '\\chi^{2}_{', x,    '}', add),
  fstat    = function(x, add='$')
    paste0(add, 'F_{', x[1], ',', x[2], '}', add),
  frac     = function(a, b, add='$', ...)
    paste0(add, '\\frac{', a, '}{', b, '}', add),
  half     = function(add='$') paste0(add, '\\frac(1}{2}', add),
  subsup   = function(a, b) paste0('$_{', a, '}^{', b, '}$'),
  varlabel = function(label, units='', hfill=FALSE, ...) {
    if(units=='') return(label) else units <- latexTranslate(units)
    fill <- if(hfill) '~\\hfill' else '~~~'
    paste0(label, fill, '\\texttt{\\smaller[2] ',
           gsub('\\*', ' ', units), '}')
    },
  space    = '~',
  lspace   = '~~',
  sspace   = '\\,',
  smallskip= '\\smallskip',
  medskip  = '\\medskip',
  bigskip  = '\\bigskip',
  lineskip = function(n) paste0('\n\\vspace{', n, 'ex}\n\n'),
  br       = '\\\\',
  hrule    = '\\hrule',
  plminus  = '$\\pm$',
  times    = '$\\times$',
  xbar     = '$\\bar{X}$',
  overbar  = function(x) paste0('$\\overline{', x, '}$')
),

plain = list(
  space = ' ',
  lspace = '   ',
  sspace = ' ',
  br     = '\n',
  lineskip = function(n) paste(rep('\n', n), collapse=''),
  hrule  = '',
  code   = function(x) x,
  chisq  = function(x, ...) if(missing(x)) 'chi-square'
                            else
                              paste0('chi-square(', x, ')'),
  frac   = function(a, b, ...) paste0(a, '/', b),
  half   = function(...) '1/2',
  varlabel = function(label, units='', ...)
    if(units == '') label else  paste0(label, '  [', units, ']'),
  times  = 'x',
  color = function(x, ...) x
),

plotmath = list(
  varlabel = function(label, units='', ...)
    labelPlotmath(label, units)
  )
)

## For expand_collapse see http://dickervasti.com/wiki-style-text-expand-collapse-no-jquery.htm#01000

## Function to translate several expressions to html form.
## Arguments inn and out specify additional input and translated
## strings over the usual defaults.

htmlTranslate <- function(object, inn=NULL, out=NULL,
                           greek=FALSE, na='', unicode=FALSE, ...)
{
  text <- ifelse(is.na(object), na, as.character(object))

  ## Must translate & first so won't be converted to &amp; when other
  ## symbols are translated
  inn <- c("&", "|",  "%",  "#",   "<=",     "<",  ">=",     ">",  "_", "\\243",
           "\\$", inn, c("[", "(", "]", ")"))

  w <- if(unicode)
         substring('\u27\u26\u7C\u25\u23\u2264\u3C\u2265\u3E\u5F\uA3\u24',
                   1:11, 1:11)
       else
         c("&#38;", "&#124;", "&#37",   "&#35;", "&#8804;", "&#60;", "&#8805;",
           "&#62;", "&#95;",  "&#164;", "&#36;")

  ## htmlarrows.com
  ##  markupSpecs$html$unicodeshow(out, surr=FALSE)

  out <- c(w, out, c('[', '(', ']', ')'))

  ##See if string contains an ^ - superscript followed by a number

  dig <- c('0','1','2','3','4','5','6','7','8','9')

  for(i in 1 : length(text)) {
    lt <- nchar(text[i])
    x <- substring(text[i], 1 : lt, 1 : lt)
    j <- x == '^'
    if(any(j)) {
      is <- ((1 : lt)[j])[1]  #get first ^
      remain <- x[-(1 : is)]
      k <- remain %in% c(' ',',',')',']','\\','$')
      if(remain[1] %in% dig ||
         (length(remain) > 1 && remain[1] == '-' && remain[2] %in% dig))
        k[-1] <- k[-1] | remain[-1] %nin% dig
      
      ie <- if(any(k)) is + ((1 : length(remain))[k])[1]
        else
          length(x) + 1
      
      substring2(text[i], is, ie - 1) <-
        paste0('BEGINSUP', substring(text[i], is + 1, ie - 1), 'ENDSUP')
    }
    text[i] <- sedit(text[i], c(inn, '^', 'BEGINSUP', 'ENDSUP'),
                     c(out,
                       htmlSpecial('caret', unicode=unicode),
                       '<sup>', '</sup>'), wild.literal=TRUE)

    if(greek) text[i] <- htmlGreek(text[i], unicode=unicode, mult=TRUE)
  }
  text
}


## markupSpecs$html$unicodeshow(c('#9660', 'chi', 'thinsp', 'frasl', 'emsp', 'plusmn', 'times'), append=TRUE)
