describe <- function(x, ...) UseMethod("describe")
describe.default <- function(x, descript, ...) {
  if(missing(descript)) {
    descript <- deparse(substitute(x))
  }
  
  if(is.matrix(x)) {
    describe.matrix(x, descript, ...)
  } else {
    describe.vector(x, descript, ...)
  }
}


describe.vector <- function(x, descript, exclude.missing=TRUE, digits=4,
                            listunique=0, listnchar=12,
                            weights=NULL, normwt=FALSE, minlength=NULL, ...)
{
  oldopt <- options('digits')
  options(digits=digits)
  on.exit(options(oldopt))

  weighted <- length(weights) > 0
  if(! weighted) weights <- rep(1, length(x))
  
  special.codes <- attr(x, "special.miss")$codes
  labx <- attr(x,"label")
  
  if(missing(descript)) descript <- as.character(sys.call())[2]

  if(length(labx) && labx != descript) descript <- paste(descript,":",labx)

  un <- attr(x, "units")
  if(length(un) && un == '') un <- NULL

  fmt <- attr(x, 'format')
  if(length(fmt) && (is.function(fmt) || fmt == '')) fmt <- NULL
  
  if(length(fmt) > 1)
    fmt <- paste(as.character(fmt[[1]]), as.character(fmt[[2]]))
  
  present <- if(all(is.na(x))) rep(FALSE, length(x))
  else if(is.character(x)) x != "" & x != " " & ! is.na(x)
  else ! is.na(x)
  
  present <- present & ! is.na(weights)
  
  if(length(weights) != length(x))
    stop('length of weights must equal length of x')

  if(normwt) {
    weights <- sum(present) * weights / sum(weights[present])
    n <- sum(present)
  } else n <- sum(weights[present])

  if(exclude.missing && n==0)
    return(structure(list(), class="describe"))
  
  missing <- sum(weights[! present], na.rm=TRUE)
  atx <- attributes(x)
  atx$names <- atx$dimnames <- atx$dim <- atx$special.miss <- NULL  
  
  atx$class <- atx$class[atx$class != 'special.miss']
  
  isdot <- testDateTime(x,'either') # is date or time var
  isdat <- testDateTime(x,'both')   # is date and time combo var

  x <- x[present, drop=FALSE]
  x.unique <- sort(unique(x))
  weights <- weights[present]

  n.unique <- length(x.unique)
  attributes(x) <- attributes(x.unique) <- atx

  isnum <- (is.numeric(x) || isdat) && ! is.factor(x)
  timeUsed <- isdat && testDateTime(x.unique, 'timeVaries')

  z <- list(descript=descript, units=un, format=fmt)

  counts <- c(n,missing)
  lab <- c("n","missing")

  if(length(special.codes)) {
    tabsc <- table(special.codes)
    counts <- c(counts, tabsc)
    lab <- c(lab, names(tabsc))
  }
  
  if(length(atx$imputed)) {
    counts <- c(counts, length(atx$imputed))
    lab <- c(lab, "imputed")
  }
  
  if(length(pd <- atx$partial.date)) {
    if((nn <- length(pd$month))>0) {
      counts <- c(counts, nn)
      lab <- c(lab, "missing month")
    }
    
    if((nn <- length(pd$day)) > 0) {
      counts <- c(counts, nn)
      lab <- c(lab,"missing day")
    }
    
    if((nn <- length(pd$both)) > 0) {
      counts <- c(counts, nn)
      lab <- c(lab,"missing month,day")
    }
  }

  if(length(atx$substi.source)) {
    tabss <- table(atx$substi.source)
    counts <- c(counts, tabss)
    lab <- c(lab, names(tabss))
  }

  counts <- c(counts, n.unique)
  lab <- c(lab, "distinct")

  if(isnum) {
    xnum <- unclass(x)
    if(n.unique < 2) reff <- 0
    else {
      fp <- wtd.table(xnum, weights, normwt=FALSE, na.rm=FALSE, type='table') /
        sum(weights)
      reff   <- (1 - sum(fp ^ 3)) / (1 - 1 / n / n)
    }
    counts <- c(counts, round(reff, 3))
    lab    <- c(lab, 'Info')
  }
  
  x.binary <- n.unique == 2 && isnum && x.unique[1] == 0 && x.unique[2] == 1
  if(x.binary) {
    counts <- c(counts, sum(weights[x == 1]))
    lab <- c(lab, "Sum")
  }
  
  if(isnum) {
    if(isdot) {
      dd <- sum(weights * xnum)  / sum(weights)
      fval <- formatDateTime(dd, atx, ! timeUsed)
      counts <- c(counts, fval)
    } else counts <- c(counts, format(sum(weights * x) / sum(weights), ...))
    
    lab <- c(lab, "Mean")
    if(! weighted) {
      gmd <- GiniMd(xnum)
      counts <- c(counts, if(isdot) formatDateTime(gmd, atx, ! timeUsed)
                          else
                            format(gmd, ...))
      lab <- c(lab, "Gmd")
    }
  } else if(n.unique == 1) {
    counts <- c(counts, format(x.unique))
    lab <- c(lab, "value")
  }

  if(n.unique >= 10 & isnum) {
    q <-
      if(any(weights != 1)) {
        wtd.quantile(xnum, weights, normwt=FALSE, na.rm=FALSE,
                     probs=c(.05,.1,.25,.5,.75,.90,.95))
      } else quantile(xnum,c(.05,.1,.25,.5,.75,.90,.95), na.rm=FALSE)

    ## Only reason to call quantile is that the two functions can give
    ## different results if there are ties, and users are used to quantile()
    fval <-
      if(isdot) formatDateTime(q, atx, ! timeUsed)
      else format(q,...)
    
    counts <- c(counts, fval)
    lab <- c(lab,".05",".10",".25",".50",".75",".90",".95")
  }
  names(counts) <- lab
  z$counts <- counts

  tableIgnoreCaseWhiteSpace <- function(x) {
    x <- gsub('\r',' ',x)
    x <- gsub('^[[:space:]]+','',gsub('[[:space:]]+$','', x))
    x <- gsub('[[:space:]]+',' ', x)
    y <- tolower(x)
    f <- table(y)
    names(f) <- x[match(names(f), y)]
    f
  }

  values <- NULL
  if(! x.binary) {
    if(inherits(x,'mChoice'))
      z$mChoice <- summary(x, minlength=minlength)
    else
      if(n.unique <= listunique && ! isnum && ! is.factor(x) &&
         max(nchar(x)) > listnchar)
        values <- tableIgnoreCaseWhiteSpace(x)
    else
      if(isnum || n.unique <= 100) {
        if(isnum) {
          if(n.unique >= 100 ||
             min(diff(sort(unique(xnum)))) < diff(range(xnum)) / 500) {
            pret <- pretty(xnum, if(n.unique >= 100) 100 else 500)
            dist <- pret[2] - pret[1]
            r    <- range(pret)
            xnum <- r[1] + dist * round((xnum - r[1]) / dist)
          }
        }
        values <- wtd.table(if(isnum) xnum else if(isdat) format(x) else x,
                            weights, normwt=FALSE, na.rm=FALSE)
        values <- list(value=values$x, frequency=unname(values$sum.of.weights))
      }
    z$values <- values
    
    if(n.unique >= 5) {
      loandhi <- x.unique[c(1 : 5, (n.unique - 4) : n.unique)]
      extremes <-
        if(isdot && (class(loandhi) %nin% 'timeDate')) {
          formatDateTime(unclass(loandhi), at=atx, roundDay=! timeUsed)
        } else if(isnum) loandhi else format(format(loandhi), ...)
      names(extremes) <- c("L1","L2","L3","L4","L5","H5","H4","H3","H2","H1")
      z$extremes <- extremes
      }
  }
  structure(z, class="describe")
}


describe.matrix <- function(x, descript, exclude.missing=TRUE,
                            digits=4, ...)
{
  if(missing(descript))
    descript <- as.character(sys.call())[2]

  nam <- dimnames(x)[[2]]
  if(length(nam)==0)
    stop('matrix does not have column names')

  Z <- vector('list', length(nam))
  names(Z) <- nam

  d <- dim(x)
  missing.vars <- NULL
  for(i in 1:ncol(x)) {
    z <- describe.vector(x[,i],nam[i],exclude.missing=exclude.missing,
                         digits=digits,...)  #13Mar99
    Z[[i]] <- z
    if(exclude.missing && length(z)==0)
      missing.vars <- c(missing.vars,nam[i]) 
  }

  attr(Z, 'descript') <- descript
  attr(Z, 'dimensions') <- d
  attr(Z, 'missing.vars') <- missing.vars
  structure(Z, class="describe")
}


describe.data.frame <- function(x, descript, exclude.missing=TRUE,
                                digits=4, ...)
{
  if(missing(descript))
    descript <- as.character(sys.call())[2]

  nam <- names(x)
  Z <- list()
  nams <- character(0)

  i <- 0
  missing.vars <- NULL
  for(xx in x) {
    mat <- is.matrix(xx)
    i <- i+1
    z <-
      if(mat) 
        describe.matrix(xx,nam[i],exclude.missing=exclude.missing,
                        digits=digits,...)
      else	  
        describe.vector(xx,nam[i],exclude.missing=exclude.missing,
                        digits=digits,...)
    
    all.missing <- length(z)==0
    if(exclude.missing && all.missing)
      missing.vars <- c(missing.vars, nam[i])
    else {
      Z <- c(Z, if(mat) z else list(z))
      nams <- c(nams, if(mat) names(z) else nam[i])
    }
  }
  names(Z) <- nams

  attr(Z, 'descript') <- descript
  attr(Z, 'dimensions') <- dim(x)
  attr(Z, 'missing.vars') <- missing.vars
  structure(Z, class="describe")
}


describe.formula <- function(x, descript, data, subset, na.action, 
                             digits=4, weights, ...)
{
  mf <- match.call(expand.dots=FALSE)
  mf$formula <- x
  mf$x <- mf$descript <- mf$file <- mf$append <- mf$... <- mf$digits <- NULL
  if(missing(na.action))
    mf$na.action <- na.retain
  
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, sys.parent())
  weights <- model.extract(mf, weights)
		
  if(missing(descript)) {
    ter <- attr(mf,"terms")
    d <- as.character(x)
    if(attr(ter,"response")==1) d <- c(d[2],d[1],d[-(1:2)])
    else d <- d[-1]
    d <- paste(d, collapse=" ")
    descript <- d
  }

  Z <- describe.data.frame(mf, descript, digits=digits, weights=weights, ...)
  if(length(z <- attr(mf,"na.action")))
    attr(Z,'naprint') <- naprint(z) 

  Z
}

na.retain <- function(d) d


print.describe <-
  function(x, ...)
{
  at <- attributes(x)
  if(length(at$dimensions)) {
    cat(at$descript,'\n\n',at$dimensions[2],' Variables     ',at$dimensions[1],
        ' Observations\n')
    
    if(length(at$naprint)) cat('\n',at$naprint,'\n')
    w <- paste(rep('-', .Options$width), collapse='')
    cat(w, '\n', sep='')
    for(z in x) {
      if(length(z)==0)
        next
      print.describe.single(z, ...)
      cat(w, '\n', sep='')
    }
    if(length(at$missing.vars)) {
      cat('\nVariables with all observations missing:\n\n')
      print(at$missing.vars, quote=FALSE)
    }
  } else print.describe.single(x, ...)
  
  invisible()
}

## Function to format part of describe.single output after description & counts
## verb=1 means verbatim mode open
formatdescribeSingle <-
  function(x, condense=c('extremes', 'frequencies', 'both', 'none'),
           lang=c('plain', 'latex', 'html'), verb=0, lspace=c(0, 0),
           size=85, ...)
{
  condense <- match.arg(condense)
  lang     <- match.arg(lang)
  wide     <- .Options$width
  specs    <- markupSpecs[[lang]]
  bv       <- function() {
    if(lang == 'latex' && ! verb) '\\begin{verbatim}' else character()
    }
  vs       <- if(lang == 'latex' && lspace[2] != 0)
                function() cat('\\vspace{', -lspace[2], 'ex}\n',
                               sep='') else function() {}

  vbtm <- if(lang == 'html')
            function(x, omit1b=FALSE, ...)
              htmlVerbatim(x, size=size, omit1b=omit1b, ...)
          else
            function(x, omit1b=NULL) capture.output(print(x, quote=FALSE, ...))

  R <- character(0)
  
  v <- x$values
  
  is.standard <- length(v) && is.list(v) &&
                 all(names(v) == c('value', 'frequency'))

  val.wide    <- length(v$value) && sum(nchar(as.character(v$value))) > 200
  val.few     <- length(v$value) && (length(v$value) <= 20)
  print.freq  <- is.standard && val.few && ! val.wide
  print.ext   <- length(x$extremes) && ! print.freq

  if(print.ext) {
    val  <- format(x$extremes)
    w    <- nchar(paste(val, collapse=' '))
    R <- c(R, bv()); verb <- 1
    if(condense %in% c('extremes', 'both')) {
      if(lang == 'html') {
        fsize <- specs$size
        mnb <- function(x) specs$color(x, col='MidnightBlue')
        spc <- specs$space
        blo <- paste0(mnb('lowest'), spc, ':')
        bhi <- paste0(mnb('highest'),     ':')
        if(w + 2 <= wide) {
          low <- paste(blo, paste(val[1: 5], collapse=' '))
          hi  <- paste(bhi, paste(val[6:10], collapse=' '))
          R <- c(R, fsize(paste(low, ', ', hi), size))
        } else {
          low <- data.frame(name=blo, e1=val[1], e2=val[2], e3=val[3],
                            e4=val[4], e5=val[5])
          hi  <- data.frame(name=bhi, e1=val[6], e2=val[7], e3=val[8],
                            e4=val[9], e5=val[10])
          tab <- html(rbind(low, hi, make.row.names=FALSE),
                      align='r',
                      header=NULL, border=0, size=size, file=FALSE)
          R <- c(R, tab)
        }
      }  # end lang='html'
      else {  # lang='plain' or 'latex'
        low <- paste('lowest :', paste(val[1: 5], collapse=' '))
        hi  <- paste('highest:', paste(val[6:10], collapse=' '))
        R <- c(R,
               if(w + 2 <= wide)
                    c('', paste0(low, ', ', hi))
               else c('', low, hi) )
      }
    }   # end condense applicable to extremes
    else
      R <- c(R, if(lang != 'html') '', vbtm(val))
  }

  if(print.freq) {
    R <- c(R, bv()); verb <- 1
    val   <- v$value
    freq  <- v$frequency
    prop <- round(freq / sum(freq), 3)

    ## First try table output, if will fit in no more than 2 sets of 4 lines
    condensed <- TRUE
    if(condense %nin% c('frequencies', 'both')) {
      fval  <- if(is.numeric(val))
                 format(val) else format(val, justify='right')
      ffreq <- format(freq)
      fprop <- format(prop)
      lval  <- nchar(fval[1])
      lfreq <- nchar(ffreq[1])
      lprop <- nchar(fprop[1])
      
      m     <- max(lval, lfreq, lprop)
      ## Right justify entries in each row
      bl    <- '                                         '
      fval  <- paste0(substring(bl, 1, m - lval ), fval)
      ffreq <- paste0(substring(bl, 1, m - lfreq), ffreq)
      fprop <- paste0(substring(bl, 1, m - lprop), fprop)
      
      w <- rbind(Value=fval, Frequency=ffreq, Proportion=fprop)
      colnames(w) <- rep('', ncol(w))
      out <- capture.output(print(w, quote=FALSE))
      if(length(out) <= 8) {
        R <- c(R, vbtm(w, omit1b=TRUE))
        condensed <- FALSE
        }
    }   # end condense frequencies (or both)

    if(condensed) {
      fval  <- as.character(val)
      ffreq <- as.character(freq)
      fprop <- format(prop)
      lval  <- nchar(fval[1])
      lfreq <- nchar(ffreq[1])
      lprop <- nchar(fprop[1])
      w <- paste0(fval, ' (', ffreq, ', ', fprop, ')')
      w <- strwrap(paste(w, collapse=', '), width=wide)
      R <- c(R, '', w)
    }
  } else if(length(v) && ! is.standard)
    R <- c(R, '', vbtm(v))
  
  if(length(x$mChoice)) {
    R <- c(R, bv()); verb <- 1
    R <- c(R, '', vbtm(x$mChoice, prlabel=FALSE))
  }

  if(lang == 'latex' && verb) R <- c(R, '\\end{verbatim}')
  R
}


print.describe.single <-
  function(x, ...)
{
  wide <- .Options$width
  des  <- x$descript
  
  if(length(x$units))
    des <- paste0(des, ' [', x$units, ']')
  
  if(length(x$format))
    des <- paste0(des, '  Format:', x$format)
  
  cat(des,'\n')
  
  print(x$counts, quote=FALSE)

  R <- formatdescribeSingle(x, lang='plain', ...)
  cat(R, sep='\n')
  invisible()
}


'[.describe' <- function(object, i, ...)
{
  at <- attributes(object)
  object <- '['(unclass(object),i)
  structure(object, descript=at$descript,
            dimensions=c(at$dimensions[1], length(object)),
            class='describe')
}


latex.describe <-
  function(object, title=NULL,
           file=paste('describe',
             first.word(expr=attr(object, 'descript')),
             'tex', sep='.'),
           append=FALSE, size='small',
           tabular=TRUE, greek=TRUE, spacing=0.7, lspace=c(0,0), ...)
{
  at <- attributes(object)
  ct <- function(..., file, append=FALSE) {
    if(file=='') cat(...)
    else cat(..., file=file, append=append)
    invisible()
  }

  spc <- if(spacing == 0) '' else
   paste0('\\begin{spacing}{', spacing, '}\n')
  ct(spc, file=file, append=append)
  if(length(at$dimensions)) {
    ct('\\begin{center}\\textbf{', latexTranslate(at$descript), '\\\\',
       at$dimensions[2],'Variables~~~~~',at$dimensions[1],
       '~Observations}\\end{center}\n', file=file, append=TRUE)
    if(length(at$naprint))
      ct(at$naprint,'\\\\\n', file=file, append=TRUE)
    
    ct('\\smallskip\\hrule\\smallskip{\\',size,'\n',
       sep='', file=file, append=TRUE)
    vnames <- at$names
    i <- 0
    for(z in object) {
      i <- i + 1
      if(length(z)==0)
        next

      val <- z$values
      potentiallyLong <-
        length(val) && ! is.matrix(val) &&
           length(val) != 10 || ! all(names(val)==
                   c("L1","L2","L3","L4","L5","H5","H4","H3","H2","H1"))
      dovbox <- TRUE     # was ! potentiallyLong
      if(dovbox) cat('\\vbox{', file=file, append=TRUE)

      latex.describe.single(z, vname=vnames[i],
                            file=file, append=TRUE,
                            tabular=tabular, greek=greek,
                            lspace=lspace, ...)
      ct('\\smallskip\\hrule\\smallskip\n', file=file, append=TRUE)
      if(dovbox) cat('}\n', file=file, append=TRUE)
    }
    
    if(length(mv <- at$missing.vars)) {
      ct('\\smallskip\\noindent Variables with all observations missing:\\ \\smallskip\n',
         file=file, append=TRUE)
      mv <- latexTranslate(mv)
      mv <- paste0('\\texttt{',mv,'}')
      mv <- paste(mv, collapse=', ')
      ct(mv, file=file, append=TRUE)
    }
    spc <- if(spacing == 0) '}\n' else '}\\end{spacing}\n'
    ct(spc, file=file, append=TRUE)
  }
  else {
    val <- object$values
    potentiallyLong <-
      length(val) && ! is.matrix(val) &&
        length(val) != 10 || ! all(names(val)==
                c("L1","L2","L3","L4","L5","H5","H4","H3","H2","H1"))
    dovbox <- TRUE   # was ! potentiallyLong
    if(dovbox) cat('\\vbox{', file=file, append=TRUE)
    latex.describe.single(object,
                          vname=first.word(expr=at$descript),
                          file=file, append=TRUE, size=size,
                          tabular=tabular, lspace=lspace, ...)
    if(dovbox) cat('}\n', file=file, append=TRUE)
    spc <- if(spacing == 0) '\n' else '\\end{spacing}\n'
    ct(spc, file=file, append=TRUE)
  }
  
  structure(list(file=file,  style=c('setspace','relsize')),
            class='latex')
}


latex.describe.single <-
  function(object, title=NULL, vname,
           file, append=FALSE, size='small',
           tabular=TRUE, greek=TRUE, lspace=c(0,0), ...)
{
  ct <- function(..., file, append=FALSE) {
    if(file=='') cat(...)
    else cat(..., file=file, append=append)
    invisible()
  }
  
  oldw <- options('width')
  options(width=if(size == 'small') 95 else 85)
  on.exit(options(oldw))
  
  wide <- switch(size,
                 normalsize = 73,  # was 66
                 small      = 95,  # was 73
                 scriptsize =110,  # was 93
                 73)

  Values <- object$values

  ## Put graph on its own line if length of label > 3.5 inches
  ## For normalsize there are 66 characters per 4.8 in. standard width

  z   <- latexTranslate(object$descript, '&', '\\&', greek=greek)
  ## If any math mode ($ not preceeded by \) don't put label part in bold
  des <- if(! length(grep('[^\\]\\$', z)))
    paste0('\\textbf{', z, '}')
  else {
    ## Get text before : (variable name)
    sp <- strsplit(z, ' : ')[[1]]
    vnm <- sp[1]
    rem <- paste(sp[-1], collapse=':')
    paste0('\\textbf{', vnm, '}: ', rem)
  }
  
  if(length(object$units))
    des <- paste0(des, '{\\smaller[1] [',
                 latexTranslate(object$units),']}')
  
  if(length(object$format))
    des <- paste0(des, '{\\smaller~~Format:', latexTranslate(object$format),
                 '}')
  
  desbas <- paste(object$descript,
                  if(length(object$units))
                  paste0(' [', object$units, ']'),
                  if(length(object$format))
                  paste0('  Format:', object$format))
  
  ct('\\noindent', des, sep='', file=file, append=append)
  lco <- if(length(Values)) length(Values$frequency) else 0
  if(lco > 2) {
    counts <- Values$frequency
    maxcounts <- max(counts)
    ## Scale distinct values to range from 1 : lco
    va <- Values$value
    if(! is.numeric(va)) va <- 1 : lco
    else {
      rang <- range(va)
      va <- 1 + (lco - 1) * (va - rang[1]) / diff(rang)
      }
    ## \mbox{~~~} makes \hfill work
    ct(if(nchar(desbas)/(wide / 4.8) > (4.8 - 1.5))' \\\\ \\mbox{~~~} \n',
       '\\setlength{\\unitlength}{0.001in}\\hfill',
       '\\begin{picture}(1.5,.1)(1500,0)',
       '\\linethickness{0.6pt}\n', sep='', file=file, append=TRUE)
    ## Todo: may need to label limits used since are pretty()'d versions
    for(i in 1 : lco) {
      ct('\\put(',
         round(1000 * (va[i] - 1) * 1.5 / lco),',0){\\line(0,1){',
         max(1, round(1000 * counts[i] / maxcounts * .1)), '}}\n',
         sep='', file=file, append=TRUE)
    }
    
    ct('\\end{picture}\n', file=file, append=TRUE)
  } else ct('\n', file=file, append=TRUE)
  
  sz <- ''
  if(tabular) {
    ml <- nchar(paste(object$counts, collapse='  '))
    if(ml > 90)
      tabular <- FALSE
    else if(ml > 80)
      sz <- '[2]'
  }
  
  ct('\n{\\smaller', sz, '\n', sep='', file=file, append=TRUE)
  if(tabular) {
    if(lspace[1] != 0)
      ct('\\vspace{', -lspace[1], 'ex}\n', sep='', file=file, append=TRUE)
    ct('\\begin{tabular}{',
       paste(rep('r',length(object$counts)),collapse=''),'}\n',
       file=file, append=TRUE)
    ct(paste(latexTranslate(names(object$counts)), collapse='&'), '\\\\\n',
       file=file, append=TRUE)
    ct(paste(latexTranslate(object$counts), collapse='&'), '\\end{tabular}\n',
       file=file, append=TRUE)
  }

  vs <- if(lspace[2] != 0) function() ct('\\vspace{', -lspace[2], 'ex}\n',
                   sep='', file=file, append=TRUE) else function() {}
  if(file != '')
    sink(file, append=TRUE)

  verb <- 0
  if(! tabular) {
    vs()
    cat('\\begin{verbatim}\n'); verb <- 1
    print(object$counts, quote=FALSE)
  }

  R <- formatdescribeSingle(object, lang='latex', verb=verb,
                            lspace=lspace, ...)
  cat(R, sep='\n')
  cat('}\n')  ## ends \smaller
  if(file != '') sink()
  invisible()
}

html.describe <-
  function(object, size=85,
           tabular=TRUE, greek=TRUE, scroll=FALSE, rows=25, cols=100, ...)
{
  at <- attributes(object)

  m <- markupSpecs$html
  center <- m$center
  bold   <- m$bold
  code   <- m$code
  br     <- m$br
  lspace <- m$lspace
  sskip  <- m$smallskip
  hrule  <- m$hrulethin
  fsize  <- m$size
  mnb    <- function(x) m$color(x, 'MidnightBlue')

  R <- c(m$unicode, m$style())   ## define thinhr (and others not needed here)
  
  if(length(at$dimensions)) {
    R <- c(R,
           mnb(center(bold(paste(htmlTranslate(at$descript), sskip,
                                 at$dimensions[2], ' Variables', lspace,
                                 at$dimensions[1],' Observations')))))
    
    if(length(at$naprint)) R <- c(R, '', at$naprint)
    
    R <- c(R, hrule)
    
    vnames <- at$names
    i <- 0
    for(z in object) {
      i <- i + 1
      if(! length(z))
        next
      
      r <- html.describe.single(z, ## vname=vnames[i],
                                tabular=tabular, greek=greek, size=size, ...)
      R <- c(R, r, hrule)
    }
    
    if(length(mv <- at$missing.vars)) {
      R <- c(R, sskip, 'Variables with all observations missing:',
             br, sskip)
      mv <- paste(code(htmlTranslate(mv)), collapse=', ')
      R <- c(R, mv)
    }

    if(scroll) R <- m$scroll(R, size=size, rows=rows, cols=cols,
                             name=at$descript)
  }
  else
    R <- c(R, html.describe.single(object, tabular=tabular,
                                   greek=greek, size=size, ...))
  
  htmltools::HTML(R)
}

html.describe.single <-
  function(object, size=85,
           tabular=TRUE, greek=TRUE, ...)
{
  m <- markupSpecs$html
  center <- m$center
  bold   <- m$bold
  code   <- m$code
  br     <- m$br
  lspace <- m$lspace
  sskip  <- m$smallskip
  fsize  <- m$size
  smaller<- m$smaller

  pngfile <- paste(tempdir(), 'needle1234567890a.png', sep='/')

  oldw <- options('width')
  options(width=if(size < 90) 95 else 85)
  on.exit(options(oldw))
  
  wide <- if(size >= 90) 73 else if(size >= 75) 95 else 110

  z   <- htmlTranslate(object$descript, greek=greek)
  des <- if(! length(grep(':', z))) bold(z)
    else {
      ## Get text before : (variable name)
      sp <- strsplit(z, ' : ')[[1]]
      vnm <- sp[1]
      rem <- paste(sp[-1], collapse=':')
      paste0(bold(vnm), ': ', rem)
    }
  
  if(length(object$units))
    des <- m$varlabel(des, htmlTranslate(object$units))
  
  if(length(object$format))
    des <- paste0(des, lspace,
                  smaller(paste0('Format:',
                                 htmlTranslate(object$format))))

  Values <- object$values
  lco <- if(length(Values)) length(Values$frequency) else 0
  if(lco > 2) {
    counts <- Values$frequency
    maxcounts <- max(counts)
    counts <- counts / maxcounts
    ## Scale distinct values to range from 1 : lco
    va <- Values$value
    if(! is.numeric(va)) va <- 1 : lco
    else {
      rang <- range(va)
      va <- 1 + (lco - 1) * (va - rang[1]) / diff(rang)
      }
    w <- if(lco >= 50) 150 / lco else 3
    des <- paste0(des,
                  m$rightAlign(tobase64image(pngNeedle(counts,
                                                       x=va, w=w, h=13, lwd=2,
                                                       file=pngfile))))
  }

  R <- des
  
  sz <- size
  if(tabular) {
    ml <- nchar(paste(object$counts, collapse='  '))
    if(ml > 90)
      tabular <- FALSE
    else if(ml > 80)
      sz <- round(0.875 * size)
  }

  if(tabular) {
    d <- as.data.frame(as.list(object$counts))
    colnames(d) <- names(object$counts)
    tab <- html(d, file=FALSE, align='c',
                align.header='c', bold.header=FALSE,
                col.header='MidnightBlue', border=0,
                translate=TRUE, size=sz)
    R <- c(R, tab)
  }
  else
    R <- c(R, htmlVerbatim(object$counts, size=sz))

  
  R <- c(R, formatdescribeSingle(object, lang='html', ...))
  R
}


dataDensityString <- function(x, nint=30)
{
  x <- as.numeric(x)
  x <- x[! is.na(x)]
  if(length(x) < 2) return('')
  r <- range(x)
  x <- floor(nint * (x-r[1])/(r[2]-r[1]))
  x <- pmin(tabulate(x), 37)
  paste0(format(r[1]),' <',
        paste(substring(' 1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
                        x+1,x+1), collapse=''),
        '> ',format(r[2]))
}



contents <- function(object, ...) UseMethod('contents')

contents.data.frame <- function(object, sortlevels=FALSE,
                                id=NULL, range=NULL, values=NULL, ...)
{
  dfname <- deparse(substitute(object))
  nam <- names(object)
  d <- dim(object)
  n <- length(nam)
  fl <- nas <- integer(n)
  cl <- sm <- lab <- un <- longlab <- character(n)
  Lev <- list()
  for(i in 1:n) {
    x <- object[[i]]
    at <- attributes(x)
    if(length(at$label))     lab[i]     <- at$label
    if(length(at$longlabel)) longlab[i] <- at$longlabel
    
    if(length(at$units))     un[i] <- at$units
    
    atl <- at$levels
    fl[i] <- length(atl)
    cli <- at$class[at$class %nin% c('labelled', 'factor')]
    if(length(cli)) cl[i] <- cli[1]
    
    sm[i] <- storage.mode(x)
    nas[i] <- sum(is.na(x))
    if(length(atl)) {
      if(sortlevels) atl <- sort(atl)
      if(length(Lev)) for(j in 1 : length(Lev)) {
        w <- Lev[[j]]
        if(! is.name(w) && is.logical(all.equal(w, atl))) {
          atl <- as.name(names(Lev)[j])
          break   
        }
      }
      Lev[[nam[i]]] <- atl
    }
  }
  
  w <- list(Labels = if(any(lab != '')) lab,
            Units  = if(any(un != ''))  un,
            Levels = if(any(fl > 0))    fl,
            Class  = if(any(cl != ''))  cl,
            Storage=                    sm,
            NAs    = if(any(nas > 0))   nas )
  
  w <- w[sapply(w, function(x)length(x) > 0)]
  
  ## R does not remove NULL elements from a list
  structure(list(contents=data.frame(w, row.names=nam),
                 dim=d, maxnas=max(nas),
                 id=id, rangevar=range, valuesvar=values,
                 unique.ids = if(length(id) && id %in% nam)
                                length(unique(object[[id]])),
                 range = if(length(range) && range %in% nam)
                               paste(as.character(range(object[[range]], na.rm=TRUE)),
                                     collapse='-'),
                 values = if(length(values) && values %in% nam)
                            paste(if(is.factor(object[[values]])) levels(object[[values]])
                                     else sort(unique(object[[values]])), collapse=' '),
                 dfname=dfname,
                 Levels=Lev,
                 longLabels=if(any(longlab != ''))
                              structure(longlab, names=nam)),
            class='contents.data.frame')
}


print.contents.data.frame <-
  function(x, sort=c('none','names','labels','NAs'),
           prlevels=TRUE, maxlevels=Inf, number=FALSE, ...)
{
  sort <- match.arg(sort)
  d <- x$dim
  maxnas <- x$maxnas
  cat('\nData frame:', x$dfname, '\t', d[1],' observations and ', d[2],
      ' variables    Maximum # NAs:', maxnas, '\n', sep='')
  if(length(x$id)) cat('Distinct ', x$id, ':', x$unique.ids, '\t', sep='')
  if(length(x$rangevar)) cat(x$rangevar, ' range:', x$range, '\t', sep='')
  if(length(x$valuesvar))cat(x$valuesvar, ':', x$values, sep='')
  cat('\n\n')
  cont <- x$contents
  nam <- row.names(cont)
  if(number) row.names(cont) <- paste(format(1:d[2]), row.names(cont))

  switch(sort,
         names={
           cont <- cont[order(nam),,drop=FALSE]
         },
         labels={
           if(length(cont$Labels)) 
             cont <-  cont[order(cont$Labels, nam),, drop=FALSE]
         },
         NAs={
           if(maxnas > 0)
             cont <- cont[order(cont$NAs, nam),, drop=FALSE]
         })

  if(length(cont$Levels))
    cont$Levels <- ifelse(cont$Levels == 0, '', format(cont$Levels))

  print(cont)

  if(prlevels && length(L <- x$Levels)) {
    cat('\n')
    nam <- names(L)
    w <- .Options$width - max(nchar(nam)) - 5
    reusingLevels <- sapply(L, is.name)
    fullLevels    <- which(! reusingLevels)
    namf <- lin <- names(L[fullLevels])
    ## separate multiple lines per var with \n for print.char.matrix
    j <- 0
    for(i in fullLevels) {
      j <- j + 1
      varsUsingSame <- NULL
      if(sum(reusingLevels)) {
        for(k in which(reusingLevels))
          if(L[[k]] == namf[j]) varsUsingSame <- c(varsUsingSame, nam[k])
        if(length(varsUsingSame))
          namf[j] <- paste(c(namf[j], varsUsingSame), collapse='\n')
      }
      Li <- L[[i]]
      if(length(Li) > maxlevels) Li <- c(Li[1 : maxlevels], '...')
      lin[j] <- paste(pasteFit(Li, width=w), collapse='\n')
    }
    z <- cbind(Variable=namf, Levels=lin)
    print.char.matrix(z, col.txt.align='left', col.name.align='left',
                      row.names=TRUE, col.names=TRUE)
  }
  
  longlab <- x$longLabels
  if(length(longlab)) {
    if(existsFunction('strwrap'))
      for(i in 1:length(longlab)) {
        if(longlab[i] != '')
          longlab[i] <- paste(strwrap(longlab[i],width=.85*.Options$width ),
                              collapse='\n')
      }
    i <- longlab != ''
    nam <- names(longlab)
    z <- cbind(Variable=nam[i], 'Long Label'=longlab[i])
    print.char.matrix(z, col.names=TRUE, row.names=FALSE,
                      cell.align='left')
  }
  
  invisible()
}


html.contents.data.frame <-
  function(object, sort=c('none', 'names', 'labels', 'NAs'), prlevels=TRUE,
           maxlevels=Inf,
           levelType=c('list', 'table'),
           number=FALSE, nshow=TRUE, ...)
{
  sort <- match.arg(sort)
  levelType <- match.arg(levelType)
  mu <- markupSpecs$html
  lspace <- mu$lspace
  hrule  <- mu$hrule
  
  d      <- object$dim
  maxnas <- object$maxnas

  if(nshow) {
    R <- paste0(hrule, '<h4>Data frame:', object$dfname,
                '</h4>', d[1],
                ' observations and ', d[2],
                ' variables, maximum # NAs:',maxnas, lspace, lspace)

    if(length(object$id))
      R <- paste0(R, 'Distinct ', object$id, ':', object$unique.ids,
                  lspace, lspace)
    if(length(object$rangevar))
      R <- paste0(R, object$rangevar, ' range:', object$range,
                  lspace, lspace)
    if(length(object$valuesvar))
      R <- paste0(R, object$valuesvar, ':', object$values,
                  lspace, lspace)
    R <- c(R, hrule)
    
  } else
    R <- paste0(hrule, '<h4>Data frame:', object$dfname,
        '</h4>', ' Variables:', d[2], hrule)
  
  cont <- object$contents
  nam <- row.names(cont)
  if(number) {
    rn <- paste(format(1:d[2]), row.names(cont))
    rn <- sedit(rn, ' ', '&#XA0;&#XA0;')
      row.names(cont) <- rn
  }

  switch(sort,
         names={cont <- cont[order(nam),,drop=FALSE]},
         labels={
           if(length(cont$Labels)) 
             cont <-  cont[order(cont$Labels, nam),,drop=FALSE]
         },
         NAs={
           if(maxnas>0) cont <- cont[order(cont$NAs,nam),,drop=FALSE]
         })
  
  link <- matrix('', nrow=nrow(cont), ncol=1+ncol(cont),
                 dimnames=list(dimnames(cont)[[1]], c('Name', dimnames(cont)[[2]])))
  
  longlab <- object$longLabels
  if(length(longlab)) {
    longlab <- longlab[longlab != '']
    link[names(longlab),'Name'] <- paste('#longlab',names(longlab),sep='.')
  }
  
  L <- object$Levels
  Lnames <- names(L)
  if(length(cont$Levels)) {
    cont$Levels <- ifelse(cont$Levels==0, '', format(cont$Levels))
    namUsed     <- sapply(L, function(z) if(is.name(z)) as.character(z) else '')
    reusingLevels <- namUsed != ''
    fullLevels  <- which(! reusingLevels)
    namUsed     <- ifelse(reusingLevels, namUsed, Lnames)
    names(namUsed) <- Lnames
    link[,'Levels'] <- ifelse(cont$Levels=='', '', paste('#levels',namUsed[nam],sep='.'))
  }
  adj <- rep('l', length(cont))
  adj[names(cont) %in% c('NAs','Levels')] <- 'r'
  if(! nshow) {
    cont$NAs <- NULL
    link <- link[, colnames(link) != 'NAs', drop=FALSE]
    adj <- adj[names(adj) != 'NAs']
  }
  out <- html(cont, file=FALSE, rownames=TRUE,
              link=link, border=2,
              col.just=adj, ...)
  R <- c(R, as.character(out), hrule)
    
  if(prlevels && length(L) > 0) {
    if(levelType=='list') {
      R <- c(R, '<h5>Category Levels</h5>')
      for(i in fullLevels) {
        l <- L[[i]]
        nami <- Lnames[i]
        w <- nami
        if(sum(reusingLevels))
          for(k in which(reusingLevels))
            if(L[[k]] == nami) w <- c(w, Lnames[k])
        R <- c(R, paste0('<a name="levels.', nami, '"><h6>',
                         paste(w, collapse=', '), '</h6>'))
        if(length(l) > maxlevels) l <- c(l[1 : maxlevels], '...')
        for(k in l) R <- c(R,  paste0('<li>', k, '</li>\n'))
      }
    }
    else {  
      ## Function to split a character vector x as evenly as
      ## possible into n elements, pasting multiple elements
      ## together when needed
      evenSplit <- function(x, n) {
        indent <- function(z) if(length(z) == 1) z else
        c(z[1], paste0('&emsp;', z[-1]))
        m <- length(x)
        if(m <= n) return(c(indent(x), rep('',n-m)))
        totalLength <- sum(nchar(x)) + (m-1)*3.5
        ## add indent, comma, space
        lineLength  <- ceiling(totalLength/n)
        y <- pasteFit(x, sep=', ', width=lineLength)
        m <- length(y)
        if(m > n) for(j in 1:10) {
          lineLength <- round(lineLength*1.1)
          y <- pasteFit(x, sep=', ', width=lineLength)
          m <- length(y)
          if(m <= n) break
        }
        ## Take evasive action if needed
        if(m == n) indent(y) else if(m < n)
          c(indent(y), rep('', n - m)) else 
        c(paste(x, collapse=', '), rep('', n - 1))
      }
      nam <- names(L)
      v <- lab <- lev <- character(0)
      j <- 0
      for(i in fullLevels) {
        j <- j + 1
        l <- L[[i]]
        if(length(l) > maxlevels) l <- c(l[1 : maxlevels], '...')
        nami <- nam[i]
        v <- c(v, nami)
        w <- nami
        if(sum(reusingLevels))
          for(k in which(reusingLevels)) if(L[[k]] == nam[i]) w <- c(w, nam[k])
        lab <- c(lab, evenSplit(w, length(l)))
        lev <- c(lev, l)
      }
      z <- cbind(Variable=lab, Levels=lev)
      out <- html(z, file=FALSE,
                  link=ifelse(lab=='','',paste('levels',v,sep='.')),
                  linkCol='Variable', linkType='name', border=2,...)
      R <- c(R, as.character(out), hrule)
    }
  }
  
  i <- longlab != ''
  if(any(i)) {
    nam <- names(longlab)[i]
    names(longlab) <- NULL
    lab <- paste('longlab', nam, sep='.')
    z <- cbind(Variable=nam, 'Long Label'=longlab[i])
    out <- html(z, file=FALSE,
                link=lab, linkCol='Variable', linkType='name', ...)
    R <- c(R, as.character(out), hrule)
  }
  htmltools::HTML(paste0(R, '\n'))
}


contents.list <- function(object, dslabels=NULL, ...) {
  nam <- names(object)
  if(length(dslabels)) {
    dslabels <- dslabels[nam]
    names(dslabels) <- NULL
  }
  
  g <- function(w) {
    if(length(w)==0 || is.null(w))
      c(Obs=0, Var=if(is.null(w))
        NA
      else
        length(w),
        Var.NA=NA)
    else
      c(Obs=length(w[[1]]), Var=length(w),
        Var.NA=sum(sapply(w, function(x) sum(is.present(x))==0)))
  }
  
  v <- t(sapply(object, g))
  structure(list(contents=if(length(dslabels))
                 data.frame(Label=dslabels,Obs=v[,'Obs'],
                            Var=v[,'Var'],Var.NA=v[,'Var.NA'],
                            row.names=nam)
  else
                 data.frame(Obs=v[,'Obs'],Var=v[,'Var'],
                            Var.NA=v[,'Var.NA'], row.names=nam)),
            class='contents.list')
}


print.contents.list <-
  function(x, sort=c('none','names','labels','NAs','vars'), ...)
{
  sort <- match.arg(sort)
  cont <- x$contents
  nam <- row.names(cont)

  cont <- cont[
               switch(sort,
                      none=1:length(nam),
                      names=order(nam),
                      vars=order(cont$Var),
                      labels=order(cont$Label, nam),
                      NAs=order(cont$Var.NA,nam)),]
  
  print(cont)
  invisible()
}
