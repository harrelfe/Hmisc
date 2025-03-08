##"label<-"  <- function(x, value) {
##  attr(x, "label") <- value
##  x
##}
## exact=TRUE and [['label']] to prevent matching with a different attribute
## "labels" (Thanks: Ivan Puzek)

label <- function(x, default=NULL, ...) UseMethod("label")

label.default <- function(x, default=NULL, units=plot, plot=FALSE,
                          grid=FALSE, html=FALSE, ...)
{
  if(length(default) > 1)
    stop("the default string cannot be of length greater then one")

  at <- attributes(x)
  lab <- at[['label']]
  if(length(default) && (!length(lab) || lab==''))
    lab <- default

  un  <- at$units
  labelPlotmath(lab,
                if(units) un else NULL,
                plotmath=plot, grid=grid, html=html)
}

label.Surv <- function(x, default=NULL, units=plot,
                       plot=FALSE, grid=FALSE, html=FALSE,
                       type=c('any', 'time', 'event'), ...)
{
  type <- match.arg(type)

  if(length(default) > 1)
    stop("the default string cannot be of length greater then one")

  at  <- attributes(x)
  lab <- at[['label']]
  ia  <- at$inputAttributes
  if((! length(lab) || lab == '') && length(ia)) {
    poss <- switch(type,
                   any   = c(ia$event$label, ia$time2$label, ia$time$label),
                   time  = c(                ia$time2$label, ia$time$label),
                   event =   ia$event$label )
    for(lb in poss)
      if(! length(lab) && lb != '') lab <- lb
  }

  if(length(default) && (!length(lab) || lab=='')) lab <- default

  un  <- NULL
  if(units) {
    un <- at$units
    if(! length(un) && length(ia)) {
      un <- ia$time2$units
      if(! length(un)) un <- ia$time$units
    }
  }

  labelPlotmath(lab, un,
                plotmath=plot, grid=grid, html=html)
}



label.data.frame <- function(x, default=NULL, self=FALSE, ...) {
  if(self) {
    label.default(x)
  } else {
    if(length(default) > 0 && length(default) != length(x)) {
      stop('length of default must same as x')
    } else if(length(default) == 0) {
      default <- list(default)
    }

    labels <- mapply(FUN=label, x=x, default=default,
                     MoreArgs=list(self=TRUE), USE.NAMES=FALSE)
    names(labels) <- names(x)
    return(labels)
  }
}

labelPlotmath <- function(label, units=NULL, plotmath=TRUE, html=FALSE,
                          grid=FALSE, chexpr=FALSE)
{
  if(! length(label)) label <- ''

  if(! length(units) || (length(units) == 1 && is.na(units))) units <- ''

  if(html)       return(markupSpecs$html$varlabel (label, units))
  if(! plotmath) return(markupSpecs$plain$varlabel(label, units))
  
  g <-
    function(x, y=NULL, xstyle=NULL, ystyle=NULL)
      {
        h <- function(w, style=NULL)
          if(length(style)) sprintf('%s(%s)', style, w) else w

        tryparse <- function(z, original, chexpr) {
          p <- try(parse(text=z), silent=TRUE)
          if(is.character(p)) original else
             if(chexpr) sprintf('expression(%s)', z) else p
        }
        if(! length(y))
          return(tryparse(h(plotmathTranslate(x), xstyle), x, chexpr))

        w <- paste('list(',h(plotmathTranslate(x), xstyle), ',',
                   h(plotmathTranslate(y), ystyle), ')', sep='')
        tryparse(w, paste(x, y), chexpr)
      }
  
  if(units=='') g(label)
  else
    if(label=='') g(units)
  else g(label, units, ystyle='scriptstyle')
}


plotmathTranslate <- function(x)
{
  if(length(grep('paste', x))) return(x)

  specials <- c(' ','%','_')
  spec <- FALSE
  for(s in specials)
    if(length(grep(s,x)))
      spec <- TRUE
  ## If x is not a legal expression, also put in paste()
  if(! spec && is.character(try(parse(text=x), silent=TRUE)))
    spec <- TRUE

  if(spec) x <- paste('paste("',x,'")',sep='')
  else if(substring(x,1,1)=='/') x <- paste('phantom()', x, sep='')
  x
}

labelLatex <- function(x=NULL, label='', units='', size='smaller[2]',
                       hfill=FALSE, bold=FALSE, default='', double=FALSE) {
  if(length(x)) {
    if(label == '') label <- label(x)
    if(units == '') units <- units(x)
  }
  if(default == '' && length(x)) default <- deparse(substitute(x))
  if(label == '') return(default)

  label <- latexTranslate(label)
  bs <- if(double) '\\\\' else '\\'
  if(bold) label <- paste('{', bs, 'textbf ', label, '}', sep='')
  if(units != '') {
    units <- latexTranslate(units)
    if(length(size) && size != '')
      units <- paste('{', bs, size, ' ', units, '}', sep='')
    if(hfill) units <- paste(bs, 'hfill ', units, sep='')
    else
      units <- paste(' ', units, sep='')
    label <- paste(label, units, sep='')
  }
  label
}

"label<-" <- function(x, ..., value) UseMethod("label<-")

##From Bill Dunlap, StatSci  15Mar95:
"label<-.default" <- function(x, ..., value)
{
  if(is.list(value)) {
    stop("cannot assign a list to be a object label")
  }

  if(length(value) != 1L) {
    stop("value must be character vector of length 1")
  }

  attr(x, 'label') <- value

  if(! inherits(x, 'labelled')) class(x) <- c('labelled', class(x))
  return(x)
}

"label<-.data.frame" <- function(x, self=TRUE, ..., value) {
  if(!is.data.frame(x)) {
    stop("x must be a data.frame")
  }

  if(missing(self) && is.list(value)) {
    self <- FALSE
  }

  if(self) {
    xc <- class(x)
    xx <- unclass(x)
    label(xx) <- value
    class(xx) <- xc
    return(xx)
  } else {
    if(length(value) != length(x)) {
      stop("value must have the same length as x")
    }

    for (i in seq(along.with=x)) {
      label(x[[i]]) <- value[[i]]
    }
  }

  return(x)
}

"[.labelled"<- function(x, ...) {
  tags <- valueTags(x)
  x <- NextMethod("[")
  valueTags(x) <- tags
  x
}

"print.labelled"<- function(x, ...) {
  x.orig <- x
  u <- attr(x, 'units', exact=TRUE)
  if(length(u))
    attr(x,'units') <- NULL   # so won't print twice

  cat(attr(x, "label", exact=TRUE),
      if(length(u))
        paste('[', paste0(u, 's'), ']', sep=''),
      "\n")

  attr(x, "label") <- NULL
  class(x) <-
    if(length(class(x)) == 1 && inherits(x, 'labelled'))
      NULL
    else
      class(x)[class(x) != 'labelled']

  ## next line works around print bug
  if(! length(attr(x, 'class')))
    attr(x, 'class') <- NULL

  NextMethod("print")
  invisible(x.orig)
}


as.data.frame.labelled <- as.data.frame.vector

Label <- function(object, ...) UseMethod("Label")


Label.data.frame <- function(object, file='', append=FALSE, ...)
{
  nn <- names(object)
  for(i in 1:length(nn)) {
    lab <- attr(object[[nn[i]]], 'label', exact=TRUE)
    lab <- if(length(lab)==0) '' else lab
    cat("label(",nn[i],")\t<- '",lab,"'\n",
        append=if(i==1)
        append
        else
        TRUE,
        file=file, sep='')
  }

  invisible()
}

relevel.labelled <- function(x, ...) {
  lab <- label(x)
  x <- NextMethod(x)
  label(x) <- lab
  x
}

reLabelled <- function(object)
{
  for(i in 1:length(object))
    {
      x <- object[[i]]
      lab <- attr(x, 'label', exact=TRUE)
      cl  <- class(x)
      if(length(lab) && ! any(cl == 'labelled')) {
        class(x) <- c('labelled',cl)
        object[[i]] <- x
      }
    }

  object
}


llist <- function(..., labels=TRUE)
{
  dotlist <- list(...)
  lname <- names(dotlist)
  name <- vname <- as.character(sys.call())[-1]
  for(i in 1:length(dotlist))
    {
      vname[i] <-
        if(length(lname) && lname[i]!='')
          lname[i]
        else
          name[i]

      ## R barked at setting vname[i] to NULL
      lab <- vname[i]
      if(labels && ! is.null(dotlist[[i]]))
        {
          lab <- attr(dotlist[[i]],'label', exact=TRUE)
          if(length(lab) == 0) lab <- vname[i]
          label(dotlist[[i]]) <- lab
        }
    }

  names(dotlist) <- vname[1:length(dotlist)]
  dotlist
}

prList <- function(x, lcap=NULL, htmlfig=0, after=FALSE) {
  if(! length(names(x))) stop('x must have names')
  if(length(lcap) && (length(lcap) != length(x)))
    stop('if given, lcap must have same length as x')
  mu <- markupSpecs$html
  g <- if(htmlfig == 0) function(x, X=NULL) paste(x, X)
       else
         if(htmlfig == 1) function(x, X=NULL) paste(mu$cap(x), mu$lcap(X))
       else
         function(x, X=NULL)
           paste0('\n### ', mu$cap(x),
                  if(length(X) && X != '') paste0('\n', mu$lcap(X)))
  i <- 0
  for(n in names(x)) {
    i <- i + 1
    y <- x[[n]]
    if(length(names(y)) && length(class(y)) == 1 &&
       inherits(y, 'list') && length(y) > 1) {
      for(m in names(y)) {
        if(! after) 
          cat('\n', g(paste0(n, ': ', m)), '\n', sep='')
        suppressWarnings(print(y[[m]]))   # for plotly warnings
        if(after) cat('\n', g(paste0(n, ': ', m)), '\n', sep='')
      }
      if(length(lcap) && lcap[i] != '') cat(mu$lcap(lcap[i]))
    }
    else {
      if(! after)
        cat('\n', g(n, if(length(lcap)) lcap[i]), '\n', sep='')
      suppressWarnings(print(x[[n]]))
      if(after) cat('\n', g(n, if(length(lcap)) lcap[i]), '\n', sep='')
    }
  }
  invisible()
}

putHfig <- function(x, ..., scap=NULL, extra=NULL, subsub=TRUE, hr=TRUE,
                    table=FALSE, file='', append=FALSE, expcoll=NULL) {
  ec <- length(expcoll) > 0
  if(ec && ! table)
    stop('expcoll can only be specified for tables, not figures')
  
  mu <- markupSpecs$html
  
  lcap <- unlist(list(...))
  if(length(lcap)) lcap <- paste(lcap, collapse=' ')

  if(ec && length(lcap))
    stop('does not work when lcap is specified because of interaction with markdown sub-subheadings')
  
  if(! length(lcap) && ! length(scap)) {
    if(ec) {
      if(hr) x <- c(mu$hrule, x)
      x <- mu$expcoll(paste(expcoll, collapse=' '),
                      paste(x, collapse='\n'))
      cat(x, file=file, append=append, sep='\n')
      return(invisible())
    }
    if(hr) cat(mu$hrule, '\n', sep='', file=file, append=append)
    if(table) cat(x, file=file, append=append || hr, sep='\n')
    else suppressWarnings(print(x))  # because of # colors in pallette warning
    return(invisible())
  }
  if(! length(scap)) {
    scap <- lcap
    lcap <- NULL
  }
  scap <- if(table) mu$tcap(scap) else mu$cap(scap)
  if(subsub) scap <- paste0('\n### ', scap)
  if(hr && ! ec) cat(mu$hrule, '\n', sep='', file=file, append=append)
  if(! ec) cat(scap, '\n', sep='', file=file, append=append | hr)
  if(length(lcap)) {
    lcap <- if(table) mu$ltcap(lcap) else mu$lcap(lcap)
    if(length(extra))
      lcap <- paste0(
        '<TABLE width="100%" BORDER="0" CELLPADDING="3" CELLSPACING="3">',
        '<TR><TD>', lcap, '</TD>',
        paste(paste0('<TD style="text-align:right;padding: 0 1ex 0 1ex;">',
                     extra, '</TD>'), collapse=''),
        '</TR></TABLE>')
    if(ec) x <- c(lcap, x)
    else
      cat(lcap, '\n', sep='', file=file, append=TRUE)
  }
  if(ec)
    x <- mu$expcoll(paste(expcoll, collapse=' '),
                    paste(c(if(hr) mu$hrule, scap, x), collapse='\n'))

if(table) cat(x, sep='\n', file=file, append=TRUE)
else
  suppressWarnings(print(x))
invisible()
}

putHcap <- function(..., scap=NULL, extra=NULL, subsub=TRUE, hr=TRUE,
                    table=FALSE, file='', append=FALSE) {


  mu    <- markupSpecs$html
  fcap  <- if(table) mu$tcap  else mu$cap
  flcap <- if(table) mu$ltcap else mu$lcap
  output <- function(r)
    if(is.logical(file)) return(r)
    else {
      cat(r, sep='\n', file=file, append=append)
      return(invisible())
      }
  
  lcap <- unlist(list(...))
  if(length(lcap)) lcap <- paste(lcap, collapse=' ')
  
  r <- NULL
  
  if(! length(lcap) && ! length(scap)) return('')

  if(! length(scap)) {
    scap <- lcap
    lcap <- NULL
  }
  scap <- fcap(scap)
  scap <- if(is.logical(subsub)) paste0('\n', if(subsub) '### ', scap)
          else if(is.character(subsub) && subsub != '') paste0(subsub, scap)
                                                                     
  if(hr) r <- c(r, mu$hrule)
  r <- c(r, scap)
  if(length(lcap)) {
    lcap <- flcap(lcap)
    if(length(extra))
      lcap <- paste0(
        '<TABLE width="100%" BORDER="0" CELLPADDING="3" CELLSPACING="3">',
        '<TR><TD>', lcap, '</TD>',
        paste(paste0('<TD style="text-align:right;padding: 0 1ex 0 1ex;">',
                     extra, '</TD>'), collapse=''),
        '</TR></TABLE>')
    r <- c(r, lcap)
  }
  output(r)
}

combineLabels <- function(...)
  {
    w <- list(...)
    labs <- sapply(w[[1]], label)
    lw <- length(w)
    if(lw > 1) for(j in 2:lw)
      {
        lab <- sapply(w[[j]], label)
        lab <- lab[lab != '']
        if(length(lab)) labs[names(lab)] <- lab
      }
    labs[labs != '']
  }
