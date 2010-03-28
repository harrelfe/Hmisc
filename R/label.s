##"label<-"  <- function(x, value) {
##  attr(x, "label") <- value
##  x
##}

## Define 'labelled' as an Splus oldClass name
if(.SV4.) {
  setOldClass('labelled')
}

label <- function(x, default=NULL, ...) UseMethod("label")

label.default <- function(x, default=NULL, units=FALSE, plot=FALSE,
                          grid=FALSE, ...)
{
  if(length(default) > 1)
    stop("the default string cannot be of length greater then one")
  
  at <- attributes(x)
  lab <- at$label
  if(length(default) && (!length(lab) || lab==''))
    lab <- default
  
  un  <- at$units
  labelPlotmath(lab,
                if(units) un else NULL,
                plotmath=plot, grid=grid)
}


label.data.frame <- function(x, default=NULL, self=FALSE, ...) {
  if(self) {
    label.default(x)
  } else {
    if(length(default) > 0 && length(default) != length(x)) {
      stop('length of default must same as x')
    }
    
    labels <- mapply(FUN=label, x=x, default=default, MoreArgs=list(self=TRUE), USE.NAMES=FALSE)
    names(labels) <- names(x)
    return(labels)
  }
}

labelPlotmath <- function(label, units=NULL, plotmath=.R., grid=FALSE)
{
  if(!length(label)) label <- ''
  
  if(!length(units) || (length(units)==1 && is.na(units))) units <- ''
  
  g <-
    if(plotmath && .R.) function(x, y=NULL, xstyle=NULL, ystyle=NULL)
      {
        h <- function(w, style=NULL)
          if(length(style))
            paste(style,'(',w,')',sep='')
          else
            w

        tryparse <- function(z, original)
          {
            p <- try(parse(text=z), silent=TRUE)
            if(is.character(p)) original else p
          }
        if(!length(y))
          return(tryparse(h(plotmathTranslate(x), xstyle), x))
      
        w <- paste('list(',h(plotmathTranslate(x), xstyle), ',',
                   h(plotmathTranslate(y), ystyle), ')', sep='')
        tryparse(w, paste(x, y))
      } else function(x, y=NULL, ...) if(length(y)) paste(x,y) else x

  if(units=='') g(label)
  else if(label=='') g(units)
  else if(plotmath && .R.)
    g(label, units, ystyle='scriptstyle')
  else paste(label,' [',units,']',sep='')
}


plotmathTranslate <- function(x)
{
  if(length(grep('paste', x))) return(x)
  
  specials <- c(' ','%','_')
  spec <- FALSE
  for(s in specials)
    if(length(grep(s,x)))
      spec <- TRUE
  
  if(spec) x <- paste('paste("',x,'")',sep='')
  else if(substring(x,1,1)=='/') x <- paste('phantom()', x, sep='')
  x
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

  if('labelled' %nin% oldClass(x)) {
    oldClass(x) <- c('labelled', oldClass(x))
  }
  return(x)
}
## } else function(x, ..., value)
##   {
##     ## Splus 5.x, 6.x
##     ##  oldClass(x) <- unique(c('labelled', oldClass(x),
##     ##                          if(is.matrix(x))'matrix'))
##     attr(x,'label') <- value
##     return(x)
##   }

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

if(!.SV4.) "[.labelled"<- function(x, ...)
{
  tags <- valueTags(x)
  x <- NextMethod("[")
  valueTags(x) <- tags
  x
}

if(!.SV4.) "print.labelled"<- function(x, ...)
{
  x.orig <- x
  u <- attr(x,'units')
  if(length(u))
    attr(x,'units') <- NULL   # so won't print twice
  
  cat(attr(x, "label"),
      if(length(u))
        paste('[', u, ']', sep=''),
      "\n")
  
  attr(x, "label") <- NULL
  class(x) <-
    if(length(class(x))==1 && class(x)=='labelled')
      NULL
    else
      class(x)[class(x) != 'labelled']
  
  ## next line works around print bug
  if(!length(attr(x,'class')))
    attr(x,'class') <- NULL
  
  NextMethod("print")
  invisible(x.orig)
}


if(.R.) as.data.frame.labelled <- as.data.frame.vector

if(!.R. && version$major < 5) as.data.frame.labelled <- function(x, ...)
{
  y <- x
  cy <- attr(y,'class')
  cy <-
    if(length(cy)>1)
      cy[cy!='labelled']
    else
      NULL
  
  if(length(cy)==0)
    cy <- NULL  # handles wierd case e.g. class=rep('lab..',2)
  
  attr(y,'class') <- cy
  
  ## data.class(character(0) class) returns ''
  d <- data.class(y)
  methodname <- paste("as.data.frame", d, sep = '.')
  if(exists(methodname, mode = "function"))
    (get(methodname, mode = "function"))(x, ...)
  else {
    if(options()$check)
      warning(paste("no method for coercing",d,"to data.frame"))
    
    as.data.frame.AsIs(y, ...)
  }
}

Label <- function(object, ...) UseMethod("Label")


Label.data.frame <- function(object, file='', append=FALSE, ...)
{
  nn <- names(object)
  for(i in 1:length(nn)) {
    lab <- attr(object[[nn[i]]],'label')
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


reLabelled <- function(object)
{
  for(i in 1:length(object))
    {
      x <- object[[i]]
      lab <- attr(x, 'label')
      cl  <- oldClass(x)
      if(length(lab) && !any(cl=='labelled')) {
        oldClass(x) <- c('labelled',cl)
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
      if(labels)
        {
          lab <- attr(dotlist[[i]],'label')
          if(length(lab) == 0)
            lab <- vname[i]
        }
    
      label(dotlist[[i]]) <- lab
    }
  
  names(dotlist) <- vname[1:length(dotlist)]
  dotlist
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
