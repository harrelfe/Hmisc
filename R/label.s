##"label<-"  <- function(x, value) {
##  attr(x, "label") <- value
##  x
##}

label <- function(x, ...) UseMethod("label")

label.default <- function(x, units=FALSE, plot=FALSE, default=NULL,
                  grid=FALSE, ...)
{
  at <- attributes(x)
  lab <- at$label
  if(length(default) && !length(lab))
    lab <- default
  
  un  <- at$units
  labelPlotmath(lab,
                if(units)
                  un
                else
                  NULL,
                plotmath=plot, grid=grid)
}


labelPlotmath <- function(label, units=NULL, plotmath=.R., grid=FALSE)
{
  if(!length(label))
    label <- ''
  
  if(!length(units))
    units <- ''
  
  g <-
    if(plotmath && .R.) function(x,y=NULL, xstyle=NULL, ystyle=NULL)
    {
      h <- function(w, style=NULL)
        if(length(style))
          paste(style,'(',w,')',sep='')
        else
          w
      
      if(!length(y))
        return(parse(text=h(plotmathTranslate(x),xstyle)))
      
      x <- paste('list(',h(plotmathTranslate(x),xstyle),',',
                 h(plotmathTranslate(y),ystyle),')',sep='')
      parse(text=x)
    } else function(x, y=NULL, ...) if(length(y)) paste(x,y) else x

  if(units=='')
    g(label)
  
  ##  return(if(label=='' || !.R. || !plotmath) label
  ##else g(paste('paste("',label,'")',sep='')))
  ## paste('foo') allows foo to have blanks in plotmath
  
  else if(label=='')
    g(units)
  else if(plotmath && .R.)
    g(label, units, ystyle='scriptstyle')
  ##  label <- if(plotmath && .R.)
  ##    paste('list(paste("',label, '"), scriptstyle(~~',units,'))',sep='')
  else
    paste(label,' [',units,']',sep='')
}


plotmathTranslate <- function(x)
{
  if(length(grep('paste', x)))
    return(x)
  
  specials <- c(' ','%','_')
  spec <- FALSE
  for(s in specials)
    if(length(grep(s,x)))
      spec <- TRUE
  
  if(spec)
    x <- paste('paste("',x,'")',sep='')
  else if(substring(x,1,1)=='/')
    x <- paste('phantom()', x, sep='')
  
  x
}

"label<-" <- function(x, value) UseMethod("label<-")

##From Bill Dunlap, StatSci  15Mar95:
if(!.SV4.) {
  "label<-.default" <- function(x, value) {
    structure(x, label=value,
              class=c('labelled',
                attr(x,'class')[attr(x,'class')!='labelled']))
  }
} else {
  "label<-.default" <- function(x, value) {
    ## 1Nov00 for Splus 5.x, 6.x
    ##  oldClass(x) <- unique(c('labelled', oldClass(x),
    ##                          if(is.matrix(x))'matrix'))
    attr(x,'label') <- value
    x
  }
}

if(!.SV4.) "[.labelled"<- function(x, ...)
{
  tags <- valueTags(x)
  ##lab <- attr(x, "label")  19sep02
  x <- NextMethod("[")

  valueTags(x) <- tags
  
  x
}

if(FALSE) {
  y <- matrix(1:12, nrow=4)
  class(y)
  oldClass(y) <- 'labelled'
  class(y)
  oldClass(y)
  attr(y,'label') <- 'Y'

  ##y <- structure(matrix(1:12, nrow=4), class=c('labelled','matrix'), label='Y')

  a <- structure(list(x=1:4, y=y),
                 class='data.frame',row.names=c('a','b','c','d'))
  a[1:4,1]
  a[1:4,2]
}

if(!.SV4.) "print.labelled"<- function(x, ...)
{
  x.orig <- x
  u <- attr(x,'units')   ## 19sep02
  if(length(u))
    attr(x,'units') <- NULL   # so won't print twice
  
  cat(attr(x, "label"),
      if(length(u))
        paste('[', u, ']', sep=''),
      "\n")
  
  attr(x, "label") <- NULL
  ##attr(x,'class') <- setdiff(attr(x,'class'), "labelled")
  ## The above didn't work under R 20Mar01
  class(x) <-
    if(length(class(x))==1 && class(x)=='labelled')
      NULL
    else
      class(x)[class(x) != 'labelled']
  
  ## 3mar03 - added NULL part above, to work for R 1.7
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
  ##else stop(paste("no method for coercing", d, "to data.frame")) 26May97
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
    lab <-
      if(length(lab)==0)
        ''
      else
        lab
    
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
  for(i in 1:length(object)) {
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
  for(i in 1:length(dotlist)) {
    vname[i] <-
      if(length(lname) && lname[i]!='')
        lname[i]
      else
        name[i]
    
    ## Was changed 21Mar01 - R barked at setting vname[i] to NULL
    lab <- vname[i]
    if(labels) {
      lab <- attr(dotlist[[i]],'label')
      if(length(lab) == 0)
        lab <- vname[i]
    }
    
    label(dotlist[[i]]) <- lab
  }
  
  names(dotlist) <- vname[1:length(dotlist)]
  dotlist
}
