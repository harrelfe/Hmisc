spss.get <- function(file, lowernames=FALSE,
                     datevars=NULL,
                     use.value.labels=TRUE,
                     to.data.frame=TRUE,
                     max.value.labels=Inf,
                     force.single=TRUE, allow=NULL, charfactor=FALSE,
                     reencode=NA) {

  w <- read.spss(file, use.value.labels=use.value.labels,
                 to.data.frame=to.data.frame,
                 max.value.labels=max.value.labels,
                 reencode=reencode)
  
  a   <- attributes(w)
  vl  <- a$variable.labels
  nam <- a$names
  nam <- makeNames(a$names, unique=TRUE, allow=allow)
  if(lowernames) nam <- casefold(nam)
  names(w) <- nam
  
  lnam <- names(vl)
  if(length(vl))
    for(i in 1:length(vl)) {
      n <- lnam[i]
      lab <- vl[i]
      if(lab != '' && lab != n) label(w[[i]]) <- lab
    }

  attr(w, 'variable.labels') <- NULL
  if(force.single || length(datevars) || charfactor)
    for(v in nam) {
      x <- w[[v]]
      changed <- FALSE
      if(v %in% datevars) {
        x <- importConvertDateTime(x, 'date', 'spss')
        changed <- TRUE
      } else if(all(is.na(x))) {
        storage.mode(x) <- 'integer'
        changed <- TRUE
      } else if(!(is.factor(x) || is.character(x))) {
        if(all(is.na(x))) {
          storage.mode(x) <- 'integer'
          changed <- TRUE
        } else if(max(abs(x),na.rm=TRUE) <= (2^31-1) &&
                  all(floor(x) == x, na.rm=TRUE)) {
          storage.mode(x) <- 'integer'
          changed <- TRUE
        }
      } else if(charfactor && is.character(x)) {
        if(length(unique(x)) < .5*length(x))
          {
            x <- sub(' +$', '', x)  # remove trailing blanks
            x <- factor(x, exclude='')
            changed <- TRUE
          }
      }
      if(changed) w[[v]] <- x
    }
  
  w
}

csv.get <- function(file, lowernames=FALSE, datevars=NULL, datetimevars=NULL,
                    dateformat='%F', fixdates=c('none','year'),
                    comment.char = "", autodate=TRUE, allow=NULL,
                    charfactor=FALSE,
                    sep=',', skip=0, vnames=NULL, labels=NULL, text=NULL, ...){
  fixdates <- match.arg(fixdates)
  if(length(text) && ! missing(file)) stop('may not specify both file and text')
  scn <- function(skip)
    if(length(text)) scan(text=text, what=character(0), skip=skip,
                          nlines=1, sep=sep, quiet=TRUE)
    else
      scan(file, what=character(0), skip=skip, nlines=1, sep=sep, quiet=TRUE)
  rcsv <- function(...)
    if(length(text)) read.csv(text=text, check.names=FALSE,
                              comment.char=comment.char, sep=sep, skip=skip, ...)
    else
      read.csv(file, check.names=FALSE, comment.char=comment.char,
               sep=sep, skip=skip,, ...)
  if(length(vnames)) vnames <- scn(skip=vnames - 1)
  if(length(labels)) labels <- scn(skip=labels - 1)

  w <- if(length(vnames))
         rcsv(header=FALSE, col.names=vnames)
       else
         rcsv()
  n <- nam <- names(w)
  m <- makeNames(n, unique=TRUE, allow=allow)
  if(length(labels)) n <- labels
  if(lowernames)
    m <- casefold(m)
  
  changed <- any(m != nam)
  if(changed)
    names(w) <- m

  cleanup.import(w,
                 labels=if(length(labels))labels else if(changed)n else NULL,
                 datevars=datevars, datetimevars=datetimevars,
                 dateformat=dateformat, autodate=autodate,
                 fixdates=fixdates, charfactor=charfactor)
}

stata.get <- function(file, lowernames=FALSE,
                        convert.dates=TRUE, convert.factors=TRUE,
                        missing.type=FALSE, convert.underscore=TRUE,
                        warn.missing.labels=TRUE, force.single=TRUE,
                        allow=NULL, charfactor=FALSE, ...) {
  ## depends on the read.dta function from foreign

  ## Function to convert the elements of w into more compact
  ## data storage types.
  convertObjs <- function(x, charfactor, force.single) {
    ## Date is not nessarely a integer but it ignores any
    ## fraction it might have
    if((inherits(x, 'Date') || is.factor(x))
       && storage.mode(x) != 'integer') {
      storage.mode(x) <- 'integer'
    } else if(charfactor && is.character(x)) {
      ## If x is a character and arg charfactor is TRUE then
      ## convert x to a factor if the number of unique values of x is less
      ## than half the total number of values in x
      if(length(unique(x)) < length(x) / 2)
        {
          x <- sub(' +$', '', x)  # remove trailing blanks
          x <- factor(x, exclude='')
        }
    } else if(is.numeric(x)) {
      
      if(all(is.na(x))) {
        ## if all values are NA then convert to integer because
        ## it is 4 bytes instead of 8
        storage.mode(x) <- 'integer'
      }
      else if(force.single && max(abs(x), na.rm=TRUE) <= (2^31-1) &&
              all(floor(x) == x, na.rm=TRUE)) {
        ## convert x to integer if arg force.single is TRUE and the maximum
        ## absolute value of x is less then maximum value that an integer
        ## can store.
        storage.mode(x) <- 'integer'
      }
    }
    
    return(x)
  }
  
  ## A function to create additional attributes to add to the elements of
  ## w
  create.attribs <- function(var.label, val.label, format, label.table) {
    attribs <- list()
    
    if(format != '') {
      attribs$format <- format
    }
    
    ## Translate var labels into Hmisc var lables
    if(var.label != '') {
      attribs$label <- var.label
    }
    
    ## The label.table values are found by looking a the checking to see
    ## if there is a non-empty value in val.labels.  That value corrasponds
    ## a named element in label.table.
    
    ## Check to see if val.label is not empty and it is one of the
    ## names in label.table and that its value is not NULL
    if(val.label != '' && val.label %in% names(label.table) &&
       !is.null(label.table[[val.label]])) {
      attribs$value.label.table <- label.table[[val.label]]
    }
    
    return(attribs)
  }
  
  ## Read the stata file into w
  w <- read.dta(file, convert.dates=convert.dates,
                convert.factors=convert.factors,
                missing.type=missing.type,
                convert.underscore=convert.underscore,
                warn.missing.labels=warn.missing.labels, ...)

  ## extract attributes from w
  a <- attributes(w)
  num.vars <- length(w)
  
  ## Do translate attributes names into R names
  nam <- makeNames(a$names, unique=TRUE, allow=allow)
  if(lowernames) nam <- casefold(nam, upper=FALSE)
  a$names <- nam

  ## If var.labels is empty then create a empty char vector.
  if(!length(a$var.labels)) {
    a$var.labels <- character(num.vars)
  }
  
  ## If val.labels is empty then create an empty char vector.
  if(length(a$val.labels)) {
    val.labels <- a$val.labels
  } else {
    val.labels <- character(num.vars)
  }
  
  ## create list of attributes for the elements in w.  An mapply is faster
  ## then a for loop in large data sets.
  attribs <- mapply(FUN=create.attribs, var.label=a$var.labels,
                    val.label=val.labels, format=a$formats,
                    MoreArgs=list(label.table=a$label.table),
                    SIMPLIFY=FALSE)
  
  ## clear var.labels attribute
  attr(w, 'var.labels') <- NULL

  ## Convert the elements of w as needed
  w <- lapply(w, FUN=convertObjs, force.single=force.single,
              charfactor=charfactor)
    
  ## strip off the naming info for w
  w <- unname(w)
  
  ## add the new attributes to the current attributes of
  ## the elements of w
  for(i in seq(along.with=w)) {
    ## Set the label for the element
    if('label' %in% names(attribs[[i]])) {
      label(w[[i]]) <- attribs[[i]]$label
      ## clear the label value from attribs[[i]]
      attribs[[i]]$label <- NULL
    }

    ## combine the new attribs with the current attributes
    consolidate(attributes(w[[i]])) <- attribs[[i]]
  }

  ## add the names, rownames, class variables, and some extra stata
  ## info back to w
  stata.info <- a[c('datalabel','version','time.stamp','val.labels','label.table')]
  attributes(w) <- c(a[c('names','row.names','class')],
                     stata.info=list(stata.info))
  return(w)
}
