if (.R.) {
  stata.get <- function(file, lowernames=FALSE,
                        convert.dates=TRUE, convert.factors=TRUE,
                        missing.type=FALSE, convert.underscore=TRUE,
                        warn.missing.labels=TRUE, force.single=TRUE,
                        allow=NULL, charfactor=FALSE, ...)
  {
    require('foreign')

    ## If file is a url download and set file = to temp file name
    if(length(grep('^http://', file))){
      tf <- tempfile()
      download.file(file, tf, mode='wb', quite=TRUE)
      file <- tf
    }

    ## Read the stata file into w
    w <- read.dta(file, convert.dates=convert.dates,
                  convert.factors=convert.factors,
                  missing.type=missing.type,
                  convert.underscore=convert.underscore,
                  warn.missing.labels=warn.missing.labels, ...)

    ## extract attributes from w
    a <- attributes(w)

    ## Do translate attributes names into R names
    nam <- a$names
    nam <- makeNames(a$names, unique=TRUE, allow=allow)
    if(lowernames) nam <- casefold(nam)
    names(w) <- nam

    ## Translate var labels into Hmisc var lables
    vl  <- a$var.labels
    vl.len <- length(vl)
    label.table.len <- length(a$label.table)
    
    for(i in seq(along.with=w)) {
      if(a$formats[i] != '') {
        attr(w[[i]],'format') <- a$formats[i]
      }

      if(vl.len) {
        lab <- vl[i]
        
        if(lab != '') {
          label(w[[i]]) <- lab
        }
      }

      if(label.table.len) {
        attr(w[[i]], 'value.label.table') <- a$label.table[[i]]
      }
    }
    
    ## clear var.labels attribute
    attr(w, 'var.labels') <- NULL

    ## convert integer vars to mode integer
    for(v in nam) {
      x <- w[[v]]
      changed <- FALSE
      if(all(is.na(x))) {
        storage.mode(x) <- 'integer'
        changed <- TRUE
      }
      else if(!(is.factor(x) || is.character(x) || inherits(x, 'Date'))) {
        if(all(is.na(x))) {
          storage.mode(x) <- 'integer'
          changed <- TRUE
        }
        else if(force.single && max(abs(x), na.rm=TRUE) <= (2^31-1) &&
                all(floor(x) == x, na.rm=TRUE)) {
          storage.mode(x) <- 'integer'
          changed <- TRUE
        }
      } else if(charfactor && is.character(x)) {
        if(max(nchar(x)) >= 2 && (length(unique(x)) < .5*length(x))) {
          x <- factor(x)
          changed <- TRUE
        }
      }


      if(changed) w[[v]] <- x
    }

    stata.info <- a[c('datalabel','version','time.stamp','val.labels')]
    attributes(w) <- c(a[c('names','row.names','class')],
                       stata.info=list(stata.info))
    return(w)
  }
  NULL
}
