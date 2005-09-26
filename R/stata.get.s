if (.R.) {
  stata.get <- function(file, lowernames=FALSE,
                        convert.dates=TRUE, convert.factors=TRUE,
                        missing.type=FALSE, convert.underscore=TRUE,
                        warn.missing.labels=TRUE, force.single=TRUE)
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
                  warn.missing.labels=warn.missing.labels)

    ## extract attributes from w
    a <- attributes(w)

    ## Do translate attributes names into R names
    nam <- a$names
    nam <- makeNames(a$names, unique=TRUE, allow=allow)
    if(lowernames) nam <- casefold(nam)
    names(w) <- nam

    ## Translate var labels into Hmisc var lables
    vl  <- a$var.labels
    if(length(vl))
      for(i in 1:length(vl)) {
        lab <- vl[i]
        if(lab != '')
          label(w[[i]]) <- lab
      }
    
    ## clear var.labels attribute
    attr(w, 'var.labels') <- NULL

    ## convert integer vars to mode integer
    if(force.single)
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
          else if(max(abs(x), na.rm=TRUE) <= (2^31-1) &&
                  all(floor(x) == x, na,rm=TRUE)) {
            storage.mode(x) <- 'integer'
            changed <- TRUE
          }
        }

        if(changed) w[[v]] <- x
      }

    return(w)
  }
  NULL
}
