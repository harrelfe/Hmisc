cleanup.import <-
  function(obj, labels=NULL, lowernames=FALSE, 
           force.single=TRUE, force.numeric=TRUE,
           rmnames=TRUE,
           big=1e20, sasdict, 
           print=prod(dimobj) > 5e5,
           datevars=NULL, datetimevars=NULL,
           dateformat='%F', fixdates=c('none','year'),
           autodate=FALSE, autonum=FALSE, fracnn=0.3,
           considerNA=NULL,
           charfactor=FALSE)
{
  fixdates <- match.arg(fixdates)
  nam      <- names(obj)
  dimobj   <- dim(obj)
  nv       <- length(nam)

  if(!missing(sasdict)) {
    sasvname <- makeNames(sasdict$NAME)
    if(any(w <- nam %nin% sasvname))
      stop(paste('The following variables are not in sasdict:',
                 paste(nam[w],collapse=' ')))
    
    saslabel <- structure(as.character(sasdict$LABEL), 
                          names=as.character(sasvname))
    labels <- saslabel[nam]
    names(labels) <- NULL
  }

  if(length(labels) && length(labels) != dimobj[2])
    stop('length of labels does not match number of variables')

  if(lowernames)
    names(obj) <- casefold(nam)

  if(print)
    cat(dimobj[2],'variables; Processing variable:')

  for(i in 1:dimobj[2]) {
    if(print) cat(i,'')

    x <- obj[[i]]
    modif <- FALSE
    if(length(dim(x)))
      next
      
    if(rmnames) {
      if(length(attr(x,'names'))) {
        attr(x,'names') <- NULL
        modif <- TRUE
      } else if(length(attr(x,'.Names'))) {
        attr(x,'.Names') <- NULL
        modif <- TRUE
      }
    }
    
    if(length(attr(x,'Csingle'))) {
      attr(x,'Csingle') <- NULL
      modif <- TRUE
    }
    
    if(length(c(datevars,datetimevars)) &&
       nam[i] %in% c(datevars,datetimevars) &&
       !all(is.na(x))) {
      if(!(is.factor(x) || is.character(x)))
        stop(paste('variable',nam[i],
                   'must be a factor or character variable for date conversion'))
      
      x <- as.character(x)
      ## trim leading and trailing white space
      x <- sub('^[[:space:]]+','',sub('[[:space:]]+$','', x))
      xt <- NULL
      if(nam[i] %in% datetimevars) {
        xt <- gsub('.* ([0-9][0-9]:[0-9][0-9]:[0-9][0-9])','\\1',x)
        xtnna <- setdiff(xt, c('',' ','00:00:00'))
        if(!length(xtnna)) xt <- NULL
        x <- gsub(' [0-9][0-9]:[0-9][0-9]:[0-9][0-9]','',x)
      }
      if(fixdates != 'none') {
        if(dateformat %nin% c('%F','%y-%m-%d','%m/%d/%y','%m/%d/%Y'))
          stop('fixdates only supported for dateformat %F %y-%m-%d %m/%d/%y %m/%d/%Y')
        x <-
          switch(dateformat,
                 '%F'      =gsub('^([0-9]{2})-([0-9]{1,2})-([0-9]{1,2})',
                                 '20\\1-\\2-\\3',x),
                 '%y-%m-%d'=gsub('^[0-9]{2}([0-9]{2})-([0-9]{1,2})-([0-9]{1,2})',
                                 '\\1-\\2-\\3',x),
                 '%m/%d/%y'=gsub('^([0-9]{1,2})/([0-9]{1,2})/[0-9]{2}([0-9]{2})',
                                 '\\1/\\2/\\3',x),
                 '%m/%d/%Y'=gsub('^([0-9]{1,2})/([0-9]{1,2})/([0-9]{2})$',
                                 '\\1/\\2/20\\3',x)
                 )
      }
            
      x <- if(length(xt) && requireNamespace("chron", quietly = TRUE)) {
             cform <- if(dateformat=='%F') 'y-m-d'
                      else gsub('%','',tolower(dateformat))
             chron::chron(x, xt, format=c(dates=cform,times='h:m:s'))
           }
           else as.Date(x, format=dateformat)
      modif <- TRUE
    }

    if(autodate) {
      x <- convertPdate(x, fracnn=fracnn, considerNA=considerNA)
      if(inherits(x, 'Date')) modif <- TRUE
    }

    if(autonum && (is.character(x) || is.factor(x))) {
      xc <- trimws(as.character(x))
      bl <- is.na(xc) | xc == ''
      xn <- suppressWarnings(as.numeric(xc))
      ignore  <- bl | tolower(xc) %in% tolower(trimws(considerNA))
      illegal <- is.na(xn) & ! bl
      illegal.not.ignored <- xc[illegal & ! ignore]
#      if(length(illegal) < sum(tolower(xc) %nin%
#                               c('', tolower(considerNA)) & ! is.na(xc)) * fracnn) {
        if(length(illegal.not.ignored) < sum(! ignore) * fracnn) {
          labx <- attr(x, 'label')
          x <- xn
          attr(x, 'label') <- labx   # was label(x) <- 2025-03-09
          attr(x, 'special.miss') <-
            list(codes=xc[illegal], obs=which(illegal))
          class(x) <- c(class(x), 'special.miss')
          modif <- TRUE
        }
    }
    
    if(length(labels)) {
      label(x) <- labels[i]
      modif <- TRUE
    }
    
    if(force.numeric && length(lev <- levels(x))) {
      if(all.is.numeric(lev)) {
        labx <- attr(x,'label')
        x <- as.numeric(as.character(x))
        attr(x, 'label') <- labx    # was label(x) <- 2025-03-09
        modif <- TRUE
      }
    }
    
    if(storage.mode(x) == 'double') {
      xu <- unclass(x)
      j <- is.infinite(xu) | is.nan(xu) | abs(xu) > big
      if(any(j,na.rm=TRUE)) {
        x[j] <- NA
        modif <- TRUE
        if(print)
          cat('\n')
        
        cat(sum(j,na.rm=TRUE),'infinite values set to NA for variable',
            nam[i],'\n')
      }
      
      isdate <- testDateTime(x)
      if(force.single && !isdate) {
        allna <- all(is.na(x))
        if(allna) {
          storage.mode(x) <- 'integer'
          modif <- TRUE
        }
        
        if(!allna) {
          notfractional <- !any(floor(x) != x, na.rm=TRUE)
          if(max(abs(x),na.rm=TRUE) <= (2^31-1) && notfractional) {
            storage.mode(x) <- 'integer'
            modif <- TRUE
          }
        }
      }
    }
    
    if(charfactor && is.character(x)) {
      if(length(unique(x)) < .5*length(x)) {
        x <- sub(' +$', '', x)  # remove trailing blanks
        x <- factor(x, exclude=c('', NA))
        modif <- TRUE
      }
    }
    
    if(modif) obj[[i]] <- x
    NULL
  }
  
  if(print) cat('\n')
  if(!missing(sasdict)) {
    sasat <- sasdict[1,]
    attributes(obj) <- c(attributes(obj),
                         sasds=as.character(sasat$MEMNAME),
                         sasdslabel=as.character(sasat$MEMLABEL))
  }
  
  obj
}

upData <- function(object, ...,
                   subset, rename=NULL, drop=NULL, keep=NULL,
                   labels=NULL, units=NULL, levels=NULL,
                   force.single=TRUE, lowernames=FALSE, caplabels=FALSE, classlab=FALSE,
                   moveUnits=FALSE, charfactor=FALSE, print=TRUE, html=FALSE) {

  if(html) print <- FALSE
  
  upfirst <- function(txt) gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", txt, perl=TRUE)

  if(lowernames) names(object) <- casefold(names(object))
  isdt <- inherits(object, 'data.table')
  no   <- names(object)
  nobs <- nrow(object)
  out  <- paste('Input object size:\t', object.size(object), 'bytes;\t',
                length(no), 'variables\t', nobs, 'observations\n')
  if(print) cat(out)

  if(! missing(subset)) {
    s <- substitute(subset)
    r <- eval(s, object, parent.frame())
    if(! is.logical(r)) stop('subset must be a logical expression')
    r <- r & ! is.na(r)
    object <- object[r, , drop=FALSE]
    nobs <- sum(r)
  }
  rnames <- row.names(object)

  g <- function(x)
    c(sm       = storage.mode(x),
      labclass = inherits(x, 'labelled'),
      labpres  = length(la <- attr(x, 'label')) && la != '',
      lowuc    = is.character(x) && length(unique(x)) < length(x) / 2)

  vinfo <- sapply(object, g)

  ## Variables with labels but not classed as 'labelled' will not
  ## keep labels upon subscripting. Examples: variables imported
  ## using the haven package
  
  j <- which(vinfo['labpres', ] == 'TRUE' & vinfo['labclass', ] == 'FALSE')
  if(classlab && length(j))
    for(i in j) {
      x <- object[[i]]
      class(x) <- c('labelled', class(x))
      object[[i]] <- x
    }
  ## Much slower:
  ##  for(i in j) class(object[[i]]) <- c('labelled', class(object[[i]]))


  ## The following is targeted at R workspaces exported from StatTransfer
  al <- attr(object, 'var.labels')
  if(length(al)) {
    if(caplabels) al <- upfirst(al)
    for(i in 1:length(no))
      if(al[i] != '') label(object[[i]]) <- al[i]
    attr(object, 'var.labels') <- NULL
    if(missing(force.single)) force.single <- FALSE
  } else
    if(caplabels) {
      for(i in which(vinfo['labpres', ] == 'TRUE')) {
        x <- object[[i]]
        if(length(la <- attr(x, 'label'))) {
          attr(x, 'label') <- upfirst(la)
          object[[i]] <- x
        }
      }
    }
  al <- attr(object, 'label.table')
  if(length(al)) {
    for(i in 1 : length(no)) {
      ali <- al[[i]]
      if(length(ali))
        object[[i]] <- factor(object[[i]], unname(ali), names(ali))
    }
    attr(object, 'label.table') <- attr(object, 'val.labels') <- NULL
  }
  
  if(moveUnits)
    for(i in 1:length(no)) {
      z <- object[[i]]
      lab <- olab <- attr(z,'label')
      if(!length(lab) || length(attr(z, 'units')))
        next

      paren <- length(grep('\\(.*\\)',lab))
      brack <- length(grep('\\[.*\\]',lab))
      if(paren + brack == 0) next

      u <- if(paren) regexpr('\\(.*\\)', lab)
           else regexpr('\\[.*\\]', lab)

      len <- attr(u,'match.length')
      un  <- substring(lab, u + 1, u + len - 2)
      lab <- substring(lab, 1, u-1)
      if(substring(lab, nchar(lab), nchar(lab)) == ' ')
        lab <- substring(lab, 1, nchar(lab) - 1)

      out <- c(out, outn <- paste('Label for', no[i], 'changed from',
                                  olab, 'to',
                                  lab, '\n\tunits set to', un, '\n'))
      if(print) cat(outn)
      attr(z,'label') <- lab
      attr(z,'units') <- un
      object[[i]] <- z
    }

  if(length(rename)) {
    nr <- names(rename)
    if(length(nr) == 0 || any(nr == ''))
      stop('the list or vector specified in rename must specify variable names')

    for(i in 1 : length(rename)) {
      if(nr[i] %nin% no)
        stop(paste('unknown variable name:',nr[i]))

      out <- c(out, outn <- paste('Renamed variable\t', nr[i],
                                  '\tto', rename[[i]], '\n'))
      if(print) cat(outn)
    }

    no[match(nr, no)] <- colnames(vinfo)[match(nr, colnames(vinfo))] <-
      unlist(rename)
    names(object) <- no
  }

  z <- substitute(list(...))
  
  if(length(z) > 1) {
    z <- z[-1]
    vn <- names(z)
    if(!length(vn) || any(vn == ''))
      stop('variables must all have names')

    for(i in 1 : length(z)) {
      x <- eval(z[[i]], object, parent.frame())
      v <- vn[i]
      if(v %in% no) {
        out <- c(out, outn <- paste0('Modified variable\t', v, '\n'))
        if(print) cat(outn)
        vinfo[, v] <- g(x)
      }
      else {
        out <- c(out, outn <- paste0('Added variable\t\t', v, '\n'))
        if(print) cat(outn)
        no <- c(no, v)
        vinfo <- cbind(vinfo, g(x))
        colnames(vinfo)[ncol(vinfo)] <- v
      }
      d <- dim(x)
      lx <- if(length(d)) d[1] else length(x)
      
      if(lx != nobs) {
        if(lx == 1) {
          warning(paste('length of ',v,
                        ' is 1; will replicate this value.', sep=''))
          x <- rep(x, length.out=nobs)
        }
        else {
          f <- find(v)
          if(length(f)) {
            out <- c(out, outn <- paste('Variable', v, 'found in',
                                        paste(f, collapse=' '), '\n'))
            if(print) cat(outn)
          }
          
          stop(paste('length of ', v, ' (', lx, ')\n',
                     'does not match number of rows in object (',
                     nobs, ')', sep=''))
        }
      }
      
      ## If x is factor and is all NA, user probably miscoded. Add
      ## msg.
      if(is.factor(x) && all(is.na(x)))
        warning(paste('Variable ',v,'is a factor with all values NA.\n',
         'Check that the second argument to factor() matched the original levels.\n',
                      sep=''))
      object[[v]] <- x
    }
  }

  if(force.single) {
    ## sm <- sapply(object, storage.mode)
    sm <- vinfo['sm', ]
    ii <- which(sm == 'double')
    if(length(ii))
      for(i in ii) {
        x <- object[[i]]
        if(testDateTime(x) || is.matrix(x))
          next
        ## For long vectors don't consider unless the first 500 are NA
        ## all(is.na(x)) is slow
        notallna <- any(! is.na(x[1:min(nobs,500)]))
        notallna <- notallna || any(! is.na(x))
        if(! notallna) storage.mode(object[[i]]) <- 'integer'
        else {
          ## For long vectors don't consider unless the first 500
          ## are integer
          if(nobs < 500 || ! any(floor(x[1:500]) != x[1:500], na.rm=TRUE)) { 
            notfractional <- ! any(floor(x) != x, na.rm=TRUE)
            if(notfractional && max(abs(x), na.rm=TRUE) <= (2 ^ 31 - 1)) {
              storage.mode(x) <- 'integer'
              object[[i]] <- x
            }
          }
        }
      }
  }
  
  if(charfactor) {
    mfact <- as.logical(vinfo['lowuc', ])
    if(any(mfact))
      for(i in which(mfact)) {
        x <- sub(' +$', '', object[[i]])  # remove trailing blanks
        object[[i]] <- factor(x, exclude=c('', NA))
      }
  }

  if(length(drop)  && length(keep)) stop('cannot specify both drop and keep')

  if(length(drop)) {
    if(length(drop) == 1) {
      out <- c(out, outn <- paste0('Dropped variable\t',drop,'\n'))
      if(print) cat(outn)
      }
    else {
      out <- c(out, outn <- paste0('Dropped variables\t',
                                   paste(drop, collapse=','), '\n'))
      if(print) cat(outn)
    }

    s <- drop %nin% no
    if(any(s))
      warning(paste('The following variables in drop= are not in object:',
                    paste(drop[s], collapse=' ')))

    no <- no[no %nin% drop]
    object <- if(isdt) object[, ..no] else object[no]
  }

  if(length(keep)) {
      if(length(keep) == 1) {
        out <- c(out, outn <- paste0('Kept variable\t', keep, '\n'))
        if(print) cat(outn)
        }
      else {
        out <- c(out, outn <- paste0('Kept variables\t',
                                     paste(keep, collapse=','), '\n'))
        if(print) cat(outn)
      }

    s <- keep %nin% no
    if(any(s))
      warning(paste('The following variables in keep= are not in object:',
                    paste(keep[s], collapse=' ')))

    no <- no[no %in% keep]
    object <- if(isdt) object[, ..no] else object[no]
  }

  if(length(levels)) {
    if(!is.list(levels)) stop('levels must be a list')

    nl <- names(levels)
    s <- nl %nin% no
    if(any(s)) {
      warning(paste('The following variables in levels= are not in object:',
                    paste(nl[s], collapse=' ')))
      nl <- nl[! s]
    }

    for(n in nl) {
      x <- object[[n]]
      if(! is.factor(x))
        x <- as.factor(x)

      levels(x) <- levels[[n]]
      object[[n]] <- x
      ## levels[[nn]] will usually be a list; S+ invokes merge.levels
    }
  }

  if(length(labels)) {
    nl <- names(labels)
    if(!length(nl)) stop('elements of labels were unnamed')
    s <- nl %nin% no
    if(any(s)) {
      warning(paste('The following variables in labels= are not in object:',
                    paste(nl[s], collapse=' ')))
      nl <- nl[!s]
    }
    
    for(n in nl) {
      x <- object[[n]]
      label(x) <- labels[[n]]
      object[[n]] <- x
      }
  }

  if(length(units)) {
    nu <- names(units)
    s <- nu %nin% no
    if(any(s)) {
      warning(paste('The following variables in units= are not in object:',
                    paste(nu[s], collapse=' ')))
      nu <- nu[!s]
    }
    for(n in nu)
      attr(object[[n]], 'units') <- units[[n]]
  }

  out <- c(out, outn <- paste0('New object size:\t',
                               object.size(object),
                               ' bytes;\t', length(no), ' variables\t', nobs,
                               ' observations\n'))
  if(print) cat(outn)
  if(html) {
    cat('<pre style="font-size:60%;">\n')
    cat(out)
    cat('</pre>\n')
  }

  object
  }

dataframeReduce <- function(data, fracmiss=1, maxlevels=NULL,
                            minprev=0, print=TRUE) {
  g <- function(x, fracmiss, maxlevels, minprev) {
    if(is.matrix(x)) {
      f <- mean(is.na(x %*% rep(1, ncol(x))))
      return(if(f > fracmiss)
             paste('fraction missing>',fracmiss,sep='') else '')
    }
        h <- function(a, b)
          if(a == '') b else if(b == '') a else paste(a, b, sep=';')
    f <- mean(is.na(x))
    x <- x[!is.na(x)]
    n <- length(x)
    r <- if(f > fracmiss)
      paste('fraction missing>', fracmiss,sep='') else ''
    if(is.character(x)) x <- factor(x)
    if(length(maxlevels) && is.factor(x) &&
       length(levels(x)) > maxlevels)
      return(h(r, paste('categories>',maxlevels,sep='')))
    s <- ''
    if(is.factor(x) || length(unique(x))==2) {
      tab <- table(x)
      if((min(tab) / max(n, 1L)) < minprev) {
        if(is.factor(x)) {
          x <- combine.levels(x, minlev=minprev)
          s <- 'grouped categories'
          if(length(levels(x)) < 2)
            s <- paste('prevalence<', minprev, sep='')
        }
        else s <- paste('prevalence<', minprev, sep='')
      } 
    }
    h(r, s)
  }
  h <- sapply(data, g, fracmiss, maxlevels, minprev)
  if(all(h == '')) return(data)
  info <- data.frame(Variable=names(data)[h != ''],
                     Reason=h[h != ''], row.names=NULL, check.names=FALSE)
  if(print) {
    cat('\nVariables Removed or Modified\n\n')
    print(info)
    cat('\n')
  }
  s <- h == 'grouped categories'
  if(any(s)) for(i in which(s))
    data[[i]] <- combine.levels(data[[i]], minlev=minprev)
  if(any(h != '' & ! s)) data <- data[h == '' | s]
  attr(data, 'info') <- info
  data
}

utils::globalVariables('..no')
