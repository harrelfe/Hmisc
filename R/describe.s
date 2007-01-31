## $Id$
describe <- function(x, ...) UseMethod("describe")  #13Mar99


describe.default <- function(x, descript, ...)  #13Mar99
{
  if(missing(descript)) {
    descript <- deparse(substitute(x)) #13Mar99
  }

  if(is.matrix(x)) {
    describe.matrix(x, descript, ...)
  } else {
    describe.vector(x, descript, ...)  #13Mar99
  }
}


describe.vector <- function(x, descript, exclude.missing=TRUE, digits=4,
                            weights=NULL, normwt=FALSE, minlength=NULL, ...)
{
  oldopt <- options(digits=digits)
  on.exit(options(oldopt))

  if(length(weights)==0) {
    weights <- rep(1,length(x))
  }

  special.codes <- attr(x, "special.miss")$codes
  labx <- attr(x,"label")
  
  if(missing(descript)) {
    descript <- as.character(sys.call())[2]
  }

  if(length(labx) && labx!=descript) {
    descript <- paste(descript,":",labx)
  }

  un <- attr(x,"units")
  if(length(un) && un=='') {
    un <- NULL
    ## 8jun03 and next
  }

  fmt <- attr(x,'format')
  if(length(fmt) && (is.function(fmt) || fmt=='')) {
    fmt <- NULL
  }
  
  ## is.function 1dec03
  if(length(fmt) > 1) {
    fmt <- paste(as.character(fmt[[1]]),as.character(fmt[[2]]))
  }
  
  present <- if(all(is.na(x)))
               rep(FALSE,length(x))
             else if(is.character(x))
               (if(.R.)
                  x!="" & x!=" " & !is.na(x)
                else
                x!="" & x!=" ")
             else
               !is.na(x)
  
  present <- present & !is.na(weights)

  if(length(weights) != length(x)) {
    stop('length of weights must equal length of x')
  }

  if(normwt) {
    weights <- sum(present)*weights/sum(weights[present])
    n <- sum(present)
  } else {
    n <- sum(weights[present])
  }

  if(exclude.missing && n==0) {
    return(structure(NULL, class="describe"))
  }
  
  missing <- sum(weights[!present], na.rm=TRUE)
  atx <- attributes(x)
  atx$names <- atx$dimnames <- atx$dim <- atx$special.miss <- NULL  
  ## added dim,dimnames 18 Dec 95, last 1 7May96
  
  atx$class <- atx$class[atx$class!='special.miss']

  isdot <- testDateTime(x,'either') # is date or time var
  isdat <- testDateTime(x,'both')   # is date and time combo var

  x <- x[present,drop=FALSE]  ## drop=F 14Nov97
  x.unique <- sort(unique(x))
  weights <- weights[present]

  n.unique <- length(x.unique)
  attributes(x) <- attributes(x.unique) <- atx

  isnum <- (is.numeric(x) || isdat) && !is.category(x)
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
      lab <- c(lab,"missing month")
    }
    
    if((nn <- length(pd$day))>0) {
      counts <- c(counts, nn)
      lab <- c(lab,"missing day")
    }
    
    if((nn <- length(pd$both))>0) {
      counts <- c(counts, nn)
      lab <- c(lab,"missing month,day")
    }
  }

  if(length(atx$substi.source)) {
    tabss <- table(atx$substi.source)
    counts <- c(counts, tabss)
    lab <- c(lab, names(tabss))
  }

  counts <- c(counts,n.unique)
  lab <- c(lab,"unique")
  x.binary <- n.unique==2 && isnum && x.unique[1]==0 && x.unique[2]==1
  if(x.binary) {
    counts <- c(counts,sum(weights[x==1]))
    lab <- c(lab,"Sum")
  }
  
  if(isnum) {
    xnum <- if(.SV4.)
              as.numeric(x)
            else
              oldUnclass(x)  # 3Dec00
    
    if(isdot) {
      dd <- sum(weights*xnum)/sum(weights)  # 3Dec00
      fval <- formatDateTime(dd, atx, !timeUsed)
      counts <- c(counts, fval)
    } else {
      counts <- c(counts,format(sum(weights*x)/sum(weights),...))
    }
    
    lab <- c(lab,"Mean")
  } else if(n.unique==1) {
    counts <- c(counts, x.unique)
    lab <- c(lab, "value")
  }

  if(n.unique>=10 & isnum) {
    q <-
      if(any(weights != 1)) {
        wtd.quantile(xnum,weights,normwt=FALSE,na.rm=FALSE,  # 3Dec00
                     probs=c(.05,.1,.25,.5,.75,.90,.95))
      } else {
        quantile(xnum,c(.05,.1,.25,.5,.75,.90,.95),na.rm=FALSE)
      }
    ## Only reason to call quantile is that the two functions can give
    ## different results if there are ties, and users are used to quantile()
    fval <-
      if(isdot)
        formatDateTime(q, atx, !timeUsed)
      else
        format(q,...)
    
    counts <- c(counts, fval)
    lab <- c(lab,".05",".10",".25",".50",".75",".90",".95")
  }
  names(counts) <- lab
  z$counts <- counts

  counts <- NULL

  if(inherits(x,'mChoice')) z$mChoice <- summary(x, minlength=minlength) else {
    if(n.unique>=20) {
      if(isnum) {
      r <- range(xnum)
      xg <- pmin(1 + floor((100 * (xnum - r[1]))/
                           (r[2] - r[1])), 100)
      z$intervalFreq <- list(range=as.single(r),
                             count = as.integer(tabulate(xg)))
    }
    
    loandhi <- x.unique[c(1:5,(n.unique-4):n.unique)]
    fval <-
      if(isdot && (class(loandhi) %nin% 'timeDate')) {
        formatDateTime(oldUnclass(loandhi), at=atx, roundDay=!timeUsed)
      } else {
        format(format(loandhi), ...)
      }
    counts <- fval
    names(counts) <- c("L1","L2","L3","L4","L5","H5","H4","H3","H2","H1")
  }

  if(n.unique>1 && n.unique<20 && !x.binary) {
    tab <- wtd.table(if(isnum)
                       format(x)
                     else
                       x,
                     weights, normwt=FALSE, na.rm=FALSE, type='table')

    pct <- round(100*tab/sum(tab))
    counts <- t(as.matrix(tab))
    counts <- rbind(counts, pct)
    dimnames(counts)[[1]]<- c("Frequency","%")
  }
}  
  z$values <- counts
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
                        digits=digits,...)  #13Mar99
    
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
  mf <- match.call(expand=FALSE)
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
    if(attr(ter,"response")==1)
      d <- c(d[2],d[1],d[-(1:2)])
    else
      d <- d[-1]
    d <- paste(d, collapse=" ")
    descript <- d
  }

  Z <- describe.data.frame(mf, descript, digits=digits, weights=weights, ...)
  if(length(z <- attr(mf,"na.action")))
    attr(Z,'naprint') <- naprint(z) 

  Z
}

na.retain <- function(d) d


print.describe <- function(x, condense=TRUE, ...)
{
  at <- attributes(x)
  if(length(at$dimensions)) {
    cat(at$descript,'\n\n',at$dimensions[2],' Variables     ',at$dimensions[1],
        ' Observations\n')
    
    if(length(at$naprint)) cat('\n',at$naprint,'\n')
    cat('---------------------------------------------------------------------------\n')
    for(z in x) {
      if(length(z)==0)
        next
      print.describe.single(z, condense=condense)
      cat('---------------------------------------------------------------------------\n')
    }
    if(length(at$missing.vars)) {
      cat('\nVariables with all observations missing:\n\n')
      print(at$missing.vars, quote=FALSE)
    }
  } else print.describe.single(x, condense=condense)
  
  invisible()
}

print.describe.single <- function(x, condense=TRUE, ...)
{
  wide <- .Options$width
  des <- x$descript
  if(length(x$units))
    des <- paste(des, ' [', x$units, ']', sep='')
  
  if(length(x$format))
    des <- paste(des, '  Format:', x$format, sep='')
  
  cat(des,'\n')
  print(x$counts, quote=FALSE)
  if(length(val <- x$values)) {
    if(length(dim(val))==0) {
      if(condense) {
        low <- paste('lowest :', paste(val[1:5],collapse=' '))
        hi  <- paste('highest:', paste(val[6:10],collapse=' '))
        cat('\n',low,sep='')
        if(nchar(low)+nchar(hi)+2>wide)
          cat('\n')
        else
          cat(', ')
        
        cat(hi,'\n')
      } else {
        cat('\n'); print(val, quote=FALSE)
      }
    } else {
      lev <- dimnames(val)[[2]]
      if(condense && (mean(nchar(lev))>10 | length(lev) < 5)) {
        z <- ''; len <- 0; cat('\n')
        for(i in 1:length(lev)) {
          w <- paste(lev[i], ' (', val[1,i], ', ', val[2,i], '%)', sep='')
          l <- nchar(w)
          if(len + l + 2 > wide) {
            cat(z,'\n'); len <- 0; z <- ''
          }
          
          if(len==0) {
            z <- w; len <- l
          } else {
            z <- paste(z, ', ', w, sep=''); len <- len + l + 2
          }
        }
        
        cat(z, '\n')
      } else {
        cat('\n'); print(val, quote=FALSE)
      }
    }
  }
  if(length(x$mChoice)) {cat('\n'); print(x$mChoice, prlabel=FALSE)}
  
  invisible()
}


'[.describe' <- function(object, i, ...)
{
  at <- attributes(object)
  object <- '['(oldUnclass(object),i)
  structure(object, descript=at$descript,
            dimensions=c(at$dimensions[1], length(object)),
            class='describe')
}


latex.describe <- function(object, title=NULL, condense=TRUE,
                           file=paste('describe',
                                      first.word(expr=attr(object, 'descript')),
                                      'tex', sep='.'),
                           append=FALSE, size='small',
                           tabular=TRUE, greek=TRUE, ...)
{
  at <- attributes(object)
  ct <- function(..., file, append=FALSE)
  {
    if(file=='')
      cat(...)
    else
      cat(..., file=file, append=append)
    
    invisible()
  }
  
  ct('\\begin{spacing}{0.7}\n', file=file, append=append)
  if(length(at$dimensions)) {
    ct('\\begin{center}\\textbf{', latexTranslate(at$descript), '\\\\',
       at$dimensions[2],'Variables~~~~~',at$dimensions[1],
       '~Observations}\\end{center}\n', file=file, append=TRUE)
    if(length(at$naprint))
      ct(at$naprint,'\\\\\n', file=file, append=TRUE)
    
    ct('\\vspace{-.5ex}\\hrule\\smallskip{\\',size,'\n',
       sep='', file=file, append=TRUE)
    vnames <- at$names
    i <- 0
    for(z in object) {
      i <- i + 1
      if(length(z)==0)
        next
      
      ct('\\vbox{', file=file, append=TRUE)
      latex.describe.single(z, condense=condense, vname=vnames[i],
                            file=file, append=TRUE, tabular=tabular, greek=greek)
      ct('\\vspace{-.5ex}\\hrule\\smallskip}\n', file=file, append=TRUE)
    }
    
    if(length(mv <- at$missing.vars)) {
      ct('\\smallskip\\noindent Variables with all observations missing:\\ \\smallskip\n',
         file=file, append=TRUE)
      mv <- latexTranslate(mv)
      mv <- paste('\\texttt{',mv,'}',sep='')
      mv <- paste(mv, collapse=', ')
      ##ct('\\texttt{',at$missing.vars, '}', sep='', file=file,
      ##append=TRUE)
      ct(mv, file=file, append=TRUE)
    }
    
    ct('}', file=file, append=TRUE)  # added 23oct02
  } else latex.describe.single(object,
                               vname=first.word(expr=at$descript),
                               condense=condense,
                               file=file, append=TRUE, size=size,
                               tabular=tabular)
  
  ## was append=append 23oct02; also removed } in cat below
  ct('\\end{spacing}\n', file=file, append=TRUE)

  ##if(!.SV4.)   18Oct01
  structure(list(file=file,  style=c('setspace','relsize')),
            class='latex')
}


latex.describe.single <- function(object, title=NULL, condense=TRUE, vname,
                                  file, append=FALSE, size='small',
                                  tabular=TRUE, greek=TRUE, ...)
{
  ct <- function(..., file, append=FALSE)
  {
    if(file=='')
      cat(...)
    else
      cat(..., file=file, append=append)
    
    invisible()
  }

  oldw <- options(width=85)
  on.exit(options(oldw))

  wide <- switch(size,
                 normalsize=66,
                 small=73,
                 scriptsize=93,
                 73)

  intFreq <- object$intervalFreq

  ## Put graph on its own line if length of label > 3.5 inches
  ## For normalsize there are 66 characters per 4.8 in. standard width

  z   <- latexTranslate(object$descript, '&', '\\&', greek=greek)
  ## If any math mode ($ not preceeded by \) don't put label part in bold
  des <- if(!length(grep('[^\\]\\$', z)))
           paste('\\textbf{', z, '}', sep='')
         else {
           ## Get text before : (variable name)
           sp <- strsplit(z, ' : ')[[1]]
           vnm <- sp[1]
           rem <- paste(sp[-1], collapse=':')
           paste('\\textbf{', vnm, '}: ', rem, sep='')
         }
  
  if(length(object$units))
    des <- paste(des, '{\\smaller[1] [',
                 latexTranslate(object$units),']}', sep='')
  
  if(length(object$format))
    des <- paste(des, '{\\smaller~~Format:', latexTranslate(object$format),
                 '}', sep='')
  
  desbas <- paste(object$descript,
                  if(length(object$units))
                    paste(' [', object$units, ']', sep=''),
                  if(length(object$format))
                    paste('  Format:', object$format, sep=''))

  ct('\\noindent', des, sep='', file=file, append=append)
  if(length(intFreq)) {
    counts <- intFreq$count
    maxcounts <- max(counts)
    ## \mbox{~~~} makes \hfill work
    ct(if(nchar(desbas)/(wide/4.8) > (4.8-1.5))' \\\\ \\mbox{~~~} \n',
       '\\setlength{\\unitlength}{0.001in}\\hfill',
       '\\begin{picture}(1.5,.1)(1500,0)',
       '\\linethickness{0.6pt}\n', sep='', file=file, append=TRUE)
    for(i in (1:100)[counts > 0]) {
      ct('\\put(',round(1000*(i-1)*1.5/100),',0){\\line(0,1){',
         max(1,round(1000*counts[i]/maxcounts*.1)),'}}\n',
         sep='', file=file, append=TRUE)
    }
    
    ct('\\end{picture}\n', file=file, append=TRUE)
  }

  sz <- ''
  if(tabular) {
    ml <- nchar(paste(object$counts,collapse='  '))
    if(ml > 90)
      tabular <- FALSE
    else if(ml > 80)
      sz <- '[2]'
  }
  
  ct('{\\smaller\n', sz, sep='', file=file, append=TRUE)
  if(tabular) {
    ct('\\\\ \\begin{tabular}{',
       paste(rep('r',length(object$counts)),collapse=''),'}\n',
       file=file, append=TRUE)
    ct(paste(names(object$counts), collapse='&'), '\\\\ \n',
       file=file, append=TRUE)
    ct(paste(object$counts, collapse='&'), '\\end{tabular}\n',
       file=file, append=TRUE)
  }
  
  ct('\\begin{verbatim}\n', file=file, append=TRUE)
  if(file!='')
    sink(file, append=TRUE)  ## 22dec02
  
  if(!tabular)
    print(object$counts, quote=FALSE)
  
  if(length(val <- object$values)) {
    if(length(dim(val))==0) {
      if(condense) {
        low <- paste('lowest :', paste(val[1:5],collapse=' '))
        hi  <- paste('highest:', paste(val[6:10],collapse=' '))
        cat('\n',low,sep='')
        if(nchar(low)+nchar(hi)+2 > wide) cat('\n') else cat(', ')
        cat(hi,'\n')
      } else {
        cat('\n'); print(val, quote=FALSE)
      }
    } else {
      lev <- dimnames(val)[[2]]
      if(condense && (mean(nchar(lev))>10 | length(lev) < 5)) {
        z <- ''; len <- 0; cat('\n')
        for(i in 1:length(lev)) {
          w <- paste(lev[i], ' (', val[1,i], ', ', val[2,i], '%)', sep='')
          l <- nchar(w)
          if(len + l + 2 > wide) {
            cat(z,'\n'); len <- 0; z <- ''
          }
    
          if(len==0) {
            z <- w; len <- l
          } else {
            z <- paste(z, ', ', w, sep=''); len <- len + l + 2
          }
        }
        
        cat(z, '\n')
      } else {
        cat('\n'); print(val, quote=FALSE)
      }
    }
  }
  if(length(object$mChoice)) print(object$mChoice, prlabel=FALSE)
  
  cat('\\end{verbatim}\n}\n')
  if(file!='')
    sink()
  
  invisible()
}


if(FALSE && .SV4.) {
  setMethod('latex', 'describe', latex.describe)
  remove('latex.describe')
}


dataDensityString <- function(x, nint=30)
{
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if(length(x) < 2) return('')
  r <- range(x)
  x <- floor(nint * (x-r[1])/(r[2]-r[1]))
  x <- pmin(tabulate(x), 37)
  paste(format(r[1]),' <',
        paste(substring(' 1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
                        x+1,x+1), collapse=''),
        '> ',format(r[2]),sep='')
}


## Unused code from latex.describe.single
if(FALSE && length(intFreq))
{
  psthere <- TRUE
  psfile <- paste(psBase,vname,'.ps',sep='')
  x <- seq(intFreq$range[1], intFreq$range[2], length=100)
  counts <- intFreq$count
  oldopt <- options(warn=-1)
  if(under.unix)
    postscript(file = psfile, horizontal = FALSE,
               width = 1.5, height = .1, 
               maximize = TRUE,
               onefile = FALSE, print.it = FALSE)
  else
    postscript(file = psfile, horizontal = FALSE,
               width=1.5, height=.1)
  
  oldpar <- par(mar=rep(0,4),oma=rep(0,4))  # add mex=.5 to prevent
                                            # error msgs.  Need this
  
  ## in 2nd par call.
  on.exit(par(oldpar))
  options(oldopt)
  plot(x, freqFun(counts), type='n', axes=FALSE, xlab='', ylab='')
  j <- counts > 0
  segments(x[j], 0, x[j], freqFun(counts[j]))
  dev.off()
}


contents <- function(object, ...) UseMethod('contents')


contents.data.frame <- function(object, ...)
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
    if(length(at$label))
      lab[i] <- at$label
    if(length(at$longlabel))
      longlab[i] <- at$longlabel
    
    if(length(at$units))
      un[i] <- at$units
    
    atl <- at$levels
    fl[i] <- length(atl)
    cli <- at$class[at$class %nin% c('labelled','factor')]
    if(length(cli))
      cl[i] <- cli[1]
    
    sm[i] <- storage.mode(x)
    nas[i] <- sum(is.na(x))
    if(length(atl))
    {
      if(length(Lev)) for(j in 1:length(Lev))
        {
          w <- Lev[[j]]
          if(!is.name(w) && is.logical(all.equal(w, atl)))
            {
              atl <- as.name(names(Lev)[j])
              break   
            }
        }
      Lev[[nam[i]]] <- atl
    }
  }
  
  w <- list(Labels=if(any(lab!=''))         lab,
            Units=if(any(un!=''))           un,
            Levels=if(any(fl>0))            fl,
            Class=if(any(cl!=''))           cl,
            Storage=                        sm,
            NAs=if(any(nas>0))              nas )
  
  if(.R.)
    w <- w[sapply(w, function(x)length(x)>0)]
  
  ## R does not remove NULL elements from a list
  structure(list(contents=data.frame(w, row.names=nam),
                 dim=d, maxnas=max(nas), dfname=dfname,
                 Levels=Lev,
                 longLabels=if(any(longlab!='')) structure(longlab, names=nam)),
            class='contents.data.frame')
}


print.contents.data.frame <-
  function(x, sort=c('none','names','labels','NAs'), prlevels=TRUE, ...)
{
  sort <- match.arg(sort)
  d <- x$dim
  maxnas <- x$maxnas
  cat('\nData frame:',x$dfname,'\t',d[1],' observations and ',d[2],
      ' variables    Maximum # NAs:',maxnas,'\n\n',sep='')
  cont <- x$contents
  nam <- row.names(cont)

  switch(sort,
         names={
           cont <- cont[order(nam),]
         },
         labels={
           if(length(cont$Labels)) 
             cont <-  cont[order(cont$Labels, nam),]
         },
         NAs={
           if(maxnas>0)
             cont <- cont[order(cont$NAs,nam),]
         })

  if(length(cont$Levels))
    cont$Levels <- ifelse(cont$Levels==0,'',format(cont$Levels))
  
  print(cont)

  if(prlevels && length(L <- x$Levels)) {
    cat('\n')
    nam <- names(L)
    w <- .Options$width-max(nchar(nam))-5
    reusingLevels <- sapply(L, is.name)
    fullLevels <- which(!reusingLevels)
    namf <- lin <- names(L[fullLevels])
    ## separate multiple lines per var with \n for print.char.matrix
    j <- 0
    for(i in fullLevels)
      {
        j <- j + 1
        varsUsingSame <- NULL
        if(sum(reusingLevels))
          {
            for(k in which(reusingLevels)) if(L[[k]] == nam[j]) 
              varsUsingSame <- c(varsUsingSame, nam[k])
            if(length(varsUsingSame))
              namf[j] <- paste(c(namf[j], varsUsingSame), collapse='\n')
          }
        lin[j] <- paste(pasteFit(L[[i]], width=w), collapse='\n')
      }
    if(.R.) {
      z <- cbind(Variable=namf, Levels=lin)
      print.char.matrix(z, col.txt.align='left', col.name.align='left',
                        row.names=TRUE, col.names=TRUE)
    } else print.char.matrix(matrix(lin,ncol=1,
                                    dimnames=list(nam,'Levels')))
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
  function(object, sort=c('none','names','labels','NAs'), prlevels=TRUE,
           file=paste('contents',object$dfname,'html',sep='.'),
           levelType=c('list','table'),
           append=FALSE, ...)
{
  sort <- match.arg(sort)
  levelType <- match.arg(levelType)
  d <- object$dim
  maxnas <- object$maxnas
  cat('<hr><h2>Data frame:',object$dfname,
      '</h2>',d[1],
      ' observations and ',d[2],
      ' variables, maximum # NAs:',maxnas,'<hr>\n',sep='',
      file=file, append=append)
  cont <- object$contents
  nam <- row.names(cont)

  switch(sort,
         names={cont <- cont[order(nam),]},
         labels={
           if(length(cont$Labels)) 
             cont <-  cont[order(cont$Labels, nam),]
         },
         NAs={
           if(maxnas>0) cont <- cont[order(cont$NAs,nam),]
         })
  
  link <- matrix('', nrow=nrow(cont), ncol=1+ncol(cont),
                 dimnames=list(dimnames(cont)[[1]], c('Name', dimnames(cont)[[2]])))
  
  longlab <- object$longLabels
  if(length(longlab)) {
    longlab <- longlab[longlab!='']
    link[names(longlab),'Name'] <- paste('#longlab',names(longlab),sep='.')
  }
  
  L <- object$Levels
  Lnames <- names(L)
  if(length(cont$Levels)) {
    cont$Levels <- ifelse(cont$Levels==0, '', format(cont$Levels))
    namUsed     <- sapply(L, function(z) if(is.name(z)) as.character(z) else '')
    reusingLevels <- namUsed != ''
    fullLevels  <- which(!reusingLevels)
    namUsed     <- ifelse(reusingLevels, namUsed, Lnames)
    names(namUsed) <- Lnames
    link[,'Levels'] <- ifelse(cont$Levels=='', '', paste('#levels',namUsed[nam],sep='.'))
  }
  adj <- rep('l', length(cont))
  adj[names(cont) %in% c('NAs','Levels')] <- 'r'
  out <- html(cont, file=file, append=TRUE,
              link=link,
              col.just=adj, ...)
  
  cat('<hr>\n', file=file, append=TRUE)
  
  if(prlevels && length(L))
    {
      if(levelType=='list')
        {
          cat('<h2 align="center">Category Levels</h2>\n', file=file, append=TRUE)
          for(i in fullLevels) 
            {
              l <- L[[i]]
              nami <- Lnames[i]
              w <- nami
              if(sum(reusingLevels))
                for(k in which(reusingLevels))
                  if(L[[k]] == nami) w <- c(w, Lnames[k])
              cat('<a name="levels.',nami,'"><h3>',
                  paste(w, collapse=', '), '</h3>\n', sep='', 
                  file=file, append=TRUE)
              cat('<ul>\n', file=file, append=TRUE)
              for(k in l) cat('<li>', k, '</li>\n', sep='',
                              file=file, append=TRUE)
              cat('</ul>\n', file=file, append=TRUE)
            }
        }
      else
        {  
          ## Function to split a character vector x as evenly as
          ## possible into n elements, pasting multiple elements
          ## together when needed
          evenSplit <- function(x, n)
            {
              indent <- function(z) if(length(z)==1)z else
              c(z[1], paste('&nbsp&nbsp&nbsp',z[-1],sep=''))
              m <- length(x)
              if(m <= n) return(c(indent(x), rep('',n-m)))
              totalLength <- sum(nchar(x)) + (m-1)*3.5
              ## add indent, comma, space
              lineLength  <- ceiling(totalLength/n)
              y <- pasteFit(x, sep=', ', width=lineLength)
              m <- length(y)
              if(m > n) for(j in 1:10)
                {
                  lineLength <- round(lineLength*1.1)
                  y <- pasteFit(x, sep=', ', width=lineLength)
                  m <- length(y)
                  if(m <= n) break
                }
              ## Take evasive action if needed
              if(m==n) indent(y) else if(m < n)
                c(indent(y), rep('', n-m)) else 
              c(paste(x, collapse=', '), rep('',n-1))
            }
          nam <- names(L)
          v <- lab <- lev <- character(0)
          j <- 0
          for(i in fullLevels) 
            {
              j <- j + 1
              l <- L[[i]]
              nami <- nam[i]
              v <- c(v, nami)
              w <- nami
              if(sum(reusingLevels))
                for(k in which(reusingLevels)) if(L[[k]] == nam[i]) w <- c(w, nam[k])
              lab <- c(lab, evenSplit(w, length(l)))
              lev <- c(lev, l)
            }
          z <- cbind(Variable=lab, Levels=lev)
          out <- html(z, file=file, append=TRUE,
                      link=ifelse(lab=='','',paste('levels',v,sep='.')),
                      linkCol='Variable', linkType='name', ...)
          cat('<hr>\n',file=file,append=TRUE)
        }
    }

  i <- longlab != ''
  if(any(i)) {
    nam <- names(longlab)[i]
    names(longlab) <- NULL
    lab <- paste('longlab', nam, sep='.')
    z <- cbind(Variable=nam, 'Long Label'=longlab[i])
    out <- html(z, file=file, append=TRUE,
                link=lab, linkCol='Variable', linkType='name', ...)
    cat('<hr>\n', file=file, append=TRUE)
  }
  out
}


contents.list <- function(object, dslabels=NULL, ...)
{
  nam <- names(object)
  if(length(dslabels)) {
    dslabels <- dslabels[nam]
    names(dslabels) <- NULL
  }
  
  g <- function(w)
  {
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
