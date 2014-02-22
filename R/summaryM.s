summaryM <- function(formula, groups=NULL, data=NULL, subset,
                     na.action=na.retain, 
                     overall=FALSE, continuous=10, na.include=FALSE,
                     quant=c(0.025, 0.05, 0.125, 0.25, 0.375, 0.5, 0.625,
                       0.75, 0.875, 0.95, 0.975),
                     nmin=100, test=FALSE,
                     conTest=conTestkw, catTest=catTestchisq,
                     ordTest=ordTestpo) {

  formula <- Formula(formula)
  Y <- if(!missing(subset) && length(subset))
    model.frame(formula, data=data, subset=subset, na.action=na.action)
  else
    model.frame(formula, data=data, na.action=na.action)

#  mf <- match.call(expand.dots=FALSE)
#  m <- match(c('formula', 'data', 'subset', 'na.action'), names(mf), 0)
#  mf <- mf[c(1, m)]
#  if(missing(na.action)) mf$na.action <- na.retain
#  formula <- Formula(formula)
#  mf[[1]] <- as.name('model.frame')
#  mf$formula <- formula
#  Y <- eval(mf, parent.frame())

   X <- model.part(formula, data=Y, rhs=1)
   Y <- model.part(formula, data=Y, lhs=1)

   getlab <- function(x, default) {
     lab <- attr(x, 'label')
     if(!length(lab) || lab=='') default else lab
   }
   
   if(length(X)) {
     xname <- names(X)
     if(length(xname) == 1 && ! length(groups)) groups <- xname
     if(! length(groups) && length(xname) > 1) {
       warnings('Must specify groups when > 1 right hand side variable is present.\ngroups taken as first right hand variable.')
       groups <- xname[1]
     }
     svar <- if(length(xname) == 1) factor(rep('.ALL.', nrow(X)))
      else do.call('interaction', list(X[setdiff(xname, groups)], sep=' '))

     group <- X[[groups]]
     glabel <- getlab(group, groups)
   } else {
     svar  <- factor(rep('.ALL.', nrow(Y)))
     group <- rep(0, nrow(Y))  # y1 + y2 ~ 1, no grouping
     groups <- group.freq <- NULL
     glabel <- ''
   }

  quants <- unique(c(quant, 0.025, 0.05, 0.125, 0.25, 0.375, 0.5,
                            0.625, 0.75, 0.875, 0.95, 0.975 ))

  nv    <- ncol(Y)
  nameY <- names(Y)

  R <- list()
  for(strat in levels(svar)) {
    instrat <- svar == strat
  
    n <- integer(nv)
    type <- n

    comp <- dat <- vector("list", nv)
    names(comp) <- names(dat) <- nameY

    labels <- Units <- vector("character", nv)
    if(test) {
      testresults <- vector('list', nv)
      names(testresults) <- names(comp)
    }

    gr <- group[instrat]
    group.freq <- table(gr)
    group.freq <- group.freq[group.freq > 0]
    if(overall) group.freq <- c(group.freq, Combined=sum(group.freq))
      
    for(i in 1 : nv) {
      w  <- Y[instrat, i]
      
      if(length(attr(w, "label")))
        labels[i] <- attr(w, "label")
      
      if(length(attr(w, 'units'))) Units[i]  <- attr(w, 'units')
      
      if(!inherits(w, 'mChoice')) {
        if(!is.factor(w) && !is.logical(w) &&
           length(unique(w[! is.na(w)])) < continuous) 
          w <- as.factor(w)
      
        s <- !is.na(w)

        if(na.include && !all(s) && length(levels(w))) {
          w <- na.include(w)
          levels(w)[is.na(levels(w))] <- 'NA'
          s <- rep(TRUE, length(s))
        }

        n[i] <- sum(s)
        w <- w[s]
        g <- gr[s, drop=TRUE]
        if(is.factor(w) || is.logical(w)) {
          tab <- table(w, g)
          if(test) {
            if(is.ordered(w))
              testresults[[i]] <- ordTest(g, w)
            else
              testresults[[i]] <- catTest(tab)
          }
          
          if(nrow(tab) == 1) {
            b <- casefold(dimnames(tab)[[1]], upper=TRUE)
            pres <- c('1', 'Y', 'YES', 'PRESENT')
            abse <- c('0', 'N', 'NO',  'ABSENT')
            jj <- match(b, pres, nomatch=0)
            if(jj > 0) bc <- abse[jj]
            else {
              jj <- match(b, abse, nomatch=0)
              if(jj > 0) bc <- pres[jj]
            }
            
            if(jj) {
              tab <- rbind(tab, rep(0, ncol(tab)))
              dimnames(tab)[[1]][2] <- bc
            }
          }
          
          if(overall)
            tab <- cbind(tab, Combined=apply(tab, 1, sum))
          
          comp[[i]] <- tab
          type[i] <- 1
        } else {
          sfn <- function(x, quant) {
            o <- options(digits=10)
            ## So won't lose precision in quantile names
            on.exit(options(o))
            c(quantile(x,quant), Mean=mean(x), SD=sqrt(var(x)),
              N=sum(!is.na(x)))
          }

          qu <- tapply(w, g, sfn, simplify=TRUE, quants)
          if(test) testresults[[i]] <- conTest(g, w)

          if(overall) qu$Combined <- sfn(w, quants)

          comp[[i]] <- matrix(unlist(qu), ncol=length(quants) + 3,
                              byrow=TRUE,
                              dimnames=list(names(qu),
                                c(format(quants), 'Mean', 'SD', 'N')))
          if(any(group.freq <= nmin))
            dat[[i]] <-
              lapply(split(w, g), nmin=nmin,
                     function(x, nmin)
                     if(length(x) <= nmin) x
                     else NULL)
          type[i] <- 2
        }
      } else {
        w <- as.numeric(w) == 1 ## multiple choice variables
        n[i] <- nrow(w)
        g    <- as.factor(gr)
        ncat <- ncol(w)
        tab <- matrix(NA, nrow=ncat, ncol=length(levels(g)),
                      dimnames=list(dimnames(w)[[2]], levels(g)))
        if(test) {
          pval <- numeric(ncat)
          names(pval) <- dimnames(w)[[2]]
          d.f. <- stat <- pval
        }
        
        for(j in 1 : ncat) {
          tab[j,] <- tapply(w[,j], g, sum, simplify=TRUE, na.rm=TRUE)
          if(test) {
            tabj <- rbind(table(g)-tab[j,],tab[j,])
            st <- catTest(tabj)
            pval[j] <- st$P
            stat[j] <- st$stat
            d.f.[j] <- st$df
          }
        }
        if(test)
          testresults[[i]] <- list(P=pval, stat=stat, df=d.f.,
                                   testname=st$testname,
                                   statname=st$statname,
                                   latexstat=st$latexstat,
                                   plotmathstat=st$plotmathstat)
                                   
        if(overall) tab <- cbind(tab, Combined=apply(tab,1,sum))
        comp[[i]] <- tab
        type[i]   <- 3
      }
    }
  
  labels <- ifelse(nchar(labels), labels, names(comp))
  R[[strat]] <- list(stats=comp, type=type, 
                     group.freq=group.freq,
                     labels=labels, units=Units,
                     quant=quant, data=dat,
                     N=sum(!is.na(group)), n=n,
                     testresults=if(test)testresults)
  }
  structure(list(results=R, group.name=groups, group.label=glabel,
                 call=call, formula=formula), 
            class="summaryM")
}

plot.summaryM <-
  function(x, vnames = c('labels', 'names'), what = c('proportion','%'),
           which = c('both', 'categorical', 'continuous'),
           xlim = if(what == 'proportion') c(0,1)
           else c(0,100), 
           xlab = if(what == 'proportion') 'Proportion'
                  else 'Percentage', 
           pch = c(16, 1, 2, 17, 15, 3, 4, 5, 0), exclude1 = TRUE,
           main, subtitles = TRUE,
           prtest = c('P', 'stat', 'df', 'name'), pdig = 3, eps = 0.001,
           conType = c('bp', 'dot', 'raw'),
           cex.means = 0.5, cex=par('cex'), ...)
{
  obj <- x
  vnames  <- match.arg(vnames)
  what    <- match.arg(what)
  which   <- match.arg(which)
  conType <- match.arg(conType)
  
  ul <- vnames=='labels'

  if(is.logical(prtest) && !prtest) prtest <- 'none'

  for(strat in names(x$results)) {
    obj <- x$results[[strat]]
    test   <- obj$testresults
    if(!length(test)) prtest <- 'none'

    varNames <- names(obj$stats)
    vn <- if(ul) obj$labels
     else varNames
    
    Units <- obj$units
  
    nw     <- if(lg <- length(obj$group.freq)) lg
     else 1

    gnames <- names(obj$group.freq) 

    if(missing(main)) main <-
     if(strat != '.ALL.') strat
     else if(nw == 1) ''
     else 
       paste(if(what=='proportion')'Proportions'
        else 'Percentages','Stratified by',
             obj$group.label)

    pch     <- rep(pch,     length=nw)
  
    lab <- vnd <- z <- nmiss <- vnamd <- NULL
    type  <- obj$type; n <- obj$n

    opar <- par()
    on.exit(setParNro(opar))

    npages <- 0
  
    if(which != 'continuous' && any(type %in% c(1,3))) {
      ftstats <- NULL  
      for(i in (1:length(type))[type==1 | type==3]) {
        nam <- vn[i]
        tab <- obj$stats[[i]]
        if(nw==1)
          tab <- as.matrix(tab)

        nr <- nrow(tab)
        denom <- if(type[i] == 1) apply(tab, 2, sum)
        else obj$group.freq

        y <- (if(what=='proportion') 1 else 100) *
          sweep(tab, 2, denom, FUN='/')

        lev <- dimnames(y)[[1]]
        exc <- exclude1 && (nr==2)
        jstart <- if(exc) 2 else 1

        rl <- casefold(lev)
        binary <- type[i]==1 && exc &&
          (all(rl %in% c("0","1"))|all(rl %in% c("false","true"))|
           all(rl %in% c("absent","present")))

        for(j in jstart:nrow(y)) {
          if(nw==1) z <- rbind(z, y[j,])
          else {
            yj               <- rep(NA, nw)
            names(yj)        <- gnames
            yj[names(y[j,])] <- y[j,]
            z                <- rbind(z, yj)
          }
          
          lab <- c(lab, if(binary) '' else lev[j])
          vnd <- c(vnd, nam)
          vnamd <- c(vnamd, varNames[i])
        }
        
        if(any(prtest != 'none')) {
          fts <- formatTestStats(test[[varNames[i]]], type[i]==3,
                                 if(type[i]==1)1
                                 else 1:nr,
                                 prtest=prtest,
                                 plotmath=TRUE,
                                 pdig=pdig, eps=eps)
          
          ftstats <- c(ftstats, fts, 
                       if(type[i]==1 && nr-exc-1 > 0)
                       rep(expression(''),
                           nr-exc-1))
        }
      }
      
      dimnames(z) <- list(lab, dimnames(z)[[2]])
      dotchart3(z, groups=factor(vnd, levels=unique(vnd)), xlab=xlab, xlim=xlim,
                auxdata=if(!any(prtest == 'none')) ftstats,
                pch=pch, ...)
    
      if(main == '' && strat != '.ALL.') title(strat)
       else if(main != '') title(main)

      npages <- npages + 1
      setParNro(opar)
      ## Dummy key if only one column, so won't use another Key from an
      ## earlier run
      if(nw < 2) {
        Key1 <- function(...)invisible(NULL)
        .setKey(Key1)
      } else { ##set up for key() if > 1 column
        Key3 <- function(x=NULL, y=NULL, lev, pch) {
          oldpar <- par(usr=c(0,1,0,1),xpd=NA)
          on.exit(par(oldpar))
          if(is.list(x)) {
            y <- x$y
            x <- x$x
          }
          
          ## Even though par('usr') shows 0,1,0,1 after lattice draws
          ## its plot, it still needs resetting
          if(!length(x)) x <- 0
          if(!length(y))
            y <- 1  ## because of formals()
          
          rlegend(x, y, legend=lev, pch=pch, ...)
          invisible()
        }
        formals(Key3) <- list(x=NULL,y=NULL,lev=names(obj$group.freq),
                              pch=pch)
        .setKey(Key3)
      }
    }
    
    ncont <- sum(type==2)
    if(which != 'categorical' && ncont) {
      mf <- par('mfrow')
      if(length(mf)==0) mf <- c(1,1)
      
      if(ncont > 1 & max(mf)==1) {
        mf <-      if(ncont <= 2) c(2,1)
        else if(ncont <= 4) c(2,2)
        else if(ncont <= 6) c(2,3)
        else if(ncont <= 9) c(3,3)
        else c(4,3)
        ## if(ncont <= 12)c(4,3) else if(ncont <= 16) c(4,4) else c(5,4)
        nr <- mf[1]
        m  <- par('mar')
        par(mfrow=mf)
      }
      
      npages <- npages + ceiling(sum(type==2) / prod(mf))
      
      for(i in (1:length(type))[type==2]) {
        nam <- labelPlotmath(vn[i], Units[i])
        st <- obj$stats[[i]]
        if(nw==1) st <- as.matrix(st)
        N <- st[,'N']
        
        if(conType=='dot') {
          quantile.columns <- dimnames(st)[[2]] %nin% c('Mean','SD','N')
          st <- st[,quantile.columns,drop=FALSE]
          xlim <- range(st)
          ns <- as.numeric(dimnames(st)[[2]])
          l  <- 1:length(ns)
          q1  <- l[abs(ns-.25) < .001]
          med <- l[abs(ns-.5)  < .001]
          q3  <- l[abs(ns-.75) < .001]
          st <- st[,c(q1,med,q3),drop=FALSE]
          dotchart3(st, xlim=xlim, xlab=nam, pch=c(91,16,93),
                    auxtitle='N', auxdata=N)
          
          Key2 <- function(x=NULL, y=NULL, quant, ...) {
            quant <- format(quant)
            txt <- paste('(0.25, 0.5, 0.75) quantiles shown\n',
                         'x-axes scaled to (',min(quant),',',
                         max(quant), ') quantiles', sep='')
            if(length(x)) {
              if(is.list(x)) {
                y <- x$y;
                x <- x$x
              }
              text(x,y,txt, cex=.8, adj=0, ...)
            } else mtitle(lr=txt, cex.l=.8, line=1, ...)
            
            invisible()
          }
          formals(Key2) <- list(x=NULL, y=NULL, quant=obj$quant)
          .setKey2(Key2)
          
        } else if(conType=='bp') {
          st <- st[,colnames(st) != 'N', drop=FALSE]
          mw <- max(strwidth(N, 'inches', cex=cex))
          omai <- par('mai')
          mai <- omai
          mai[4] <- .3 + 1.1*mw
          par(mai=mai)
          bpplt(st, xlab=nam, cex.points=cex.means)
          upedge <- par('usr')[4]
          outerText('N',
                    upedge + strheight('N', cex=cex)/2,
                    cex=cex)
          outerText(N, length(N):1, cex=cex)
          par(omai)
        }
        else
          stripChart(obj$data[[i]], xlab=nam)
        
        if(all(prtest != 'none')) {
          fts <- formatTestStats(test[[varNames[i]]], prtest=prtest,
                                 plotmath=TRUE,
                                 pdig=pdig, eps=eps)
          title(fts, line=.5)
        }
      }
    }
}  
  invisible(npages)
}

print.summaryM <- 
  function(x, digits, prn=any(n != N),
           what=c('proportion', '%'), pctdig=if(what=='%') 0 else 2, 
           npct=c('numerator','both','denominator','none'),
           exclude1=TRUE, vnames=c("labels","names"), prUnits=TRUE,
           sep="/", abbreviate.dimnames=FALSE, 
           prefix.width=max(nchar(lab)), 
           min.colwidth, formatArgs=NULL, round=NULL,
           prtest=c('P','stat','df','name'), prmsd=FALSE, long=FALSE,
           pdig=3, eps=0.001, ...)
{
  npct   <- match.arg(npct)
  vnames <- match.arg(vnames)
  what   <- match.arg(what)
  if(is.logical(prtest) && !prtest)
    prtest <- 'none'
  obj <- x
  for(strat in names(obj$results)) {
    x <- obj$results[[strat]]
    
    stats  <- x$stats
    nv     <- length(stats)
    cstats <- lab <- character(0)
    nn     <- integer(0)
    type   <- x$type
    n      <- x$n
    N      <- x$N
    nams   <- names(stats)
    labels <- x$labels
    Units  <- x$units
    test   <- x$testresults
    if(!length(test))
      prtest <- 'none'
    
    nw     <- if(lg <- length(x$group.freq)) lg
     else 1

    gnames <- names(x$group.freq)

    if(!missing(digits)) {
      oldopt <- options(digits=digits)
      on.exit(options(oldopt))
    }
    
    cstats <- NULL
    for(i in 1 : nv) {
      nn <- c(nn, n[i])
      nam <- if(vnames=="names") nams[i] else labels[i]
      if(prUnits && nchar(Units[i]))
        nam <- paste(nam,' [',translate(Units[i],'*',' '),']',sep='')
      
      tr <- if(length(test) && all(prtest!='none')) test[[nams[i]]]
      else NULL
      
      if(type[i]==1 || type[i]==3) {
        cs <- formatCats(stats[[i]], nam, tr, type[i],
                         if(length(x$group.freq)) x$group.freq else x$n[i],
                         what, npct, pctdig, exclude1, long, prtest,
                         pdig=pdig, eps=eps)
        nn <- c(nn, rep(NA, nrow(cs) - 1))
      } else cs <- formatCons(stats[[i]], nam, tr, x$group.freq, prmsd,
                              sep, formatArgs, round, prtest,
                              pdig=pdig, eps=eps)
      
      cstats <- rbind(cstats, cs)
    }
    
    lab <- dimnames(cstats)[[1]]
    gl <- names(x$group.freq)
    gl <- if(length(gl)) paste(gl," \n(N=",x$group.freq,")",sep="")
    else ""
    
    if(length(test) && !all(prtest=='none'))
      gl <- c(gl,
              if(length(prtest)==1 && prtest!='stat')
              if(prtest=='P')'P-value'
              else prtest
              else '  Test\nStatistic')
    
    nc <- nchar(cstats)
    spaces <- substring("                                                        ",
                        1, (max(nc) - nc + 1) / 2)   ## center strings
    dc <- dim(cstats)
    cstats <- paste(spaces, cstats, sep="")
    dim(cstats) <- dc
    if(prn) {
      cnn <- format(nn)
      cnn[is.na(nn)] <- ''
      cstats <- cbind(cnn, cstats)
      gl <- c('N', gl)
    }

    cstats <- rbind(gl, cstats)
    dimnames(cstats) <- list(c('',lab), NULL)

    if(strat != '.ALL.')
      cat('\n', strat, '\n', sep='')
    cat("\n\nDescriptive Statistics",
        if(length(x$group.label))
        paste(" by",x$group.label)
        else
        paste("  (N=",x$N,")",sep=""),"\n\n", sep="")
    
    if(missing(min.colwidth))
      min.colwidth <- max(min(nchar(gl)), min(nc[nc > 0]))
    
    print.char.matrix(cstats, col.names=FALSE,
                      col.txt.align='left', ...)
  }
  invisible(cstats)
}

latex.summaryM <- 
  function(object, title=first.word(deparse(substitute(object))),
           file=paste(title, 'tex', sep='.'), append=FALSE,
           digits, prn = any(n!=N),
           what=c('proportion', '%'), pctdig=if(what=='%') 0 else 2, 
           npct=c('numerator','both','denominator','none'),
           npct.size='scriptsize', Nsize='scriptsize',
           exclude1=TRUE,  vnames=c("labels","names"), prUnits=TRUE,
           middle.bold=FALSE, outer.size="scriptsize",
           caption, rowlabel="",
           insert.bottom=TRUE, dcolumn=FALSE, formatArgs=NULL, round=NULL,
           prtest=c('P','stat','df','name'), prmsd=FALSE, msdsize=NULL,
           long=FALSE, pdig=3, eps=.001, auxCol=NULL, table.env=TRUE, ...)
{
  if(! append) cat('', file=file)
  append <- TRUE
  what   <- match.arg(what)
  npct   <- match.arg(npct)
  vnames <- match.arg(vnames)
  if(is.logical(prtest) && !prtest) prtest <- 'none'
  strats <- names(object$results)
  
  for(strat in strats) {
    x <- object$results[[strat]]
    
    stats  <- x$stats
    nv     <- length(stats)
    cstats <- lab <- character(0)
    nn     <- integer(0)
    type   <- x$type
    n      <- x$n
    N      <- x$N
    nams   <- names(stats)
    labels <- x$labels
    Units  <- x$units
    nw     <- if(lg <- length(x$group.freq)) lg else 1
    gnames <- names(x$group.freq)
    test   <- x$testresults
    if(!length(test))
      prtest <- 'none'
    
    gt1.test <-
      if(all(prtest=='none'))
        FALSE
      else
        length(unique(sapply(test,function(a)a$testname))) > 1

    if(!missing(digits)) {   #.Options$digits <- digits 6Aug00
      oldopt <- options(digits=digits)
      on.exit(options(oldopt))
    }
    
    if(missing(caption))
      caption <- paste("Descriptive Statistics",
                       if(length(x$group.label))
                        paste(" by",x$group.label)
                       else
                        paste("  $(N=",x$N,")$",sep=""), sep="")
    
    bld <- if(middle.bold) '\\bf '
     else ''

    cstats <- NULL
    testUsed <- auxc <- character(0)

    for(i in 1:nv) {
      if(length(auxCol))
        auxc <- c(auxc, auxCol[[1]][i])

      nn <- c(nn, n[i])
      nam <- if(vnames=="names") nams[i]
      else labels[i]
      
      if(prUnits && nchar(Units[i]) > 0)
        nam <- paste(nam, '~\\hfill\\tiny{',translate(Units[i],'*',' '),'}',sep='')
      
      tr  <- if(length(test) && all(prtest!='none')) test[[nams[i]]]
       else NULL
      
      if(length(test) && all(prtest!='none'))
        testUsed <- unique(c(testUsed, tr$testname))

      if(type[i]==1 || type[i]==3) {
        cs <- formatCats(stats[[i]], nam, tr, type[i],
                         if(length(x$group.freq)) x$group.freq else x$n[i],
                         what, npct, pctdig, exclude1, long, prtest,
                         latex=TRUE, testUsed=testUsed,
                         npct.size=npct.size,
                         pdig=pdig, eps=eps,
                         footnoteTest=gt1.test)
        nn <- c(nn, rep(NA, nrow(cs) - 1))
      } else cs <- formatCons(stats[[i]], nam, tr, x$group.freq, prmsd,
                              prtest=prtest, formatArgs=formatArgs, round=round,
                              latex=TRUE, testUsed=testUsed,
                              middle.bold=middle.bold,
                              outer.size=outer.size, msdsize=msdsize,
                              pdig=pdig, eps=eps, footnoteTest=gt1.test)
      
      cstats <- rbind(cstats, cs)
      if(length(auxc) && nrow(cstats) > 1)
        auxc <- c(auxc, rep(NA, nrow(cs)-1))
    }
    
    lab <- dimnames(cstats)[[1]]
    gl <- names(x$group.freq)
    if(!length(gl)) gl <- " "
    
    lab <- sedit(lab,c(" ","&"),c("~","\\&"))  #was format(lab) 21Jan99
    lab <- latexTranslate(lab, greek=TRUE)
    gl  <- latexTranslate(gl, greek=TRUE)
    extracolheads <-
      if(any(gl != " "))
        c(if(prn)'', paste('$N=',x$group.freq,'$',sep=''))
      else NULL
    
    if(length(test) && !all(prtest=='none')) {
      gl <- c(gl,
              if(length(prtest)==1 && prtest!='stat')
              if(prtest=='P') 'P-value'
              else prtest
              else 'Test Statistic')
      
      if(length(extracolheads)) extracolheads <- c(extracolheads,'')
    }
    
    dimnames(cstats) <- list(NULL,gl) 
    cstats <- data.frame(cstats, check.names=FALSE, stringsAsFactors=FALSE)
    
    col.just <- rep("c",length(gl))
    if(dcolumn && all(prtest!='none') &&
       gl[length(gl)] %in% c('P-value','Test Statistic'))
      col.just[length(col.just)] <- '.'
    
    if(prn) {
      cstats <- data.frame(N=nn, cstats, check.names=FALSE,
                           stringsAsFactors=FALSE)
      col.just <- c("r",col.just)
    }
    
    if(is.logical(insert.bottom) && !insert.bottom)
      legend <- NULL
    else {
      legend <- character()
      if(any(type==2)) {
        legend <- paste("\n\\begin{minipage}[t]{3.75in}{\\", outer.size,
                        " $a$\\ }{", bld,
                        "$b$\\ }{\\", outer.size,
                        " $c$\\ } represent the lower quartile $a$, the median $b$, and the upper quartile $c$\\ for continuous variables.",
                        if(prmsd) '~~$x\\pm s$ represents $\\bar{X}\\pm 1$ SD.'
                        else '',
                        sep="")
      }
      
      if(prn) {
        legend <- c(legend, '$N$\\ is the number of non--missing values.')
      }
      
      if(any(type==1) && npct=='numerator') {
        legend <- c(legend, 'Numbers after percents are frequencies.')
      }
      
      if(length(testUsed))
        legend <-c(legend,
                   if(length(testUsed) == 1)'\\noindent Test used:'
                   else '\\indent Tests used:',
                   if(length(testUsed) == 1) paste(testUsed,'test')
                   else paste(paste('\\textsuperscript{\\normalfont ',
                                    1:length(testUsed),'}',testUsed,
                                    ' test',sep=''),collapse='; '))
      legend <- c(legend, '\\end{minipage}%')

    }

    if(length(auxc)) {
      if(length(auxc) != nrow(cstats))
        stop(paste('length of auxCol (',length(auxCol[[1]]),
                   ') is not equal to number or variables in table (',
                   nv,').', sep=''))
      auxcc <- format(auxc)
      auxcc[is.na(auxc)] <- ''
      cstats <- cbind(auxcc, cstats)
      nax <- names(auxCol)
      heads <- get2rowHeads(nax)
      names(cstats)[1] <- heads[[1]]
      if(length(col.just)) col.just <- c('r', col.just)
      if(length(extracolheads)) extracolheads <- c(heads[2], extracolheads)
    }
    if(length(legend) && ! table.env) legend[1] <- paste('\n', legend[1], sep='')
    w <- latex.default(cstats, title=title, file=file, append=TRUE,
                       caption=if(table.env) caption,
                       rowlabel=rowlabel, table.env=table.env,
                       col.just=col.just, numeric.dollar=FALSE, 
                       insert.bottom=if(strat==strats[length(strats)])
                         legend,
                       rowname=lab, dcolumn=dcolumn,
                       extracolheads=extracolheads, extracolsize=Nsize,
                       insert.top=if(strat != '.ALL.') strat,
                       ...)
  }
  w
}
