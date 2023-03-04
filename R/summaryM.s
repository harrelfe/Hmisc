summaryM <- function(formula, groups=NULL, data=NULL, subset,
                     na.action=na.retain, 
                     overall=FALSE, continuous=10, na.include=FALSE,
                     quant=c(0.025, 0.05, 0.125, 0.25, 0.375, 0.5, 0.625,
                       0.75, 0.875, 0.95, 0.975),
                     nmin=100, test=FALSE,
                     conTest=conTestkw, catTest=catTestchisq,
                     ordTest=ordTestpo) {

  marg <- length(data) && '.marginal.' %in% names(data)
  if(marg) formula <- update(formula, .~. + .marginal.)
  
  formula <- Formula::Formula(formula)
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

   X <- Formula::model.part(formula, data=Y, rhs=1)
   Y <- Formula::model.part(formula, data=Y, lhs=1)

   getlab <- function(x, default) {
     lab <- attr(x, 'label')
     if(!length(lab) || lab=='') default else lab
   }

  if(marg) {
    xm <- X$.marginal.
    X$.marginal. <- NULL
  } else xm <- rep('', nrow(X))
  
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
     group <- rep('', nrow(Y))  # y1 + y2 ~ 1, no grouping
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
    xms <- xm[instrat]
    ## Need to ignore marginal summaries in N unless stratifying by
    ## the variable that is marginalized over
    if(all(xms != '')) xms <- rep('', length(xms))
    group.freq <- table(gr)
    group.freq <- group.freq[group.freq > 0]
    if(overall) group.freq <- c(group.freq, Combined=sum(group.freq))
      
    for(i in 1 : nv) {
      w  <- Y[instrat, i]
      
      if(length(attr(w, "label")))
        labels[i] <- attr(w, "label")
      
      if(length(attr(w, 'units'))) Units[i]  <- attr(w, 'units')

      if(is.character(w))
        w <- as.factor(w)
      
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

        n[i] <- sum(s & xms == '')
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
            ## Won't lose precision in quantile names with digits=15
            y <- c(quantile(x,quant), Mean=mean(x), SD=sqrt(var(x)),
                   N=sum(!is.na(x)))
            names(y) <-
              c(paste0(formatC(100 * quant, format='fg', width=1, digits=15),
                       '%'), 'Mean', 'SD', 'N')
            y
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
        ## n[i] <- nrow(w)
        n[i] <- sum(! is.na(apply(w, 1, sum)) & xms == '')
        g    <- as.factor(gr)
        ncat <- ncol(w)
        tab  <- matrix(NA, nrow=ncat, ncol=length(levels(g)),
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
            st   <- catTest(tabj)
            pval[j] <- st$P
            stat[j] <- st$stat
            d.f.[j] <- st$df
          }
        }
        if(test)
          testresults[[i]] <- list(P=pval,
                                   stat        = stat,
                                   df          = d.f.,
                                   testname    = st$testname,
                                   namefun     = st$namefun,
                                   statname    = st$statname,
                                   latexstat   = st$latexstat,
                                   plotmathstat= st$plotmathstat)
                                   
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
                     N=sum(!is.na(gr) & xms ==''), n=n,
                     testresults=if(test)testresults)
  }
  structure(list(results=R, group.name=groups, group.label=glabel,
                 call=call, formula=formula), 
            class="summaryM")
}

plot.summaryM <-
  function(x, vnames = c('labels', 'names'), 
           which = c('both', 'categorical', 'continuous'),
           vars = NULL,
           xlim = c(0,1),
           xlab = 'Proportion',
           pch = c(16, 1, 2, 17, 15, 3, 4, 5, 0), exclude1 = TRUE,
           main, ncols=2,
           prtest = c('P', 'stat', 'df', 'name'), pdig = 3, eps = 0.001,
           conType = c('bp', 'dot', 'raw'),
           cex.means = 0.5, cex=par('cex'),
           height='auto', width=700, ...)
{
  obj <- x
  vnames  <- match.arg(vnames)
  which   <- match.arg(which)
  conType <- match.arg(conType)

  if(grType() == 'plotly')
    return(plotpsummaryM(x, vnames=vnames, which=which, vars=vars,
                         xlim=xlim,
                         xlab=xlab, exclude1=exclude1, ncols=ncols,
                         prtest=prtest, pdig=3, eps=0.001,
                         height=height, width=width))

  html    <- FALSE
  
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
       paste('Proportions',
             'Stratified by',
             x$group.label)
             
    pch     <- rep(pch, length.out=nw)
  
    lab <- vnd <- z <- nmiss <- vnamd <- NULL
    type  <- obj$type; n <- obj$n

    opar <- par()
    on.exit(setParNro(opar))

    npages <- 0
  
    if(which != 'continuous' && any(type %in% c(1, 3))) {
      ftstats <- NULL  
      for(i in (1:length(type))[type %in% c(1, 3)]) {
        nam <- vn[i]
        tab <- obj$stats[[i]]
        if(nw == 1)
          tab <- as.matrix(tab)

        nr <- nrow(tab)
        denom <- if(type[i] == 1) apply(tab, 2, sum)
        else obj$group.freq

        y <- sweep(tab, 2, denom, FUN='/')

        lev <- dimnames(y)[[1]]
        exc <- exclude1 && (nr == 2)
        jstart <- if(exc) 2 else 1

        rl <- casefold(lev)
        binary <- type[i] == 1 && exc &&
          (all(rl %in% c("0", "1")) | all(rl %in% c("false", "true"))|
           all(rl %in% c("absent", "present")))

        for(j in jstart : nrow(y)) {
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
          fts <- formatTestStats(test[[varNames[i]]], type[i] == 3,
                                 if(type[i] == 1) 1
                                 else 1 : nr,
                                 prtest  = prtest,
                                 plotmath= TRUE,
                                 pdig=pdig, eps=eps)
          ftstats <- c(ftstats, fts, 
                       if(type[i] == 1 && nr - exc - 1 > 0)
                       rep(expression(''), nr - exc - 1))
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
		  oldpar <- par('usr', 'xpd')
          par(usr=c(0, 1, 0, 1), xpd=NA)
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
      if(length(mf) == 0) mf <- c(1, 1)
      
      if(ncont > 1 & max(mf) == 1) {
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
      
      npages <- npages + ceiling(sum(type == 2) / prod(mf))
      
      for(i in (1 : length(type))[type == 2]) {
        nam <- labelPlotmath(vn[i], Units[i])
        st <- obj$stats[[i]]
        if(nw==1) st <- as.matrix(st)
        N <- st[, 'N']
        
        if(conType == 'dot') {
          quantile.columns <- dimnames(st)[[2]] %nin% c('Mean', 'SD', 'N')
          st <- st[,quantile.columns, drop=FALSE]
          xlim <- range(st)
          ns <- as.numeric(dimnames(st)[[2]])
          l  <- 1 : length(ns)
          q1  <- l[abs(ns - .25) < .001]
          med <- l[abs(ns - .5)  < .001]
          q3  <- l[abs(ns - .75) < .001]
          st <- st[,c(q1, med, q3), drop=FALSE]
          dotchart3(st, xlim=xlim, xlab=nam, pch=c(91, 16, 93),
                    auxtitle='N', auxdata=N)
          
          Key2 <- function(x=NULL, y=NULL, quant, ...) {
            quant <- format(quant)
            txt <- paste0('(0.25, 0.5, 0.75) quantiles shown\n',
                          'x-axes scaled to (',min(quant),',',
                          max(quant), ') quantiles')
            if(length(x)) {
              if(is.list(x)) {
                y <- x$y;
                x <- x$x
              }
              text(x, y, txt, cex=.8, adj=0, ...)
            } else mtitle(lr=txt, cex.l=.8, line=1, ...)
            
            invisible()
          }
          formals(Key2) <- list(x=NULL, y=NULL, quant=obj$quant)
          .setKey2(Key2)
          
        } else if(conType == 'bp') {
          st <- st[, colnames(st) != 'N', drop=FALSE]
          mw <- max(strwidth(N, 'inches', cex=cex))
          omai <- par('mai')
          mai <- omai
          mai[4] <- .3 + 1.1 * mw
          par(mai=mai)
          bpplt(st, xlab=nam, cex.points=cex.means)
          upedge <- par('usr')[4]
          outerText('N',
                    upedge + strheight('N', cex=cex) / 2,
                    cex=cex)
          outerText(N, length(N) : 1, cex=cex)
          par(mai=omai)
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

print.summaryM <- function(...) {
  lang <- prType()
  switch(lang,
         plain = printsummaryM(...),
         latex = latex.summaryM(...),
         html  = latex.summaryM(..., html=TRUE) )
  }
         
printsummaryM <- function(x, digits, prn=any(n != N),
           what=c('proportion', '%'), pctdig=if(what == '%') 0 else 2, 
           npct=c('numerator', 'both', 'denominator', 'none'),
           exclude1=TRUE, vnames=c("labels", "names"), prUnits=TRUE,
           sep="/", abbreviate.dimnames=FALSE, 
           prefix.width=max(nchar(lab)), 
           min.colwidth, formatArgs=NULL, round=NULL,
           prtest=c('P', 'stat', 'df', 'name'), prmsd=FALSE, long=FALSE,
           pdig=3, eps=0.001, prob=c(0.25, 0.5, 0.75), prN=FALSE, ...)
{
  npct   <- match.arg(npct)
  vnames <- match.arg(vnames)
  what   <- match.arg(what)
  if(is.logical(prtest) && !prtest) prtest <- 'none'
  obj <- x
  if(! length(obj$results)) return()
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
    if(!length(test)) prtest <- 'none'
    
    nw     <- if(lg <- length(x$group.freq)) lg else 1

    gnames <- names(x$group.freq)

    if(!missing(digits)) {
	  oldopt <- options('digits')
      options(digits=digits)
      on.exit(options(oldopt))
    }
    
    cstats <- NULL
    for(i in 1 : nv) {
      nn <- c(nn, n[i])
      nam <- if(vnames == "names") nams[i] else labels[i]
      if(prUnits && nchar(Units[i]))
        nam <- paste0(nam,' [', gsub('\\*', ' ', Units[i]),']')
      
      tr <- if(length(test) && all(prtest != 'none')) test[[nams[i]]]
      else NULL
      
      if(type[i] %in% c(1, 3)) {
        cs <- formatCats(stats[[i]], nam, tr, type[i],
                         if(length(x$group.freq)) x$group.freq else x$n[i],
                         what, npct, pctdig, exclude1, long, prtest,
                         pdig=pdig, eps=eps)
        nn <- c(nn, rep(NA, nrow(cs) - 1))
      } else cs <- formatCons(stats[[i]], nam, tr, x$group.freq, prmsd,
                              sep, formatArgs, round, prtest,
                              pdig=pdig, eps=eps, prob=prob, prN=prN)
      
      cstats <- rbind(cstats, cs)
    }
    
    lab <- dimnames(cstats)[[1]]
    gl <- names(x$group.freq)
    gl <- if(length(gl)) paste0(gl," \n(N=", x$group.freq, ")")
    else ""
    
    if(length(test) && !all(prtest == 'none'))
      gl <- c(gl,
              if(length(prtest) == 1 && prtest != 'stat')
              if(prtest == 'P')'P-value'
              else prtest
              else '  Test\nStatistic')
    
    nc <- nchar(cstats)
    spaces <- substring("                                                        ",
                        1, (max(nc) - nc + 1) / 2)   ## center strings
    dc <- dim(cstats)
    cstats <- paste0(spaces, cstats)
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
        paste(" by", x$group.label)
        else
        paste0("  (N=", x$N,")"), "\n\n", sep="")
    
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
           what=c('proportion', '%'), pctdig=if(what == '%') 0 else 2, 
           npct=c('numerator', 'both', 'denominator', 'slash', 'none'),
           npct.size=if(html) mspecs$html$smaller else 'scriptsize',
           Nsize    =if(html) mspecs$html$smaller else 'scriptsize',
           exclude1=TRUE,  vnames=c("labels","names"), prUnits=TRUE,
           middle.bold=FALSE,
           outer.size=if(html) mspecs$html$smaller else 'scriptsize',
           caption, rowlabel="", rowsep=html,
           insert.bottom=TRUE, dcolumn=FALSE, formatArgs=NULL, round=NULL,
           prtest=c('P', 'stat', 'df', 'name'), prmsd=FALSE,
           msdsize=if(html) function(x) x else NULL, brmsd=FALSE,
           long=FALSE, pdig=3, eps=.001, auxCol=NULL, table.env=TRUE,
           tabenv1=FALSE, prob=c(0.25, 0.5, 0.75), prN=FALSE,
           legend.bottom=FALSE, html=FALSE, mspecs=markupSpecs, ...)
{
  if(! length(object$results)) return()
  if(! append) cat('', file=file)
  append <- TRUE
  what   <- match.arg(what)
  npct   <- match.arg(npct)
  vnames <- match.arg(vnames)
  if(is.logical(prtest) && ! prtest) prtest <- 'none'
  strats <- names(object$results)
  probdef <- c(0.25, 0.5, 0.75)
  if(length(prob) != 3) {
    prob <- probdef
  }

  lang    <- if(html) 'html' else 'latex'
  specs   <- mspecs[[lang]]
  math    <- specs$math
  spc     <- specs$lspace
  bold    <- specs$bold
  sup     <- specs$sup
  br      <- specs$br
  plminus <- specs$plminus

  Npct.size <- npct.size; NNsize <- Nsize; Outer.size <- outer.size
  if(! is.function(npct.size))
    npct.size <- function(x)  paste0('{\\', Npct.size, ' ', x, '}')
  if(! is.function(Nsize))
    Nsize     <- function(x)  paste0('{\\', NNsize,    ' ', x, '}')
  if(! is.function(outer.size))
    outer.size <- function(x) paste0('{\\', Outer.size,' ', x, '}')

  ## Add this back if revert to previous chisq function in markupSpecs
  ## if(html) cat(specs$styles)   ## define html styles such as xscript for chisq

  maxlablen  <- 0
  istr       <- 0
  Lab        <- character(0)
  Cstats     <- NULL
  n.tspanner <- integer(0)

  for(strat in strats) {
    
    istr   <- istr + 1
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
    maxlablen <- max(maxlablen, nchar(labels))
    Units  <- x$units
    nw     <- if(lg <- length(x$group.freq)) lg else 1
    gnames <- names(x$group.freq)
    test   <- x$testresults
    if(!length(test)) prtest <- 'none'
    
    gt1.test <-
      if(all(prtest == 'none'))
        FALSE
      else
        length(unique(sapply(test,function(a)a$testname))) > 1

    if(!missing(digits)) {
	  oldopt <- options('digits')
      options(digits=digits)
      on.exit(options(oldopt))
    }
    
    if(missing(caption))
      caption <- paste0("Descriptive Statistics",
                       if(length(x$group.label))
                        paste(" by", x$group.label)
                       else
                        math(paste0("  (N=", x$N, ")")))
    
    if(! middle.bold) bold <- function(x) x

    cstats <- NULL
    testUsed <- auxc <- character(0)

    rows.per.var <- integer(0)
    
    for(i in 1:nv) {
      if(length(auxCol))
        auxc <- c(auxc, auxCol[[1]][i])

      nn <- c(nn, n[i])
      nam <- if(vnames == "names") nams[i]
      else labels[i]
      
      if(prUnits && nchar(Units[i]) > 0)
        nam <- specs$varlabel(nam, Units[i], hfill=TRUE)

      tr  <- if(length(test) && all(prtest != 'none')) test[[nams[i]]]
       else NULL
      
      if(length(test) && all(prtest != 'none'))
        testUsed <- unique(c(testUsed, tr$testname))

      if(type[i] %in% c(1, 3)) {
        cs <- formatCats(stats[[i]], nam, tr, type[i],
                         if(length(x$group.freq)) x$group.freq else x$n[i],
                         what, npct, pctdig, exclude1, long, prtest,
                         lang=lang, testUsed=testUsed,
                         npct.size=npct.size,
                         pdig=pdig, eps=eps,
                         footnoteTest=gt1.test)
        nn <- c(nn, rep(NA, nrow(cs) - 1))
        rows.per.var <- c(rows.per.var, nrow(cs))
      } else {
        cs <- formatCons(stats[[i]], nam, tr, x$group.freq, prmsd,
                         prtest=prtest, formatArgs=formatArgs, round=round,
                         lang=lang, testUsed=testUsed,
                         middle.bold=middle.bold,
                         outer.size=outer.size, msdsize=msdsize,
                         brmsd=brmsd,
                         pdig=pdig, eps=eps, footnoteTest=gt1.test,
                         prob=prob, prN=prN)
        rows.per.var <- c(rows.per.var, 1)
        }
      
      cstats <- rbind(cstats, cs)
      if(length(auxc) && nrow(cstats) > 1)
        auxc <- c(auxc, rep(NA, nrow(cs) - 1))
    }

    lab <- dimnames(cstats)[[1]]
    gl <- names(x$group.freq)
    if(!length(gl)) gl <- " "

    if(! html) {
      lab <- latexTranslate(lab, c(" "), c("~"), greek=TRUE)
      gl  <- latexTranslate(gl,  greek=TRUE)
      }

    extracolheads <-
      if(any(gl != " "))
        c(if(prn)'', math(paste0('N=', x$group.freq)))
      else NULL
    
    if(length(test) && !all(prtest == 'none')) {
      gl <- c(gl,
              if(length(prtest) == 1 && prtest != 'stat')
              if(prtest == 'P') 'P-value'
              else prtest
              else 'Test Statistic')
      
      if(length(extracolheads)) extracolheads <- c(extracolheads, '')
    }

    dimnames(cstats) <- list(NULL, gl)
    cstats <- data.frame(cstats, check.names=FALSE, stringsAsFactors=FALSE)
    if(length(gl) == 1 && gl == '') colnames(cstats) <- ' '   ## override V1
    
    col.just <- rep("c", length(gl))
    if(dcolumn && all(prtest != 'none') &&
       gl[length(gl)] %in% c('P-value', 'Test Statistic'))
      col.just[length(col.just)] <- '.'
    
    if(prn) {
      cstats <- data.frame(N=nn, cstats, check.names=FALSE,
                           stringsAsFactors=FALSE)
      col.just <- c("r", col.just)
    }

    noib <- is.logical(insert.bottom) && ! insert.bottom
    defs <- NULL
    if(is.character(insert.bottom)) defs <- insert.bottom
    else {

      if(any(type == 2)) {
        if(identical(prob, probdef))
          defs <- 
            paste0(outer.size(math('a')), ' ', bold(math('b')), ' ',
                   outer.size(math('c')), ' represent the lower quartile ',
                   math('a'), ', the median ', math('b'),
                   ', and the upper quartile ', math('c'),
                   ' for continuous variables.')
        else {
          prob <- sprintf("%1.0f\\%%", 100 * prob)
          defs <- 
            paste0(outer.size(math('a')), ' ', bold(math('b')), ' ',
                   outer.size(math('c')), 
                   ' represent the ', prob[1],
                   ' quantile ', math('a'), ' the ', prob[2],
                   ' quantile ', math('b'), ' and the ', prob[3],
                   ' quantile ', math('c'), ' for continuous variables.')
        }
        if(prmsd) defs <-
                    paste0(defs, spc,
                           if(html) paste0(math(paste0('x', specs$space,
                                                       plminus, ' s')),
                                           ' represents ', specs$xbar,
                                           specs$space, plminus, ' 1 SD.')
                           else
                             '$x\\pm s$ represents $\\bar{X}\\pm 1$ SD.')
      }
      if(prn)
        defs <- c(defs, if(length(defs)) spc,
                  paste0(math('N'), ' is the number of non-missing values.'))
      
      if(any(type == 1) && npct == 'numerator')
        defs <- c(defs, 'Numbers after proportions are frequencies.')
      
      if(length(testUsed)) {
        if(html)
          defs <- c(defs, br,
                    if(length(testUsed) == 1)'Test used:'
                    else 'Tests used:', 
                    if(length(testUsed) == 1) paste(testUsed, 'test')
                    else paste(paste0(sup(1 : length(testUsed)),
                                      testUsed,
                                      ' test'), collapse='; '), '.')
        else
          defs <-c(defs,
                   if(length(testUsed) == 1)'\\noindent Test used:'
                   else '\\indent Tests used:',
                   if(length(testUsed) == 1) paste(testUsed, 'test')
                   else paste(paste0('$^{', 1 : length(testUsed),'}$',
                                     testUsed, ' test'), collapse='; '), '.')
      }
    }

    legend <- NULL
    if(! html) legend <- defs
    else if(! noib)
      insert.bottom <- paste(defs, collapse=' ')

    if(length(auxc)) {
      if(length(auxc) != nrow(cstats))
        stop(paste0('length of auxCol (',length(auxCol[[1]]),
                    ') is not equal to number or variables in table (',
                    nv,').'))
      auxcc   <- format(auxc)
      auxcc[is.na(auxc)] <- ''
      cstats  <- cbind(auxcc, cstats)
      nax     <- names(auxCol)
      heads   <- get2rowHeads(nax)
      names(cstats)[1] <- heads[[1]]
      if(length(col.just)) col.just <- c('r', col.just)
      if(length(extracolheads)) extracolheads <- c(heads[2], extracolheads)
    }
    if(length(legend) && (html || ! table.env))
      legend[1] <- paste0('\n', legend[1])
    laststrat <- strat == strats[length(strats)]
    finalcaption <- NULL
    finallegend  <- NULL
    if((! tabenv1 && table.env) || (tabenv1 && istr == 1)) {
      finalcaption <- caption
      if(((! tabenv1 && laststrat) || (tabenv1 && istr == 1)) &&
         !legend.bottom) {
        finalcaption <- paste(finalcaption, paste(legend, collapse=' '),
                              sep='. ')
      }
    }
    if(! noib && laststrat && ! table.env) {
      finallegend <- legend
    } else if(legend.bottom) {
      finallegend <- paste(legend, collapse=' ')
    }

    if(html) {
      heads  <- colnames(cstats)
      if(length(extracolheads)) heads <- paste(heads, extracolheads, sep=br)
      Cstats <- rbind(Cstats, cstats)
      Lab    <- c(Lab, lab)
      n.tspanner <- c(n.tspanner, length(lab))
    }
    else {
      w <- latex(cstats, title=title, file=file, append=TRUE,
                 caption=finalcaption, rowlabel=rowlabel,
                 table.env=(! tabenv1 && table.env) || (tabenv1 && istr == 1),
                 col.just=col.just, numeric.dollar=FALSE, 
                 insert.bottom=finallegend, rowname=lab, dcolumn=dcolumn,
                 extracolheads=extracolheads, extracolsize=NNsize,
                 insert.top=if(strat != '.ALL.') strat,
                 ...)
      if(tabenv1 && istr == 1) cat('\\clearpage\n', file=file, append=TRUE)
      else if(istr < length(strats))
        cat('\\Needspace{2.7in}\n', file=file, append=TRUE)
      ## trieds to avoid page break in middle of stratum
      attr(w, 'legend') <- legend
      }
  }

  if(! html) {
    attr(w, 'nstrata') <- istr
    return(w)
  }

  cs <- c(paste0('width:', round(0.85*maxlablen), 'ex;'),
          rep('padding: 0 7px 0 7px;', ncol(Cstats)))
  ## was rep('padding-left:3ex;'...

  if(length(strats) > 1) {
    tspanner <- ifelse(strats == '.ALL', bold('Overall'), strats)
    w <- htmlTable::htmlTable(Cstats, header=heads,
                              caption  = paste(finalcaption, finallegend),
                              rowlabel = rowlabel,
#                              n.rgroup = if(rowsep) rows.per.var,
                              align    = col.just, rnames=Lab,
                              tspanner=tspanner, n.tspanner=n.tspanner,
                              tfoot=insert.bottom,
                              css.cell=cs, escape.html=FALSE)
  }
  else 
    w <- htmlTable::htmlTable(Cstats, header=heads,
                              caption  = paste(finalcaption, finallegend),
                              rowlabel = rowlabel,
 #                             n.rgroup = if(rowsep) rows.per.var,
                              align    = col.just, rnames=lab,
                              tfoot    = insert.bottom,
                              css.cell = cs, escape.html=FALSE)
  
  rendHTML(w)
}


html.summaryM <- 
  function(object, ...) latex.summaryM(object, file='', html=TRUE, ...)


plotpsummaryM <-
  function(x, vnames = c('labels', 'names'), 
           which = c('both', 'categorical', 'continuous'),
           vars=NULL, xlim = c(0,1), 
           xlab = 'Proportion',
           exclude1 = TRUE, main=NULL, ncols=2,
           prtest = c('P', 'stat', 'df', 'name'), pdig = 3, eps = 0.001,
           height=NULL, width=NULL)
{
    
  if (!requireNamespace("plotly"))
    stop("This function requires the 'plotly' package.")
    
  obj <- x
  vnames  <- match.arg(vnames)
  which   <- match.arg(which)

  html    <- TRUE
  
  ul <- vnames=='labels'

  if(is.logical(prtest) && !prtest) prtest <- 'none'

  stratnames  <- names(x$results)
  nstrat      <- length(stratnames)
  gcat        <- vector('list', nstrat)
  names(gcat) <- stratnames

  ## Create annotations to simulate strata titles for dot charts
#  if(nstrat > 1) {
#    annotations <- list()
#    xx <- (1 : nstrat) / (nstrat + 1)
#    for(i in 1 : nstrat)
#      annotations[[i]] <- list(x=xx[i], y=1.05, text=stratnames[i],
#                               xref='paper', yref='paper', showarrow=FALSE)
#    }

    for(strat in stratnames) {
    obj  <- x$results[[strat]]
    test <- obj$testresults
    if(!length(test)) prtest <- 'none'

    varNames <- names(obj$stats)
    vn <- if(ul) obj$labels
     else varNames
    
    Units <- obj$units
  
    nw     <- if(lg <- length(obj$group.freq)) lg
     else 1

    gnames <- names(obj$group.freq) 
    
    ggl <- obj$group.label

    if(! length(main)) main <-
     if(strat != '.ALL.') strat
     else if(nw == 1) ''
     else paste('Proportions', 'Stratified by', obj$group.label)

    lab <- vnd <- z <- Frac <- nmiss <- vnamd <- NULL
    type  <- obj$type; n <- obj$n

    gcon <- NULL

    iv <- which(type %in% c(1, 3))
    if(length(vars)) iv <- iv[intersect(vars, 1 : length(iv))]
    if(which != 'continuous' && length(iv)) {
      ftstats <- NULL
      for(i in iv) {
        nam <- vn[i]
        tab <- obj$stats[[i]]
        if(nw == 1)
          tab <- as.matrix(tab)
        
        nr <- nrow(tab)
        denom <- if(type[i] == 1) apply(tab, 2, sum)
                 else obj$group.freq
        
        y <- sweep(tab, 2, denom, FUN='/')
        frac <- sweep(tab, 2, denom,
                      FUN=markupSpecs$html$frac, size=95)
        dim(frac)      <- dim(y)       ## paste loses these
        dimnames(frac) <- dimnames(y)
        
        lev <- dimnames(y)[[1]]
        exc <- exclude1 && (nr == 2)
        jstart <- if(exc) 2 else 1
        
        rl <- casefold(lev)
        binary <- type[i] == 1 && exc &&
          (all(rl %in% c("0", "1")) | all(rl %in% c("false", "true"))|
           all(rl %in% c("absent", "present")))
        
        for(j in jstart : nrow(y)) {
          if(nw==1) {
            z    <- rbind(z, y[j,])
            Frac <- rbind(Frac, frac[j,])
            }
          else {
            yj               <- rep(NA, nw)
            names(yj)        <- gnames
            yj[names(y[j,])] <- y[j,]
            z                <- rbind(z, yj)
            fj               <- rep('', nw)
            names(fj)        <- gnames
            fj[names(frac[j,])] <- frac[j,]
            Frac             <- rbind(Frac, fj)
          }
          
          lab <- c(lab, if(binary) '' else lev[j])
          vnd <- c(vnd, nam)
          vnamd <- c(vnamd, varNames[i])
        }
        if(any(prtest != 'none')) {
          fts <- formatTestStats(test[[varNames[i]]], type[i] == 3,
                                 if(type[i] == 1) 1 else 1 : nr,
                                 prtest  = prtest,
                                 lang = 'html',
                                 pdig = pdig, eps=eps)
          ftstats <- c(ftstats, fts, 
                       if(type[i] == 1 &&  nr - exc - 1 > 0)
                       rep('', nr - exc - 1))
        }
      }
      dimnames(z) <- dimnames(Frac) <- list(lab, dimnames(z)[[2]])
      if(! any(prtest == 'none'))
        Frac[, 1] <- paste0(Frac[, 1], '<br>', ftstats)

      gcat[[strat]] <-
        dotchartp(z, groups=factor(vnd, levels=unique(vnd)),
                  xlab=xlab, xlim=xlim,
                  auxdata=Frac, auxwhere='hover',
                  dec=3,
                  height=if(length(height) && height == 'auto')
                           plotlyParm$heightDotchart(nrow(z)) else height,
                  width=width,
                  layoutattr=FALSE && nstrat > 1)
    }

    iv <- which(type == 2)
    if(length(vars)) iv <- iv[intersect(vars, 1 : length(iv))]
    if(which != 'categorical' && length(iv)) {
      if(nstrat > 1) warning('only plots last stratum for continuous variables')
      icon <- iv
      ii   <- 0
      p <- list()
      for(i in icon) {
        ii <- ii + 1
        nam <- markupSpecs$html$varlabel(vn[i], Units[i], size=73)
        st <- obj$stats[[i]]
        if(nw==1) st <- as.matrix(st)
        N <- st[, 'N']

        teststat <- if(all(prtest != 'none'))
                      formatTestStats(test[[varNames[i]]], prtest=prtest,
                                      lang='html',
                                      pdig=pdig, eps=eps)

        p[[ii]] <- bppltp(stats=st, xlab=nam, teststat=teststat,
                          showlegend=FALSE)

      }
      nrows <- ceiling(length(p) / ncols)
      gcon <- plotly::subplot(p, shareY=TRUE, nrows=nrows,
                              titleX=TRUE, margin=.1)
      if(FALSE) {
        if(! length(height)) height <- min(1000, 275 * nrows)
        if(! length(width))  width  <- min(900,  400 * ncols)
        gcon <- plotly::layout(gcon, height=height, width=width)
        ## and note: height and width are now arguments to plot_ly
        }
    }
  }
  if(! is.null(gcat)) {    # plotly objects have length 0
    gcat <- if(nstrat == 1) gcat[[1]]
            else
              plotly::subplot(gcat, shareY=TRUE,
                            titleX=TRUE, nrows=1, margin=.1)
#            else {
#              lo <- attr(gcat[[1]], 'layout')
#              gcat <- plotly::subplot(gcat, shareY=TRUE,
#                                      titleX=TRUE, nrows=1, margin=.1)
#              ann <- list()
#              for(i in 1 : nstrat)
#                ann[[i]] <- list(x= i / (nstrat + 1), y=1.05,
#                                 text=stratnames[i], showarrow=FALSE,
#                                 xref='paper', yref='paper')
#              lo$xaxis1 <- lo$xaxis
#              lo$xaxis1$title <- 'this is it'
#              lo$axis2 <- lo$axis1
#              lo <- c(lo, ann)
#              do.call(plotly::layout, lo)
#            }
  }
  
  if(! is.null(gcat) && ! is.null(gcon))   # plotly objects have length 0
    list(Categorical = gcat,
         Continuous  = gcon)
  else
    if(! is.null(gcat)) gcat
  else gcon
}

