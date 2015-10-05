biVar <- function(formula, statinfo, data=NULL, subset=NULL,
                  na.action=na.retain, exclude.imputed=TRUE, ...)
{
  call <- match.call()
  x <- do.call('model.frame',
               list(formula, data=data, subset=subset, na.action=na.action))
  nam <- names(x); yname <- nam[1]
  y <- x[[1]]
  ylabel <- label(y)
  x <- x[-1]
  xlabel <- sapply(x, label)
  m <- ncol(x)
  statnames <- statinfo$names
  stats <- matrix(NA, nrow=m, ncol=length(statnames),
                  dimnames=list(names(x), statnames))
  nmin <- statinfo$nmin
  fun  <- statinfo$fun
  
  N <- integer(m)
  yna <- if(is.matrix(y))is.na(y %*% rep(1,ncol(y))) else is.na(y)
  for(i in 1:m) {
    w <- x[[i]]
    j <- !(yna | is.na(w))
    if(exclude.imputed) j <- j & !(is.imputed(w) | is.imputed(y))
    yy <- if(is.matrix(y)) y[j,,drop=FALSE] else y[j]
    w <- w[j]
    N[i] <- length(w)
    stats[i,] <- if(N[i] >= nmin) fun(w, yy, ...) else
     rep(NA, length(statnames))
  }
  stats <- cbind(stats, n=N)
  structure(stats, class='biVar', yname=yname,
            ylabel=ylabel, xlabel=xlabel,
            statinfo=statinfo, call=call)
}

print.biVar <- function(x, ...) {
  info  <- attr(x, 'statinfo')
  yname <- attr(x, 'yname')
  cat('\n', info$title, '    Response variable:', yname, '\n\n', sep='')

  dig <- c(info$digits,0)
  for(i in 1:ncol(x))
    x[,i] <- round(x[,i],dig[i])
  
  attr(x,'yname') <- attr(x, 'statinfo') <- attr(x, 'call') <-
    attr(x, 'ylabel') <- attr(x, 'xlabel') <- class(x) <- NULL
  print(x)
  invisible()
}


plot.biVar <- function(x,
                       what=info$defaultwhat,
                       sort.=TRUE,
                       main, xlab,
                       vnames=c('names','labels'), ...) {

  vnames <- match.arg(vnames)
  yname <- attr(x, 'yname')
  ylabel <- attr(x, 'ylabel')
  if(vnames=='labels' && ylabel!='') yname <- sedit(ylabel, ' ', '~')
  xlabel <- attr(x, 'xlabel')
  info  <- attr(x, 'statinfo')
  aux   <- info$aux
  auxlabel <- info$auxlabel
  if(!length(auxlabel)) auxlabel <- aux
  
  i <- match(what, info$names)
  if(is.na(i)) stop(paste('what must be one of',
                          paste(info$names,collapse=' ')))
  if(missing(xlab))
    xlab <- info$rxlab[i]
  if(missing(main)) main <-
    parse(text=paste(as.character(info$rmain),'~~~~Response:',
            yname,sep=''))
  auxtitle <- 'N'; auxdata <- format(x[,'n'])
  if(length(aux)) {
    auxtitle <- paste('N', auxlabel, sep='  ')
    auxdata  <- paste(format(x[,'n']), format(x[,aux]))
  }
  stat <- x[,what]
  if(vnames=='labels')
    names(stat) <- ifelse(xlabel=='', names(stat), xlabel)
  if(sort.) {
    i <- order(stat)
    stat <- stat[i]
    auxdata <- auxdata[i]
  }
  dotchart3(stat, auxdata=auxdata,
            xlab=xlab, auxtitle=auxtitle,
            main=main, ...)
  invisible()
}

chiSquare <- function(formula, data=NULL, subset=NULL, na.action=na.retain,
                      exclude.imputed=TRUE, ...) {
  
g <- function(x, y, minlev=0, g=3) {
  if(minlev) y <- combine.levels(y, minlev=minlev)
  if((is.character(x) || is.factor(x)) && minlev)
      x <- combine.levels(x, minlev=minlev)
  if(is.numeric(x) && length(unique(x)) > g) x <- cut2(x, g=g)
  ct <- chisq.test(x, y)
  chisq <- ct$statistic
  df    <- ct$parameter
  pval  <- ct$p.value
  c(chisq, df, chisq-df, pval)
}

statinfo <- list(fun=g,
                 title='Pearson Chi-square Tests',
                 main='Pearson Chi-squared',
                 rmain=expression(Pearson~chi^2),
                 names=c('chisquare','df','chisquare-df','P'),
                 xlab=c('Chi-square','d.f.','Chi-square - d.f.','P-value'),
                 rxlab=expression(chi^2, d.f., chi^2 - d.f., P-value),
                 digits=c(2,0,2,4),
                 aux='df', nmin=2, defaultwhat='chisquare-df')

biVar(formula, statinfo=statinfo, data=data, subset=subset,
      na.action=na.action, exclude.imputed=TRUE, ...)
}

spearman2 <- function(x, ...) UseMethod("spearman2") 

spearman2.default <- function(x, y, p=1, minlev=0,
                              na.rm=TRUE, exclude.imputed=na.rm, ...)
{
  if(p > 2)
    stop('p must be 1 or 2')
  
  
  y <- as.numeric(y)
  if(is.character(x))
    x <- factor(x)

  if(na.rm) {
    s <- !(is.na(x) | is.na(y))
    if(exclude.imputed) {
      im <- is.imputed(x) | is.imputed(y)
      s <- s & !im
    }
    x <- x[s]; y <- y[s]
  }
  n <- length(x)
  
  ## If number of non-NA values is less then 3 then return a NA
  ## value.
  if(n < 3)
    return(c(rho2=NA,F=NA,df1=0,df2=n,P=NA,n=n,'Adjusted rho2'=NA))

  ## Find the number of unique values in x
  u <- length(unique(x))

  ## If is a factor and unique values are greater then 2 then find the
  ## lm.fit.qr.bare without an intercept.
  if(is.factor(x) && u > 2) {
    if(minlev > 0) {
      x <- combine.levels(x, minlev)
      if(length(levels(x))<2) {
        warning(paste('x did not have >= 2 categories with >=',
                      minlev,'of the observations'))
        return(c(rho2=NA,F=NA,df1=0,df2=n,P=NA,n=n,'Adjusted rho2'=NA))
      }
    }
    
    x <- model.matrix(~x, data=data.frame(x))
    p <- ncol(x)-1
    rsquare <- lm.fit.qr.bare(x, rank(y), intercept=FALSE)$rsquared
  } else {
    x <- as.numeric(x)
    if(u < 3)
      p <- 1
    
    x <- rank(x)
    rsquare <-
      if(p==1)
        cor(x, rank(y))^2
      else {
        x <- cbind(x, x^2)
        lm.fit.qr.bare(x, rank(y), intercept=TRUE)$rsquared
      }
  }
  
  df2 <- n-p-1
  fstat <- rsquare/p/((1-rsquare)/df2)
  pvalue <- 1-pf(fstat,p,df2)
  rsqa <- 1 - (1 - rsquare)*(n-1)/df2
  
  x <- c(rsquare,fstat,p,df2,pvalue,n,rsqa)
  names(x) <- c("rho2","F","df1","df2","P","n","Adjusted rho2")
  x
}

spearman2.formula <- function(formula, data=NULL, subset=NULL,
                              na.action=na.retain,
                              exclude.imputed=TRUE, ...)
{
  g <- function(x, y, p=1, minlev=0)
    spearman2(x, y, p=p, minlev=minlev, na.rm=FALSE)[-6]
    
statinfo <- list(fun=g,
                 title='Spearman rho^2',
                 main='Spearman rho^2',
                 rmain=expression(Spearman~rho^2),
                 names=c('rho2','F','df1','df2','P','Adjusted rho2'),
                 xlab=c('rho^2','F','df2','df2','P-value','Adjusted rho^2'),
                 rxlab=expression(rho^2, F, df1, df2, P-value, Adjusted~rho^2),
                 digits=c(3,2,0,0,4,3),
                 aux='df1', auxlabel='df', nmin=2, defaultwhat='Adjusted rho2')

biVar(formula, statinfo=statinfo, data=data, subset=subset,
      na.action=na.action, exclude.imputed=exclude.imputed, ...)
}


rcorrcens <- function(x, ...) UseMethod("rcorrcens") 

rcorrcens.formula <- function(formula, data=NULL, subset=NULL,
                              na.action=na.retain,
                              exclude.imputed=TRUE, outx=FALSE, ...)
{
  g <- function(x, y, outx)
    {
      lev <- levels(x)
      if(is.factor(x) && length(lev)==2) x <- as.integer(x)
      
      u <- if(is.factor(x))
        {
          i <- order(-table(x))
          u <- rcorr.cens(1*(x==lev[i[1]]), y, outx=outx)
          v <- rcorr.cens(1*(x==lev[i[2]]), y, outx=outx)
          if(abs(v['Dxy']) > abs(u['Dxy'])) v else u
        }
      else rcorr.cens(x, y, outx=outx)
      Dxy <- u['Dxy']
      SE <- u['S.D.']
      aDxy <- abs(Dxy)
      z <- aDxy/SE
      P <- 2 * pnorm(- z)
      c(C=u['C Index'], Dxy=Dxy, aDxy=aDxy, SD=SE, Z=z, P=P)
    }
  
statinfo <- list(fun=g,
                 title="Somers' Rank Correlation for Censored Data",
                 main="Somers' Rank Correlation",
                 rmain=expression(paste("Somers' ", D[xy])),
                 names=c('C','Dxy','aDxy','SD','Z','P'),
                 xlab=c('C','Dxy','|Dxy|','SD','Z','P-value'),
                 rxlab=expression(C-index, D[xy], paste('|',D[xy],'|'), SD, Z, P-value),
                 digits=c(3,3,3,3,2,4),
#                 aux='n', auxlabel='N',
                 nmin=2, defaultwhat='aDxy')

biVar(formula, statinfo=statinfo, data=data, subset=subset,
      na.action=na.action, exclude.imputed=exclude.imputed, outx=outx, ...)
}
