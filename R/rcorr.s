rcorr <- function(x, y, type=c("pearson","spearman"))
{
  type <- match.arg(type)

  if(!missing(y))
    x <- cbind(x, y)
  
  x[is.na(x)] <- 1e30
  storage.mode(x) <- if(.R.)"double"
                     else "single"
  
  p <- as.integer(ncol(x))
  if(p<1)
    stop("must have >1 column")
  
  n <- as.integer(nrow(x))
  if(n<5)
    stop("must have >4 observations")
  
  h <-
    if(.R.)
      .Fortran("rcorr", x, n, p, itype=as.integer(1+(type=="spearman")),
               hmatrix=double(p*p), npair=integer(p*p),
               double(n), double(n),  double(n), double(n),
               double(n), integer(n), PACKAGE="Hmisc")
    else
      .Fortran("rcorr", x, n, p, itype=as.integer(1+(type=="spearman")),
               hmatrix=single(p*p), npair=integer(p*p),
               single(n), single(n),  single(n), single(n),
               single(n), integer(n))
  
  npair <- matrix(h$npair, ncol=p)
  h <- matrix(h$hmatrix, ncol=p)
  h[h>1e29] <- NA
  nam <- dimnames(x)[[2]]
  dimnames(h) <- list(nam, nam)
  dimnames(npair) <- list(nam, nam)
  P <- matrix(2*(1-pt(abs(h)*sqrt(npair-2)/sqrt(1-h*h), npair-2)),ncol=p)
  P[abs(h)==1] <- 0
  diag(P) <- NA
  dimnames(P) <- list(nam,nam)
  structure(list(r=h, n=npair, P=P), class="rcorr")
}


print.rcorr <- function(x, ...)
{
  print(round(x$r,2))
  n <- x$n
  if(all(n==n[1,1]))
    cat("\nn=",n[1,1],"\n\n")
  else {
    cat("\nn\n")
    print(n)
  }
  
  cat("\nP\n")
  P <- x$P
  P <- ifelse(P<.0001,0,P)
  p <- format(round(P,4))
  p[is.na(P)] <- ""
  print(p, quote=FALSE)
  invisible()
}


spearman2 <- function(x, ...) UseMethod("spearman2") 


spearman2.default <- function(x, y, p=1, minlev=0,
                              exclude.imputed=TRUE, ...)
{
  if(p > 2)
    stop('p must be 1 or 2')
  
  if(exclude.imputed)
    im <- is.imputed(x) | is.imputed(y)
  
  y <- as.numeric(y)
  if(is.character(x))
    x <- factor(x)
  
  s <- !(is.na(x) | is.na(y))
  if(exclude.imputed)
    s <- s & !im
  
  n <- sum(s)
  ## 28Apr99:
  ## If number of non-NA values is less then 3 then return a NA
  ## value.
  if(n < 3)
    return(c(rho2=NA,F=NA,df1=0,df2=n,P=NA,n=n,'Adjusted rho2'=NA))

  ## Remove NAs from x and y
  x <- x[s]; y <- y[s]

  ## Find the number of unique values in x
  u <- length(unique(x))

  ## If is a factor and unique values are greater then 2 then find the
  ## lm.fit.qr.bare without an intercept.
  if(is.category(x) && u > 2) {
    if(minlev > 0) {
      x <- combine.levels(x, minlev)
      if(length(levels(x))<2) {
        warning(paste('x did not have >= 2 categories with >=',
                      mlev,'of the observations'))
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


spearman2.formula <- function(x, p=1, data, subset, na.action,
                              minlev=0, exclude.imputed=TRUE, ...)
{
  call <- match.call()
  nact <- NULL
  y <- match.call(expand=FALSE)
  y$formula <- x
  y$x <- y$p <- y$minlev <- y$exclude.imputed <- y$... <- NULL
  if(missing(na.action)) y$na.action <- na.retain
  y[[1]] <- as.name("model.frame")
  ##See if Des argument exists in current model.frame.default 3aug02
  ##if(length(model.frame.default$Des)) y$Des  <- FALSE   #turn off Design
  x <- eval(y, sys.parent())
  nam <- names(x)
  y <- x[[1]]
  w <- t(sapply(x[-1], spearman2, y=y, minlev=minlev, p=p,
                exclude.imputed=exclude.imputed))
  dimnames(w)[[2]] <- c("rho2","F","df1","df2","P","n","Adjusted rho2")
  structure(w, class='spearman2.formula', yname=names(x)[1])
}


print.spearman2.formula <- function(x, ...)
{
  cat('\nSpearman rho^2    Response variable:',attr(x,'yname'),'\n\n')
  dig <- c(3,2,0,0,4,0,3)
  for(i in 1:7)
    x[,i] <- round(x[,i],dig[i])
  
  attr(x,'yname') <- oldClass(x) <- NULL
  print(x)
  invisible()
}


plot.spearman2.formula <- function(x,
                                   what=c('Adjusted rho2','rho2','P'),
                                   sort.=TRUE, main, xlab, ...)
{
  what <- match.arg(what)
  if(missing(xlab)) xlab <- switch(what,
                                   'Adjusted rho2'=
                                     if(.R.)expression(Adjusted~rho^2)
                                     else 'Adjusted rho^2',
                                   'rho2'=if(.R.)expression(rho^2)
                                          else 'rho^2',
                                   'P'='P-value')
  if(missing(main))
    main <- if(.R.) parse(text=paste('paste(Spearman,~rho^2,~~~~Response:',
                                     attr(x,'yname'),')',sep=''))
            else
              paste('Spearman rho^2    Response variable:',attr(x,'yname'))
  
  if(.SV4.) x <- matrix(oldUnclass(x), nrow=nrow(x),
                        dimnames=dimnames(x)) ## 19Nov00
  ## SV4 doesn't consider a matrix with extra attributes as a matrix
  aux <- paste(x[,'n'],x[,'df1'])
  stat <- x[,what]
  if(sort.) {
    i <- order(stat)
    stat <- stat[i]
    aux <- aux[i]
  }
  dotchart2(stat, auxdata=aux, reset.par=TRUE,
            xlab=xlab, auxtitle=c('N  df'),
            main=main, ...)
  invisible()
}
