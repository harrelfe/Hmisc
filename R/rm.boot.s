rm.boot <- function(time, y, id=seq(along=time), subset=TRUE,
                    plot.individual=FALSE,
                    bootstrap.type=c('x fixed','x random'),
                    nk=6, knots, B=500, smoother=supsmu, 
                    xlab, xlim, ylim=range(y), 
                    times=seq(min(time),max(time),length=100),
                    absorb.subject.effects=FALSE, rho=0,
                    cor.pattern=c('independent','estimate'), ncor=10000,
                    ...)
{
  bootstrap.type <- match.arg(bootstrap.type)
  absorb.subject.effects <- absorb.subject.effects & !missing(id)
  if(!is.function(cor.pattern))
    cor.pattern <- match.arg(cor.pattern)
  
  if(!(is.character(cor.pattern) && cor.pattern=='independent') && 
     rho!=0)
    stop("can't specify both cor.pattern='estimate' and rho")
  
  if(rho != 0)
    cor.pattern <- 'equal correlation'
  
  dodep <- rho !=0 || !is.character(cor.pattern) || cor.pattern=='estimate'

  ## X fixed also implies that subjects are fixed

  id <- as.character(id)
  ylab <- label(y)
  if(ylab=='')
    ylab <- 'y'
  
  if(missing(xlab)) {
    xlab <- units(time)
    if(xlab=='')
      xlab <- 'Time'
  }

  if(length(subset) > 1) {
    id <- id[subset];
    time <- time[subset]; y <- y[subset]
  }

  s <- is.na(time + y)
  if(any(s)) {
    s <- !s
    id <- id[s]
    time <- time[s]
    y <- y[s]
  }
  ## Need to order data so that a subject's records stay together
  ## Otherwise, the mean residuals at each time will not vary over resamples
  ## when bootstrap.type='x fixed'

  s <- order(id, time)
  id <- id[s];
  time <- time[s];
  y <- y[s]

  if(bootstrap.type=='x fixed' && diff(range(table(id))) != 0) 
    warning('To work properly with bootstrap.type="x fixed" all subjects must have the same # observations')

  n <- length(y)

  clusters <- unique(id)

  if(plot.individual) {
    ploti <- function(time, y, id, clusters, xlim, ylim, xlab, ylab, 
                      smoother, ...)
    {
      plot(0,0,xlim=range(pretty(range(time))),ylim=ylim,
           xlab=xlab, ylab=ylab, type='n')
      j <- 0
      for(i in clusters) {
        s <- id==i
        j <- j+1
        lines(smoother(time[s],y[s],...),lty=j)
      }
    }
    
    ploti(time, y, id, clusters, xlim, ylim, xlab, ylab, smoother, ...)
  }

  if(nk==0) knots <- double(0)
  if(missing(knots) && nk>0) {
    knots <- rcspline.eval(time,nk=nk,knots.only=TRUE)
    if(length(knots) != nk) {
      warning('could not obtain requested number of knots')
      nk <- length(knots) 
    }
  } else nk <- length(knots)
  
  p <- if(nk==0) 1
       else nk-1

  X.times <- if(nk==0) as.matrix(times)
             else rcspline.eval(times, knots, inclx=TRUE)

  X.Time <- if(nk==0) as.matrix(time)
            else rcspline.eval(time, knots, inclx=TRUE)
  
  X <- if(missing(id)) cbind(X.Time,1)
       else 
         model.matrix(~ X.Time+id-1,
                      data=list(X.Time=X.Time,id=as.factor(id)))
  
  ## was id=id 3Apr02   Thanks: Don MacQueen, for R

  f <- lm.fit.qr.bare(X, y, intercept=FALSE)
  res <- f$residuals
  sigma2 <- sum(res^2)/n

  if(absorb.subject.effects) {
    mean.intercept <- mean(c(0,f$coef[-(1:p)]))
    y <- y + mean.intercept - (f$coef[-(1:p)])[paste('id',id,sep='')]
    if(plot.individual) {
      ploti(time, y, id, clusters, xlim, ylim, xlab, ylab, smoother, ...)
      title('Raw Data Adjusted to Have a Common Intercept')
    }
  }

  if(is.character(cor.pattern) && cor.pattern=='estimate') {
    timediff <- product <- single(ncor)
    used <- 0
    i <- 0
    meanres <- tapply(res, time, mean)
    names(meanres) <- as.numeric(names(meanres))
    sdres   <- sqrt(tapply(res, time, var))
    names(sdres) <- as.numeric(names(sdres))
    if(any(is.na(sdres)))
      stop('one or more times occur in only one subject')

    for(wid in clusters) {
      s <- id==wid
      x <- time[s]
      cx <- as.character(x)
      r <- (res[s] - meanres[cx])/sdres[cx]
      if(any(is.na(r)))
        stop('program logic error')
      
      diffs <- outer(x, x, FUN=function(a,b)abs(a-b))
      prods <- outer(r, r, FUN='*')
      np <- length(prods)
      if(used + np > ncor) {
        cat('\nUsed only',i,'subjects in estimating covariance pattern.\nMay want to increase ncor.\n')
        break
      }
      
      i <- i+1
      timediff[(used+1):(used+np)] <- diffs
      product[(used+1):(used+np)]  <- prods
      used <- used+np
    }
    
    timediff <- timediff[1:used]; product <- product[1:used]
    product <- tapply(product, round(timediff,4), mean)
    timediff <- as.numeric(names(product))
    product[timediff==0] <- 1
    plot(timediff, product, xlab='Absolute Difference in Time',
	 ylab='Correlation', type='b')

    cor.pattern <- list(x=timediff, y=product)
  }

  ##Subject effects are at the end, using cell means model
  ##Take intercept as average of all subject effects
  cof <- function(fit,p)
  {
    ko <- fit$coef
    c(mean(ko[-(1:p)]), ko[1:p])
  }

  o.coef   <- cof(f,p)

  if(bootstrap.type=='x random') {
    orig.obsno <- split(1:n, id)
  } else {
    R    <- split(res, id)
    yhat <- if(!absorb.subject.effects) f$fitted.values
            else o.coef[1] + X.Time %*% o.coef[-1]
  }

  Coef <- matrix(NA, B+1, p+1)
  sse  <- loglik <- single(B+1)
  loglik.dep <- NULL

  Coef[1,]  <- o.coef
  sse[1]    <- sigma2*n
  loglik[1] <- n*logb(2*pi*sigma2) + n

  if(dodep) {
    loglik.dep <- loglik
    lldep <- function(time, id, sigma2, res, rho, cor.pattern)
    {
      ll <- 0
      for(subj in unique(id)) {
        s  <- id==subj
        x  <- time[s]
        y  <- res[s]
        p  <- sum(s)
        if(is.character(cor.pattern) && cor.pattern=='equal correlation')
          cov <- sigma2*(diag(rep(1-rho,p))+rho)
        else {
          cov <- if(is.function(cor.pattern)) 
                   outer(x, x, cor.pattern)*sigma2
                 else {
                   timediff <- outer(x, x, function(a,b)abs(a-b))
                   matrix(approx(cor.pattern, xout=timediff)$y, nrow=p)*sigma2
                 }
        }
        
        ## Following code taken from dmvnorm()
        eS <- eigen(cov, symmetric = TRUE)
	##  y <- y %*% (eS$vectors * rep(1/sqrt(eS$values), each = p)) 24Feb02
        y <- y %*% (eS$vectors * rep(1/sqrt(eS$values),
                                     rep(p,length(eS$values))))
        logl <- sum(y^2) + p*logb(2*pi) + logb(prod(eS$values))
        ll <- ll + logl
      }
      
      ll
    }
    
    loglik.dep[1] <- lldep(time, id, sigma2, res, rho, cor.pattern)
  }

  uneven    <- 0

  for(i in 1:B) {
    if(i %% 10 ==0)
      cat(i,'')
    
    pts <- sample(clusters, replace=TRUE)

    if(bootstrap.type=='x random') {
      obsn <- unlist(orig.obsno[pts])
      idb <- id[obsn]

      xt <- X.Time[obsn,,drop=FALSE]
      f.b <- lm.fit.qr.bare(if(absorb.subject.effects || missing(id)) 
                              cbind(xt,1)
                            else 
                              model.matrix(~xt+idb-1,
                                           data=list(xt=xt,idb=as.factor(idb))),
                            y[obsn], intercept=FALSE)
      
      ## was idb=idb 3Apr02
    } else {
      rr <- unlist(R[pts])
      lrr <- length(rr)
      uneven <- max(uneven, abs(lrr-n))
      if(lrr > n)
        rr <- rr[1:n]
      else if(lrr < n)
        rr <- c(rr, sample(rr, n-lrr, replace=TRUE))
      
      yb.e <- yhat + rr
      f.b <- if(absorb.subject.effects) 
               lm.fit.qr.bare(cbind(X.Time,1), yb.e,
                              intercept=FALSE)
             else
               lm.fit.qr.bare(X, yb.e, intercept=FALSE)
    }

    cofb <- cof(f.b, p)   #26Jun97

    pred <-
      if(bootstrap.type=='x fixed') {
        if(!absorb.subject.effects)
          X %*% f.b$coefficients
        else
          cofb[1] + X.Time %*% cofb[-1]
        
      } else cofb[1] + X.Time %*% cofb[-1]
    
    ## x random case may only work properly if absorb.subject.effects, as
    ## we have to ignore the original subject ids anyway (the bootstrap
    ## sample in general won't represent all subjects)
    Coef[i+1,]  <- cofb    #26Jun97
    sse[i+1]    <- sum((y-pred)^2)
    sigma2      <- sum(f.b$residuals^2)/length(f.b$residuals)
    loglik[i+1] <-  n*logb(2*pi*sigma2) + sse[i+1]/sigma2
    if(dodep)
      loglik.dep[i+1] <- lldep(time, id, sigma2, y-pred,
                               rho, cor.pattern)
  }
  
  if(uneven>0)
    warning(paste('Subjects had unequal number of records.\nMaximum discrepency between ',
                  'total number of bootstrap records sampled and original\nnumber of ',
                  'records (',n,') is ',uneven,'. Bootstrap estimates are approximate.',
                  sep=''))

  if(dodep) {
    srho <- spearman(loglik, loglik.dep)
    cat('\n\nSpearman rank correlation between',B+1,'log likelihoods ',
        'assuming independence and assuming dependence:',
        round(srho,3),'\n')
  }

  mode(Coef) <- 'single'
  mode(sse)  <- 'single'
  structure(list(Coef=Coef, sse=sse, loglik=loglik, loglik.dep=loglik.dep,
                 times=times, X.times=X.times,
                 xlab=xlab, ylab=ylab, ylim=ylim, 
                 bootstrap.type=bootstrap.type, fit=f, knots=knots, 
                 rho=rho, cor.pattern=cor.pattern), 
            class='rm.boot')
}


plot.rm.boot <-
  function(x, obj2, conf.int=.95,
           xlab=x$xlab, ylab=x$ylab, xlim, ylim=x$ylim,
           individual.boot=FALSE,
           pointwise.band=FALSE,
           curves.in.simultaneous.band=FALSE,
           col.pointwise.band=2,
           objective=c('-2 log L','sse','dep -2 log L'), 
           add=FALSE, ncurves,
           multi=FALSE, multi.method=c('color','density'),
           multi.conf=c(.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95,.99),
           multi.density=c(-1,90,80,70,60,50,40,30,20,10, 7,4),
           multi.col =c( 1, 8,20, 5, 2, 7,15,13,10,11,9,14),
           subtitles=TRUE, ...)
{
  ##	2 was between 5 and 7, 17 was between 8 and 20

  obj <- x
  objective <- match.arg(objective)
  if(missing(objective))
    objective <- 
      if(obj$rho==0 && is.character(obj$cor.pattern))
        '-2 log L'
      else 'dep -2 log L'

  sse <- switch(objective, 
                sse            = obj$sse,
                '-2 log L'     = obj$loglik,
                'dep -2 log L' = obj$loglik.dep)

  B     <- length(sse)
  Coef  <- obj$Coef
  times <- obj$times

  if(!missing(obj2)) {
    if((length(times) != length(obj2$times)) || 
       (any(times != obj2$times, na.rm=TRUE)))
      stop('times vector must be identical for both rm.boot objects')
    
    times <- ifelse(is.na(times), NA, obj2$times)
    sse <- sse + obj2$sse
    if(missing(ylab))
      ylab <- paste(obj$ylab,'-',obj2$ylab) 
  }

  ## order from best -2 log likelihood or sum of squared errors to worst
  i <- order(sse)
  ## Select best confidence coefficient*B estimates
  conf <- if(multi) max(multi.conf)
          else conf.int
  
  i <- i[1:round(conf*B)]
  if(i[1] != 1)
    warning(paste('design is imbalanced enough that best log likelihood or SSE was not\n',
                 'obtained from overall fit (objective=',format(sse[1]),') but from\n',
                 'a bootstrap fit (objective=',format(sse[i[1]]),
                 ')\nThis can also happen if the objective is not -2 log L',sep=''))

  ## Evaluate all fits on time grid and compute point by point max and min

  curves <- cbind(1,obj$X.times) %*% t(Coef)
  if(!missing(obj2)) {
    curves <- curves - cbind(1,obj2$X.times) %*% t(obj2$Coef)
    if(missing(ylim))
      ylim <- range(curves[,i])
  }			
  
  if(multi) {
    multi.method <- match.arg(multi.method)
    if(missing(xlim))
      plot(times, curves[,1], type='n',
           xlab=xlab, ylab=ylab, ylim=ylim)
    else
      plot(times, curves[,1], type='n',
	   xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim)
    
    title(paste('Simultaneous',min(multi.conf),'-',max(multi.conf),
		'Confidence Regions'))
    high.prev <- low.prev <- curves[,1]
    for(j in 1:length(multi.conf)) {
      ii <- i[1:round(multi.conf[j]*B)]
      high <- apply(curves[,ii], 1, max)
      low  <- apply(curves[,ii], 1, min)
      if(multi.method=='density') {
	polygon(c(times,rev(times)), c(high.prev,rev(high)), 
                density=multi.density[j])
	polygon(c(times,rev(times)), c(low.prev, rev(low)),  
                density=multi.density[j])
      } else {
	polygon(c(times,rev(times)), c(high.prev,rev(high)), 
                col=multi.col[j])
	polygon(c(times,rev(times)), c(low.prev, rev(low)),  
                col=multi.col[j])
      }
      
      high.prev <- high; low.prev <- low
    }
    
    lines(times, curves[,1], lwd=2, col=0)  ## point estimates in white
  } else {
    if(add)
      lines(times, curves[,1])
    else {
      if(missing(xlim))
        plot(times, curves[,1], type='l',
             xlab=xlab, ylab=ylab, ylim=ylim)
      else
	plot(times, curves[,1], type='l',
             xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim)
      
      title(paste('Simultaneous',conf.int,'Confidence Region'))
    }

    high <- apply(curves[,i], 1, max)
    low  <- apply(curves[,i], 1, min)
    lines(times, high, lty=2)
    lines(times, low,  lty=2)
  }
  
  result <- list(times=times, fitted=curves[,1], lower=low, upper=high)

  if(individual.boot || curves.in.simultaneous.band) {
    subs <- if(individual.boot) 1:B
            else i
    
    if(!missing(ncurves))
      subs <- sample(subs, ncurves)
    
    for(j in subs)
      lines(times, curves[,j], lty=2)
  }

  if(pointwise.band) {
    p <- apply(curves, 1, quantile, probs=c((1-conf.int)/2,1-(1-conf.int)/2))
    lines(times,p[1,],col=col.pointwise.band)
    lines(times,p[2,],col=col.pointwise.band)
    result <- c(result, list(pointwise.lower=p[1,], pointwise.upper=p[2,]))
  }
  
  if(!add && subtitles) {
    title(sub=obj$bootstrap.type,adj=1)
    title(sub=paste(B-1,'bootstrap repetitions'),adj=0)
  }

  invisible(result)
}
