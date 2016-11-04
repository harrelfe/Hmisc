gbayes <- function(mean.prior, var.prior, m1, m2, stat, var.stat,
                   n1, n2, cut.prior, cut.prob.prior=.025) {
  if(!missing(cut.prior)) 
    var.prior <- ((cut.prior - mean.prior)/qnorm(1 - cut.prob.prior))^2

  if(!is.function(var.stat)) {
    vs <- var.stat
    if(!missing(n1))
      stop('may not specify n1,n2 when var.stat is not a function')
  } else
    vs <- var.stat(m1,m2)
  
  var.post  <- 1/(1/var.prior + 1/vs)
  mean.post <- (mean.prior/var.prior + stat/vs)*var.post
  result <- list(mean.prior=mean.prior, var.prior=var.prior, 
                 mean.post=mean.post,   var.post=var.post)

  if(!missing(n1)) {
    mean.pred <- mean.post
    var.pred <- var.post + var.stat(n1,n2)
    result$mean.pred <- mean.pred
    result$var.pred  <- var.pred
  }
  
  structure(result, class='gbayes')
}


plot.gbayes <- function(x, xlim, ylim, name.stat='z', ...) {
  obj <- x
  pred <- length(obj$mean.pred)>0
  if(missing(xlim))
    xlim <- obj$mean.post + c(-6,6)*sqrt(obj$var.post)

  x <- seq(xlim[1], xlim[2], length=200)
  y1 <- dnorm(x,obj$mean.prior,sqrt(obj$var.prior))
  y2 <- dnorm(x,obj$mean.post, sqrt(obj$var.post))
  plot(x, y1, xlab=name.stat, ylab='Density',type='l',lty=1,
       ylim=if(missing(ylim))
              range(c(y1,y2))
            else
              ylim)
  
  curves <- vector('list',2+pred)
  names(curves) <- c('Prior','Posterior',
                     if(pred)'
                       Predictive')
  
  curves[[1]] <- list(x=x,y=y1)
  lines(x, y2, lty=2)
  curves[[2]] <- list(x=x,y=y2)
  if(pred) {
    y <- dnorm(x,obj$mean.pred,sqrt(obj$var.pred))
    lines(x, y, lty=3)
    curves[[3]] <- list(x=x,y=y)
  }
  
  labcurve(curves, ...)
  invisible()
}


gbayes2 <- function(sd, prior, delta.w=0, alpha=0.05,
                    upper=Inf, prior.aux=NULL) {
  
  if(!is.function(prior))
    stop('prior must be a function')

  z <- qnorm(1-alpha/2)
  prod <- function(delta, prior, delta.w, sd, z, prior.aux) {
    (1 - pnorm((delta.w - delta)/sd + z)) *
      if(length(prior.aux))
        prior(delta, prior.aux)
      else
        prior(delta)
  }
  
  ww <- 'value'


  ip <- if(length(prior.aux))
    integrate(prior, -Inf, upper, prior.aux=prior.aux)[[ww]]
  else
    integrate(prior, -Inf, upper)[[ww]]
  
  if(abs(ip - 1) > .01)
    warning(paste('integrate failed to obtain 1.0 for integral of prior.\nDivided posterior probability by the integral it did obtain (',
                  format(ip),').\nTry specifying upper=.',sep=''))
  integrate(prod, delta.w, upper,
            prior=prior, delta.w=delta.w, sd=sd, z=z,
            prior.aux=prior.aux)[[ww]]
}


## v = variance of Xn after future obs.
gbayesMixPredNoData <- function(mix=NA, d0=NA, v0=NA, d1=NA, v1=NA,
                                what=c('density','cdf')) {
  what <- match.arg(what)
  g <- function(delta, v, mix, d0, v0, d1, v1, dist) {
    if(mix==1) {
      pv <- 1/(1 / v0 + 1 / v)
      dist(delta, d0, sqrt(pv))
    } else if(mix == 0) {
      pv <- 1/(1 / v1 + 1 / v)
      dist(delta, d1, sqrt(pv))
    } else {
      pv0 <- 1/(1 / v0 + 1 / v)
      pv1 <- 1/(1 / v1 + 1 / v)
      mix       * dist(delta, d0, sqrt(pv0)) +
        (1-mix) * dist(delta, d1, sqrt(pv1))
    }
  }

  formals(g) <- list(delta=numeric(0), v=NA, mix=mix, d0=d0, v0=v0,
                     d1=d1, v1=v1, dist=NA)
  g
}

gbayesMixPost <- function(x=NA, v=NA, mix=1, d0=NA, v0=NA, d1=NA,
                          v1=NA, what=c('density','cdf','postmean')) {
  what <- match.arg(what)
  g <- function(delta, x, v, mix=1, 
                d0, v0, d1, v1, dist) {
    if(mix == 1) {
      pv <- 1 / (1 / v0 + 1 / v)
      if(what == 'postmean') (d0 / v0 + x / v) * pv
      else
        dist(delta, (d0 / v0 + x / v) * pv, sqrt(pv))
    }
    else
      if(mix == 0) {
        pv <- 1 / (1 / v1 + 1 / v)
        if(what == 'postmean') (d1 / v1 + x / v) * pv
        else
          dist(delta, (d1 / v1 + x / v) * pv, sqrt(pv))
      } else {
        prior.odds <- mix / (1 - mix)
        pv0 <- 1 / (1 / v0 + 1 / v)
        pv1 <- 1 / (1 / v1 + 1 / v)
        # Until 2016-10-17 had omitted v+ in two sqrt below
        likelihood.ratio <- dnorm(x, d0, sqrt(v + v0)) /
                            dnorm(x, d1, sqrt(v + v1))
        post.odds <- prior.odds * likelihood.ratio
        mixp <- post.odds / (1 + post.odds)

        if(what == 'postmean')
           mixp  * (d0 / v0 + x / v) * pv0 +
        (1-mixp) * (d1 / v1 + x / v) * pv1
        else
             mixp * dist(delta, (d0 / v0 + x / v) * pv0, sqrt(pv0)) +
          (1-mixp)* dist(delta, (d1 / v1 + x / v) * pv1, sqrt(pv1))
    }
  }

  formals(g) <- list(delta=numeric(0), x=x, v=v, mix=mix,
                     d0=d0, v0=v0,
                     d1=d1, v1=v1,
                     dist=switch(what,
                                 density = dnorm,
                                 cdf     = pnorm,
                                 postmean= NULL))
  g
}


gbayesMixPowerNP <- function(pcdf, delta, v, delta.w=0, mix, interval,
                             nsim=0, alpha=0.05) {
  if(nsim==0) {
    ## Solve for statistic x such that the posterior cdf at
    ## (delta.w,x)=alpha/2
    g <- function(x, delta.w, v, alpha, pcdf, mix) {
      pcdf(delta.w, x, v, mix) - alpha/2
    }
    
    formals(g) <- list(x=numeric(0), delta.w=delta.w, v=v,
                       alpha=alpha, pcdf=pcdf,
                       mix=if(missing(mix)) as.list(pcdf)$mix else mix)

    x <- uniroot(g, interval=interval)$root
    c('Critical value'=x, Power=1 - pnorm(x, delta, sqrt(v)))
  } else {
    x <- rnorm(nsim, delta, sqrt(v))
    probs <-
      if(missing(mix))
        pcdf(delta.w, x, v)
      else
        pcdf(delta.w, x, v, mix=mix)
    
    pow <- mean(probs <= alpha/2)
    se <- sqrt(pow*(1-pow)/nsim)
    c(Power=pow, 'Lower 0.95'=pow-1.96*se, 'Upper 0.95'=pow+1.96*se)
  }
}


gbayes1PowerNP <- function(d0, v0, delta, v, delta.w=0, alpha=0.05) {
  pv <- 1/(1/v0 + 1/v)
  z <- qnorm(alpha/2)
  1 - pnorm(v*( (delta.w - sqrt(pv)*z)/pv - d0/v0 ), delta, sqrt(v))
}
