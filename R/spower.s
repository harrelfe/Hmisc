spower <- function(rcontrol, rinterv, rcens, nc, ni,
                   test=logrank, cox=FALSE, nsim=500, alpha=.05, pr=TRUE)
{
  crit <- qchisq(1-alpha, 1)
  group <- c(rep(1,nc), rep(2,ni))
  nexceed <- 0
  if(cox) beta <- numeric(nsim)

  maxfail <- 0; maxcens <- 0
  for(i in 1:nsim) {
    if(pr && i %% 10 == 0) cat(i,'\r')

    yc <- rcontrol(nc)
    yi <- rinterv(ni)
    cens <- rcens(nc+ni)
    y <- c(yc, yi)
    maxfail <- max(maxfail, max(y))
    maxcens <- max(maxcens, max(cens))
    S <- cbind(pmin(y,cens), 1*(y <= cens))
    nexceed <- nexceed + (test(S, group) > crit)
    if(cox)
      {
        fit <- survival::coxph.fit(as.matrix(group), S, strata=NULL,
                         offset=NULL, init=NULL,
                         control=survival::coxph.control(iter.max=10, eps=.0001), 
                         method="efron", rownames=NULL)
        beta[i] <- fit$coefficients
      }
  }
  cat('\n')
  if(maxfail < 0.99*maxcens)
      stop(paste('Censoring time distribution defined at later times than\nsurvival time distribution. There will likely be uncensored failure times\nstacked at the maximum allowed survival time.\nMaximum simulated failure time:', max(y),'\nMaximum simulated censoring time:', max(cens)))

  power <- nexceed/nsim
  if(cox) structure(list(power=power, betas=beta, nc=nc, ni=ni,
                         alpha=alpha, nsim=nsim), class='spower') else power
}

print.spower <- function(x, conf.int=.95, ...)
  {
    b <- x$betas
    hr <- exp(b)
    pp <- (1+conf.int)/2
    cl <- quantile(hr, c((1-conf.int)/2, pp))
    meanbeta <- mean(b)
    medbeta <- median(b)
    hrmean <- exp(meanbeta)
    hrmed  <- exp(medbeta)
    moehi <- cl[2]/hrmed
    moelo <- hrmed/cl[1]
    g <- function(w) round(w, 4)
    mmoe <- max(moehi, moelo)
    cat('\nTwo-Group Event Time Comparison Simulation\n\n',
        x$nsim,' simulations\talpha: ', x$alpha, '\tpower: ', x$power,
        '\t', conf.int, ' confidence interval\n',
        '\nHazard ratio from mean   beta     : ', g(hrmean),
        '\nHazard ratio from median beta     : ', g(hrmed),
        '\nStandard error of log hazard ratio: ', g(sd(b)),
        '\nConfidence limits for hazard ratio: ', g(cl[1]), ', ', g(cl[2]),
        '\nFold-change margin of error high  : ', g(moehi),
        '\t(upper CL/median HR)',
        '\nFold-change margin of error low   : ', g(moelo),
        '\t(median HR/lower CL)',
        '\nMax fold-change margin of error   : ', g(mmoe),'\n\n')

    cat('The fold change margin of error of', g(mmoe),
        'represents the margin of error\n',
        'the study is likely to achieve in estimating the intervention:control\n',
        'hazard ratio. It is the ratio of a', conf.int, 'confidence limit on the\n',
        'hazard ratio to the median hazard ratio obtained over the', x$nsim, 'simulations.\n',
        'The confidence limit was obtained by computing the', pp, 'quantile of the\n',
        x$nsim, 'observed hazard ratios.  The standard error is the standard deviation\n',
        'of the', x$nsim, 'simulated log hazard ratios.\n\n')

    res <- c(cl, hrmean, hrmed, sd(b), moelo, moehi, x$power)
    names(res) <- c('CLlower','CLupper','HRmean','HRmedian','SE',
                    'MOElower','MOEupper','Power')
    invisible(res)
  }
      
Quantile2 <- function(scontrol, hratio, 
                      dropin=function(times)0, 
                      dropout=function(times)0,
                      m=7500, tmax, qtmax=.001, mplot=200, pr=TRUE,
                      ...)
{
  ## Solve for tmax such that scontrol(t)=qtmax
  dlist <- list(...)
  k <- length(dlist) && !is.null(dlist)
  f    <- if(k) function(x, scontrol, qt, ...) scontrol(x, ...) - qt
          else function(x, scontrol, qt) scontrol(x) - qt

  if(missing(tmax)) {
    if(k) tmax <- uniroot(f, c(0,1e9), scontrol=scontrol, qt=qtmax, ...)$root
    else tmax <- uniroot(f, c(0,1e9), scontrol=scontrol, qt=qtmax)$root
  }

  if(pr)
    cat('\nInterval of time for evaluating functions:[0,',
        format(tmax),']\n\n')

  ## Generate sequence of times to use in all approximations and sequence
  ## to use for plot method

  times <- seq(0, tmax, length.out=m)
  tim   <- seq(0, tmax, length.out=mplot)
  tinc  <- times[2]

  ## Approximate hazard function for control group
  sc <- scontrol(times, ...)    
  hc <- diff(-logb(sc))
  hc <- c(hc, hc[m-1])/tinc  ## to make length=m

  ## hazard function for intervention group
  hr <- rep(hratio(times), length.out=m)
  hi <- hc*hr

  ## hazard for control group with dropin
  di  <- rep(dropin(times), length.out=m)
  hc2 <- (1-di)*hc + di*hi

  ## hazard for intervention group with dropout
  do  <- rep(dropout(times), length.out=m)
  hi2 <- (1-do)*hi + do*hc

  ## survival for intervention group
  si  <- exp(-tinc*cumsum(hi))

  ## Compute contaminated survival function for control and intervention
  sc2 <- if(any(di>0))exp(-tinc*cumsum(hc2))
         else sc

  si2 <- exp(-tinc*cumsum(hi2))
  

  ## Store all functions evaluated at shorter times vector (tim), for
  ## plotting
  asing <- function(x) x

  sc.p  <- asing(approx(times, sc,  xout=tim)$y)
  hc.p  <- asing(approx(times, hc,  xout=tim)$y)
  sc2.p <- asing(approx(times, sc2, xout=tim)$y)
  hc2.p <- asing(approx(times, hc2, xout=tim)$y)
  
  si.p  <- asing(approx(times, si,  xout=tim)$y)
  hi.p  <- asing(approx(times, hi,  xout=tim)$y)
  si2.p <- asing(approx(times, si2, xout=tim)$y)
  hi2.p <- asing(approx(times, hi2, xout=tim)$y)

  dropin.p  <- asing(approx(times, di, xout=tim)$y)
  dropout.p <- asing(approx(times, do, xout=tim)$y)
  hratio.p  <- asing(approx(times, hr, xout=tim)$y)
  hratio2.p <- hi2.p/hc2.p

  tim       <- asing(tim)

  plot.info <- list("C Survival"                   =list(Time=tim,Survival=sc.p),
                    "I Survival"                   =list(Time=tim,Survival=si.p),
                    "C Survival w/Dropin"          =list(Time=tim,Survival=sc2.p),
                    "I Survival w/Dropout"         =list(Time=tim,Survival=si2.p),
                    "C Hazard"                     =list(Time=tim,Hazard=hc.p),
                    "I Hazard"                     =list(Time=tim,Hazard=hi.p),
                    "C Hazard w/Dropin"            =list(Time=tim,Hazard=hc2.p),
                    "I Hazard w/Dropout"           =list(Time=tim,Hazard=hi2.p),
                    "Dropin"                       =list(Time=tim,Probability=dropin.p),
                    "Dropout"                      =list(Time=tim,Probability=dropout.p),
                    "Hazard Ratio"                 =list(Time=tim,Ratio=hratio.p),
                    "Hazard Ratio w/Dropin+Dropout"=list(Time=tim,Ratio=hratio2.p))

  ## Create S-Plus functions for computing random failure times for
  ## control and intervention subject to dropin, dropout, and hratio

  r <- function(n, what=c('control','intervention'), 
                times, csurvival, isurvival)
  {
    what <- match.arg(what)
    approx(if(what=='control')csurvival
           else isurvival, 
           times, xout=runif(n), rule=2)$y
  }

  asing <- function(x) x

  formals(r) <- list(n=integer(0),
                     what=c('control','intervention'),
                     times=asing(times), csurvival=asing(sc2),
                     isurvival=asing(si2))

  structure(r, plot.info=plot.info, 
            dropin=any(di>0), dropout=any(do>0),
            class='Quantile2')
}


print.Quantile2 <- function(x, ...)
{
  attributes(x) <- NULL
  print(x)
  invisible()
}

plot.Quantile2 <- function(x, 
                           what=c('survival','hazard','both','drop','hratio',
                                  'all'), dropsep=FALSE,
                           lty=1:4, col=1, xlim, ylim=NULL, 
                           label.curves=NULL, ...)
{
  what <- match.arg(what)
  pi <- attr(x, 'plot.info')
  if(missing(xlim))
    xlim <- c(0,max(pi[[1]][[1]]))

  dropin  <- attr(x, 'dropin')
  dropout <- attr(x, 'dropout')
  i <- c(1,2,
         if(dropin)3,
         if(dropout)4)

  if(what %in% c('survival','both','all')) {
    if(dropsep && (dropin|dropout)) {
      labcurve(pi[1:2], pl=TRUE, lty=lty, col.=col, xlim=xlim, ylim=ylim,
               opts=label.curves)
      labcurve(pi[i[-(1:2)]], pl=TRUE, lty=lty, col.=col, xlim=xlim, ylim=ylim,
               opts=label.curves)
    } else
      labcurve(pi[i], pl=TRUE, lty=lty, col.=col, xlim=xlim, ylim=ylim,
               opts=label.curves)
  }

  if(what %in% c('hazard','both','all')) {
    if(dropsep && (dropin|dropout)) {
      labcurve(pi[5:6], pl=TRUE, lty=lty, col.=col, xlim=xlim, ylim=ylim,
               opts=label.curves)
      labcurve(pi[4+i[-(1:2)]], pl=TRUE, lty=lty, col.=col, xlim=xlim,
               ylim=ylim, opts=label.curves)
    } else
      labcurve(pi[4+i], pl=TRUE, lty=lty, col.=col, xlim=xlim, ylim=ylim,
               opts=label.curves)
  }
  
  if(what=='drop' || (what=='all' && (dropin | dropout))) {
    i <- c(if(dropin)9,
           if(dropout)10)

    if(length(i)==0)
      i <- 10

    labcurve(pi[i], pl=TRUE, lty=lty, col.=col, xlim=xlim, ylim=ylim,
             opts=label.curves)
  }

  if(what %in% c('hratio','all')) {
    i <- c(11,
           if(dropin|dropout) 12)

    labcurve(pi[i], pl=TRUE, lty=lty, col.=col, xlim=xlim, ylim=ylim,
             opts=label.curves)
  }

  invisible()
}

logrank <- function(S, group)
{
  i <- is.na(S) | is.na(group)
  if(any(i))
    {
      i     <- ! i
      S     <- S[i,, drop=FALSE]
      group <- group[i]
    }
  u <- sort(unique(group))
  if(length(u) > 2) stop('group must have only 2 distinct values')
  x <- ifelse(group == u[2], 1, 0)
  y     <- S[, 1]
  event <- S[, 2]
  # Sort all data in descending failure time order
  i     <- order(- y)
  y     <- y[i]
  event <- event[i]
  x     <- x[i]

  x     <- cbind(1 - x, x, (1 - x) * event, x * event)
  s     <- rowsum(x, y, FALSE)

  nr1 <- cumsum(s[, 1])
  nr2 <- cumsum(s[, 2])
  d1  <- s[,3]
  d2  <- s[,4]
  rd  <- d1 + d2
  rs  <- nr1 + nr2 - rd
  n   <- nr1 + nr2
  oecum <- d1 - rd * nr1/n
  vcum  <- rd * rs * nr1 * nr2 / n / n / (n - 1)
  chisq <- sum(oecum) ^ 2 / sum(vcum, na.rm=TRUE)
  o1 <- sum(d1)
  o2 <- sum(d2)
  e1 <- sum(nr1 * rd / n)
  e2 <- sum(nr2 * rd / n)
  hr <- (o2 / e2) / (o1 / e1)
  structure(chisq, hr=hr)
}


Weibull2 <- function(times, surv)
{
  z1 <- -logb(surv[1])
  z2 <- -logb(surv[2])
  t1 <- times[1]
  t2 <- times[2]
  gamma <- logb(z2/z1)/logb(t2/t1)
  alpha <- z1/(t1^gamma)
  
  g <- function(times, alpha, gamma)
  {
    exp(-alpha*(times^gamma))
  }

  formals(g) <- list(times=NULL, alpha=alpha, gamma=gamma)
  g
}

# Non-working code where logrank was tried to extend to stratification
 if(FALSE) {
      OE <- v <- hrn <- hrd <- 0
      for(strat in unique(strata))
        {
          j <- strata==strat
          s <- rowsum(x[j,], y[j], FALSE)
          nr1 <- cumsum(s[,1])
          nr2 <- cumsum(s[,2])
          d1  <- s[,3]
          d2  <- s[,4]
          rd  <- d1+d2
          rs  <- nr1+nr2-rd
          n   <- nr1+nr2
          oecum <- d1 - rd*nr1/n
          vcum  <- rd * rs * nr1 * nr2 / n / n / (n-1)
          OE <- OE + sum(oecum)
          v  <- v + sum(vcum, na.rm=TRUE)
          hrn <- hrn + sum(d1*(nr1-d1)/n)
          hrd <- hrd + sum(d2*(nr2-d2)/n)
        }
      chisq <- OE^2 / v
      hr <- hrn/hrd
    }
 

## Function to fit a Gompertz survival distribution to two points
## The function is S(t) = exp[-(1/b)exp(a+bt)]
## Returns a list with components a and b, and a function for
## generating S(t) for a vector of times
Gompertz2 <- function(times, surv)
{
  z1 <- logb(-logb(surv[1]))
  z2 <- logb(-logb(surv[2]))
  t1 <- times[1]
  t2 <- times[2]
  b  <- (z2-z1)/(t2-t1)
  a  <- z1 + logb(b)-b*t1
  
  g <- function(times, a, b) {
    exp(-exp(a+b*times)/b)
  }

  formals(g) <- list(times=NULL, a=a, b=b)
  g
}


Lognorm2 <- function(times, surv)
{
  z1 <- qnorm(1-surv[1])
  z2 <- qnorm(1-surv[2])
  sigma <- logb(times[2]/times[1])/(z2-z1)
  mu    <- logb(times[1]) - sigma*z1

  g <- function(times, mu, sigma) {
    pnorm(- (logb(times) - mu) / sigma)
  }

  formals(g) <- list(times=NULL, mu=mu, sigma=sigma)
  g
}
