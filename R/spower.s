spower <- function(rcontrol, rinterv, rcens, nc, ni,
					test=logrank, nsim=500, alpha=.05, pr=TRUE) {

crit <- qchisq(1-alpha, 1)
group <- c(rep(1,nc), rep(2,ni))
nexceed <- 0

for(i in 1:nsim) {
  if(pr && i %% 10 == 0) cat(i,'')
  yc <- rcontrol(nc)
  yi <- rinterv(ni)
  cens <- rcens(nc+ni)
  y <- c(yc, yi)
  S <- cbind(pmin(y,cens), 1*(y <= cens))
  nexceed <- nexceed + (test(S, group) > crit)
}
nexceed/nsim
}
  

Quantile2 <- function(scontrol, hratio, 
					  dropin=function(times)0, 
					  dropout=function(times)0,
					  m=7500, tmax, qtmax=.001, mplot=200, pr=TRUE, ...) {

## Solve for tmax such that scontrol(t)=qtmax
dlist <- list(...)
k <- length(dlist) && !is.null(dlist)
f    <- if(k) function(x, scontrol, qt, ...) scontrol(x, ...) - qt
else function(x, scontrol, qt) scontrol(x) - qt

if(missing(tmax)) {
  if(k) tmax <- uniroot(f, c(0,1e9), scontrol=scontrol, qt=qtmax, ...)$root
	else tmax <- uniroot(f, c(0,1e9), scontrol=scontrol, qt=qtmax)$root
}
if(pr) cat('\nInterval of time for evaluating functions:[0,',
		   format(tmax),']\n\n')

## Generate sequence of times to use in all approximations and sequence
## to use for plot method

times <- seq(0, tmax, length=m)
tim   <- seq(0, tmax, length=mplot)
tinc  <- times[2]

## Approximate hazard function for control group
sc <- scontrol(times, ...)    
hc <- diff(-logb(sc))
hc <- c(hc, hc[m-1])/tinc  ## to make length=m

## hazard function for intervention group
hr <- rep(hratio(times), length=m)
hi <- hc*hr

## hazard for control group with dropin
di  <- rep(dropin(times),length=m)
hc2 <- (1-di)*hc + di*hi

## hazard for intervention group with dropout
do  <- rep(dropout(times),length=m)
hi2 <- (1-do)*hi + do*hc

## survival for intervention group
si  <- exp(-tinc*cumsum(hi))

## Compute contaminated survival function for control and intervention
sc2 <- if(any(di>0))exp(-tinc*cumsum(hc2)) else sc
si2 <- exp(-tinc*cumsum(hi2))


## Store all functions evaluated at shorter times vector (tim), for
## plotting
asing <- if(.R.)function(x)x else as.single
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

plot.info <- list(
  "C Survival"                   =list(Time=tim,Survival=sc.p),
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
			  times, csurvival, isurvival) {
  what <- match.arg(what)
  approx(if(what=='control')csurvival else isurvival, 
		 times, xout=runif(n), rule=2)$y
}
asing <- if(.R.)function(x)x else as.single
formals(r) <- list(n=integer(0),
                   what=c('control','intervention'),
                   times=asing(times), csurvival=asing(sc2),
                   isurvival=asing(si2))

structure(r, plot.info=plot.info, 
		  dropin=any(di>0), dropout=any(do>0),
		  class='Quantile2')
}

print.Quantile2 <- function(x, ...) {
  attributes(x) <- NULL
  print(x)
  invisible()
}

plot.Quantile2 <- function(x, 
						   what=c('survival','hazard','both','drop','hratio',
							 'all'), dropsep=FALSE,
						   lty=1:4, col=1, xlim, ylim=NULL, 
						   label.curves=NULL, ...) {
  what <- match.arg(what)
  pi <- attr(x, 'plot.info')
  if(missing(xlim)) xlim <- c(0,max(pi[[1]][[1]]))
  dropin  <- attr(x, 'dropin')
  dropout <- attr(x, 'dropout')
  i <- c(1,2,if(dropin)3,if(dropout)4)
  if(what %in% c('survival','both','all')) {
	if(dropsep && (dropin|dropout)) {
	  labcurve(pi[1:2], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			   opts=label.curves)
	  labcurve(pi[i[-(1:2)]], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			   opts=label.curves)
	} else
	labcurve(pi[i], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			 opts=label.curves)
  }

  if(what %in% c('hazard','both','all')) {
	if(dropsep && (dropin|dropout)) {
	  labcurve(pi[5:6], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			   opts=label.curves)
	  labcurve(pi[4+i[-(1:2)]], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			   opts=label.curves)
	} else
	labcurve(pi[4+i], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			 opts=label.curves)
  }

  if(what=='drop' || (what=='all' && (dropin | dropout))) {
	i <- c(if(dropin)9, if(dropout)10)
	if(length(i)==0) i <- 10
    labcurve(pi[i], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			 opts=label.curves)
  }

  if(what %in% c('hratio','all')) {
    i <- c(11, if(dropin|dropout) 12)
    labcurve(pi[i], pl=TRUE, lty=lty, col=col, xlim=xlim, ylim=ylim,
			 opts=label.curves)
  }

  invisible()
}

logrank <- function(S, group) {
  y     <- S[,1]
  event <- S[,2]
  i     <- order(-y)
  y     <- y[i]
  event <- event[i]
  group <- group[i]
  x     <- cbind(group==1, group==2, (group==1)*event, (group==2)*event)
  s     <- rowsumFast(x, y, FALSE)
  nr1 <- cumsum(s[,1])
  nr2 <- cumsum(s[,2])
  d1  <- s[,3]
  d2  <- s[,4]
  rd  <- d1+d2
  rs  <- nr1+nr2-rd
  n   <- nr1+nr2
  oecum <- d1 - rd*nr1/n
  vcum  <- rd * rs * nr1 * nr2 / n / n / (n-1)
  sum(oecum)^2 / sum(vcum,na.rm=TRUE)
}

Weibull2 <- function(times, surv) {

z1 <- -logb(surv[1])
z2 <- -logb(surv[2])
t1 <- times[1]
t2 <- times[2]
gamma <- logb(z2/z1)/logb(t2/t1)
alpha <- z1/(t1^gamma)

g <- function(times, alpha, gamma) {exp(-alpha*(times^gamma))}
formals(g) <- list(times=NULL, alpha=alpha, gamma=gamma)
g
}

#Function to fit a Gompertz survival distribution to two points
#The function is S(t) = exp[-(1/b)exp(a+bt)]
#Returns a list with components a and b, and a function for
#generating S(t) for a vector of times

Gompertz2 <- function(times, surv) {

z1 <- logb(-logb(surv[1]))
z2 <- logb(-logb(surv[2]))
t1 <- times[1]
t2 <- times[2]
b  <- (z2-z1)/(t2-t1)
a  <- z1 + logb(b)-b*t1

g <- function(times, a, b) {exp(-exp(a+b*times)/b)}
formals(g) <- list(times=NULL, a=a, b=b)
g
}

Lognorm2 <- function(times, surv) {

z1 <- qnorm(1-surv[1])
z2 <- qnorm(1-surv[2])
sigma <- logb(times[2]/times[1])/(z2-z1)
mu    <- logb(times[1]) - sigma*z1

g <- function(times, mu, sigma) {1 - pnorm((logb(times)-mu)/sigma)}
formals(g) <- list(times=NULL, mu=mu, sigma=sigma)
g
}
