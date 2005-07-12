## tref        time at which mortalities estimated
## n           total sample size
## mc          tref-year mortality, control
## r           % reduction in m1c by intervention
## accrual     duration of accrual period
## tmin        minimum follow-up time
## noncomp.c   % non-compliant in control group (drop-ins)
## noncomp.i   % non-compliant in intervention group (non-adherers)
## alpha       type I error
## nc          Sample size for control (if not n/2)
## ni          Sample size for intervention (if not n/2)
## pr          set to T to print intermediate results
##
## non-compliance handled by an approximation of Eq. 5.4 of
## Lachin JM, Foulkes MA (1986): Evaluation of sample size and power for
## analyses of survival with allowance for nonuniform patient entry,
## losses to follow-up, noncompliance, and stratification.
## Here we're using log hazard ratio instead of their hazard difference

cpower <- function(tref,   
                   n,     
                   mc,
                   r,
                   accrual,
                   tmin,   
                   noncomp.c=0,
                   noncomp.i=0,
                   alpha=.05,  
                   nc, ni,
                   pr=TRUE)
{
  if(mc>1)
    stop("mc should be a fraction")

  ## Find mortality in intervention group
  mi <- (1-r/100)*mc

  if(missing(nc) | missing(ni)) {
    nc <- n/2; ni <- n/2
  } else n <- nc+ni

  if(pr) {
    cat("\nAccrual duration:",accrual,"y  Minimum follow-up:",tmin,"y\n")
    cat("\nTotal sample size:",n,"\n")
    cat("\nAlpha=",alpha,"\n")
    d <- c("Control","Intervention")
    m <- c(mc,mi)
    names(m) <- d
    cat("\n",tref,"-year Mortalities\n",sep=""); print(m)
  }

  ## Find exponential hazards for all groups
  lamc <- -logb(1-mc)/tref
  lami <- -logb(1-mi)/tref

  if(pr) {
    lam <- c(lamc,lami)
    names(lam) <- d
    cat("\nHazard Rates\n");
    print(lam)
  }

  ## Find probability that a subject will have her event observed during
  ## the study, for all groups
  tmax <- tmin+accrual
  pc <- if(accrual==0)
          1-exp(-lamc*tmin)
        else
          1-1/accrual/lamc*(exp(-tmin*lamc)-exp(-tmax*lamc))
  
  pi <- if(accrual==0)
          1-exp(-lami*tmin)
        else
          1-1/accrual/lami*(exp(-tmin*lami)-exp(-tmax*lami))

  if(pr) {
    p <- c(pc,pi)
    names(p) <- d
    cat("\nProbabilities of an Event During Study\n")
    print(p)
  }

  ## Find expected number of events, all groups
  mc <- pc*nc
  mi <- pi*ni

  if(pr) {
    m <- c(mc,mi)
    names(m) <- d
    cat("\nExpected Number of Events\n")
    print(round(m,1))
  }

  ## Find expected value of observed log hazard ratio
  delta <- logb(lami/lamc)
  if(pr)
    cat("\nHazard ratio:",format(exp(delta)),"\n")

  if(noncomp.c+noncomp.i>0) {
    if(pr)
      cat("\nDrop-in rate (controls):",noncomp.c,
          "%\nNon-adherence rate (intervention):",noncomp.i,"%\n",sep="")
    
    delta <- delta * (1 - (noncomp.c+noncomp.i)/100)
    if(pr)
      cat("Effective hazard ratio with non-compliance:",
          format(exp(delta)),"\n")
  }

  ## Find its variance
  v <- 1/mc + 1/mi
  
  ## Get same as /sasmacro/samsizc.sas if use 4/(mc+mi)

  sd <- sqrt(v)
  if(pr)
    cat("Standard deviation of log hazard ratio:",format(sd),"\n")

  z <- -qnorm(alpha/2)

  c(Power = 1 - (pnorm(z - abs(delta)/sd) - pnorm(-z - abs(delta)/sd)))
}
