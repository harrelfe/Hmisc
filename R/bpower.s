bpower <- function(p1, p2, odds.ratio, percent.reduction, n, n1, n2, 
                   alpha=.05)
{
  if(!missing(odds.ratio))
    p2 <- p1*odds.ratio/(1-p1+p1*odds.ratio)
  else if(!missing(percent.reduction))
    p2 <- p1*(1-percent.reduction/100)

  if(!missing(n)) {
    n1 <- n2 <- n/2
  }
  z <- qnorm(1-alpha/2)
  q1 <- 1-p1
  q2 <- 1-p2
  pm <- (n1*p1+n2*p2)/(n1+n2)
  ds <- z*sqrt((1/n1 + 1/n2)*pm*(1-pm))
  ex <- abs(p1-p2)
  sd <- sqrt(p1*q1/n1+p2*q2/n2)
  c(Power = 1-pnorm((ds-ex)/sd)+pnorm((-ds-ex)/sd) )
}


bsamsize <- function(p1, p2, fraction=.5, alpha=.05, power=.8)
{
  z.alpha <- qnorm(1-alpha/2)
  z.beta  <- qnorm(power)

  ratio <- (1-fraction)/fraction
  p <- fraction*p1+(1-fraction)*p2

  n1 <- (z.alpha * sqrt((ratio+1) * p * (1-p)) +
         z.beta * sqrt(ratio * p1 * (1-p1) + p2 * (1 - p2))
        )^2/ratio/((p1-p2)^2)
  
  n2 <- ratio*n1
  c(n1=n1, n2=n2)
}

ballocation <- function(p1, p2, n, alpha=.05)
{
  q1 <- 1-p1
  q2 <- 1-p2

  f.minvar.diff <- 1/(1+sqrt(p2*q2/(p1*q1)))
  f.minvar.ratio <- 1/(1+sqrt(p1*q2/p2/q1))

  z <- c(fraction.group1.min.var.diff=f.minvar.diff,
         fraction.group1.min.var.ratio=f.minvar.ratio,
         fraction.group1.min.var.logodds=1-f.minvar.diff)

  if(!missing(n)) {
    possf <- seq(.001,.999,length=1000)
    pow <- bpower(p1, p2, n1=n*possf, n2=n*(1-possf), alpha=alpha)
    ## fun <- function(f, n, p1, p2, alpha) bpower(p1, p2, n1=f*n, n2=(1-f)*n, alpha=alpha)
    ## f.maxpow <- optimize(fun, lower=.01, upper=.99, maximum=T,
    ##                      n=n, p1=p1, p2=p2, alpha=alpha)$maximum
    f <- possf[pow==max(pow)]
    f <- f[abs(f-.5)==min(abs(f-.5))]
    z <- c(z, fraction.group1.max.power=f[1])
  }
  z
}

bpower.sim <- function(p1, p2, odds.ratio, percent.reduction, n, n1, n2, 
                       alpha=.05, nsim=10000)
{
  if(!missing(odds.ratio))
    p2 <- p1*odds.ratio/(1-p1+p1*odds.ratio)
  else if(!missing(percent.reduction))
    p2 <- p1*(1-percent.reduction/100)

  if(!missing(n)) {
    n1 <- n2 <- round(n/2)
  }
  n <- n1+n2

  if(length(p1)+length(p2)+length(n1)+length(n2)+length(alpha)+length(nsim)!=6)
    stop('all arguments must have length 1')

  chi2 <- qchisq(1-alpha, 1)

  d1 <- rbinom(nsim, n1, p1)
  d2 <- rbinom(nsim, n2, p2)
  chisq <- n*(d1*(n2-d2)-(n1-d1)*d2)^2/(d1+d2)/(n-d1-d2)/n1/n2
  power <- mean(chisq>chi2)
  se <- sqrt(power*(1-power)/nsim)
  c(Power=power,Lower=power-1.96*se,Upper=power+1.96*se)
}
