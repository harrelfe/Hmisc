popower <- function(p, odds.ratio, n, n1, n2, alpha=.05)
{
  if(missing(n))
    n <- n1+n2
  else {
    n1 <- n2 <- n/2
  }
  
  p <- p[!is.na(p)]
  if(abs(sum(p)-1)>.0001)
    stop('probabilities in p do not add up to 1')
  
  z <- qnorm(1-alpha/2)
  A <- n2/n1
  ps <- 1 - sum(p^3)
  V <- n1*n2*n/3/((n+1)^2)*ps
  power <- pnorm(abs(logb(odds.ratio))*sqrt(V) - z)
  eff <- ps/(1-1/n/n)
  structure(list(power=power, eff=eff), class='popower')
}


print.popower <- function(x, ...)
{
  cat('Power:',round(x$power,3),
      '\nEfficiency of design compared with continuous response:',
      round(x$eff,3),'\n\n')
  invisible()
}


posamsize <- function(p, odds.ratio, fraction=.5, 
                      alpha=.05, power=.8)
{
  p <- p[!is.na(p)]
  if(abs(sum(p)-1)>.0001)
    stop('probabilities in p do not add up to 1')

  A <- (1-fraction)/fraction
  log.or <- logb(odds.ratio)
  z.alpha <- qnorm(1-alpha/2)
  z.beta <- qnorm(power)
  ps <- 1 - sum(p^3)
  n <- 3*((A+1)^2)*(z.alpha+z.beta)^2/A/(log.or^2)/ps
  eff <- ps/(1-1/n/n)
  structure(list(n=n,eff=eff), class='posamsize')
}


print.posamsize <- function(x, ...)
{
  cat('Total sample size:',round(x$n,1),
      '\nEfficiency of design compared with continuous response:',
      round(x$eff,3),'\n\n')
  invisible()
}
