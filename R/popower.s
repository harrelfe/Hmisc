popower <- function(p, odds.ratio, n, n1, n2, alpha=.05)
{
  if(missing(n))
    n <- n1 + n2
  else {
    n1 <- n2 <- n / 2
  }
  
  p <- p[! is.na(p)]
  if(abs(sum(p) - 1) > .00001)
    stop('probabilities in p do not add up to 1')
  
  z <- qnorm(1 - alpha / 2)
  A <- n2 / n1
  ps <- 1 - sum(p ^ 3)
  V <- n1 * n2 * n / 3 / ((n + 1) ^ 2) * ps
  power <- pnorm(abs(logb(odds.ratio)) * sqrt(V) - z)
  eff <- ps / (1 - 1 / n / n)
  structure(list(power=power, eff=eff), class='popower')
}


print.popower <- function(x, ...)
{
  cat('Power:',round(x$power,3),
      '\nEfficiency of design compared with continuous response:',
      round(x$eff, 3),'\n\n')
  invisible()
}


posamsize <- function(p, odds.ratio, fraction=.5, 
                      alpha=.05, power=.8)
{
  p <- p[!is.na(p)]
  if(abs(sum(p) - 1) > .00001)
    stop('probabilities in p do not add up to 1')

  A <- (1 - fraction) / fraction
  log.or <- logb(odds.ratio)
  z.alpha <- qnorm(1 - alpha / 2)
  z.beta <- qnorm(power)
  ps <- 1 - sum(p ^ 3)
  n <- 3 * ((A + 1) ^ 2) * (z.alpha + z.beta) ^ 2 / A / (log.or ^ 2) / ps
  eff <- ps / (1 - 1 / n / n)
  structure(list(n=n, eff=eff), class='posamsize')
}


print.posamsize <- function(x, ...)
{
  cat('Total sample size:',round(x$n, 1),
      '\nEfficiency of design compared with continuous response:',
      round(x$eff, 3),'\n\n')
  invisible()
}

pomodm <- function(x=NULL, p, odds.ratio=1) {
  if(length(x) && (length(x) != length(p)))
    stop('p and x must have same length')
  if(abs(sum(p) - 1) > .00001)
    stop('probabilities do not sum to 1')

  ## Compute cumulative probabilities (exceedances)
  cp <- function(p) c(1, 1 - cumsum(p)[- length(p)])

  ## Compute individual probabilities given exceedance probabilities
  ip <- function(ep) {
    p <- c(-diff(ep), ep[length(ep)])
    if(abs(sum(p) - 1.) > 1e-7) stop('logic error')
    p
  }
  ## Function to alter a vector of individual probabilities by a given
  ## odds ratio on the exceedance probabilities
  pmod <- function(p, or) {
    ## Compute exceedence probabilities
    ep <- cp(p)
    ## Apply odds ratio
    ep <- plogis(qlogis(ep) + log(or))
    ## Get back to individual probabilities
    ip(ep)
  }

  ## Convert individual probabilities under the odds ratio
  p <- pmod(p, odds.ratio)
  if(! length(x)) return(p)

  ## Compute mean and weighted median
  xbar  <- sum(p * x)
  xmed1 <- approx(cumsum(p), x, xout=0.5)$y
  xmed2 <- approx(rev(cumsum(rev(p))), x, xout=0.5)$y
  xmed  <- (xmed1 + xmed2) / 2
  c(mean=xbar, median=xmed)
  }
