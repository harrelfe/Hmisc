## $Id$
## Relative risk estimation from binary responses
## See http://www.csm.ornl.gov/~frome/ES/RRMHex/MHanalysis.txt and
## http://www.csm.ornl.gov/~frome/ES/RRMHex for related code

mhgr <- function(y, group, strata, conf.int=.95)
  {
    group <- as.factor(group)
    i <- is.na(y) | is.na(group) | is.na(strata)
    if(any(i))
      {
        i      <- !i
        y      <- y[i]
        group  <- group[i]
        strata <- strata[i]
      }
    N <- tapply(y, list(group,strata), length)
    if(nrow(N) != 2) stop('only works for 2 groups')
    N[is.na(N)] <- 0
    s <- tapply(y, list(group,strata), sum)
    s[is.na(s)] <- 0
    n <- N[1,]
    m <- N[2,]
    x <- s[1,]
    y <- s[2,]
    N <- m + n
    tk<- x + y
    R <- x*m/N
    S <- y*n/N
    D <- (m*n*tk - x*y*N)/N/N
    rr <- sum(R)/sum(S)
    varlog <- sum(D)/(sum(R)*sum(S))
    sigma <- sqrt(varlog)
    z <- -qnorm((1-conf.int)/2)
    ci <- rr*c(exp(-z*sigma), exp(z*sigma))
    structure(list(rr=rr, ci=ci, conf.int=conf.int, N=table(group)),
              class='mhgr')
  }
print.mhgr <- function(x, ...)
  {
    cat('Mantel-Haenszel Risk Ratio and', x$conf.int, 'Greenland-Robins Confidence Interval\n\n')
    cat('Common Relative Risk:', x$rr, 'CI:', x$ci, '\n\n')
    cat('N in Each Group\n\n')
    print(x$N)
    invisible()
  }

