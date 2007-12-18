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


lrcum <- function(a, b, c, d, conf.int=0.95)
  {
    if(any(is.na(a+b+c+d))) stop('NAs not allowed')
    if(min(a,b,c,d)==0)
      {
        warning('A frequency of zero exists.  Adding 0.5 to all frequencies.')
        a <- a + .5
        b <- b + .5
        c <- c + .5
        d <- d + .5
      }
    
    lrpos <- a/(a+c) / (b/(b+d))
    lrneg <- c/(a+c) / (d/(b+d))

    zcrit <- qnorm((1+conf.int)/2)
    
    varloglrpos <- 1/a - 1/(a+c) + 1/b - 1/(b+d)
    varloglrneg <- 1/d - 1/(b+d) + 1/c - 1/(a+c)
    upperlrpos <- exp(log(lrpos) + zcrit*sqrt(varloglrpos))
    lowerlrpos <- exp(log(lrpos) - zcrit*sqrt(varloglrpos))
    upperlrneg <- exp(log(lrneg) + zcrit*sqrt(varloglrneg))
    lowerlrneg <- exp(log(lrneg) - zcrit*sqrt(varloglrneg))

    lrposcum <- cumprod(lrpos)
    lrnegcum <- cumprod(lrneg)

    varloglrposcum <- cumsum(varloglrpos)
    varloglrnegcum <- cumsum(varloglrneg)

    upperlrposcum <- exp(log(lrposcum) + zcrit*sqrt(varloglrposcum))
    lowerlrposcum <- exp(log(lrposcum) - zcrit*sqrt(varloglrposcum))
    upperlrnegcum <- exp(log(lrnegcum) + zcrit*sqrt(varloglrnegcum))
    lowerlrnegcum <- exp(log(lrnegcum) - zcrit*sqrt(varloglrnegcum))

    structure(llist(lrpos, upperlrpos, lowerlrpos,
                    lrneg, upperlrneg, lowerlrneg,
                    lrposcum, upperlrposcum, lowerlrposcum,
                    lrnegcum, upperlrnegcum, lowerlrnegcum, conf.int),
              class='lrcum')
  }

print.lrcum <- function(x, dec=3, ...)
  {
    ci <- x$conf.int
    l <- paste('Lower', ci)
    u <- paste('Upper', ci)
    a <- with(x,
              cbind(lrpos,    lowerlrpos,    upperlrpos,
                    lrposcum, lowerlrposcum, upperlrposcum))
    b <- with(x,
              cbind(lrneg,    lowerlrneg,    upperlrneg,
                    lrnegcum, lowerlrnegcum, upperlrnegcum))
    a <- round(a, dec)
    b <- round(b, dec)
    colnames(a) <- c('LR+', l, u, 'Cum. LR+', l, u)
    colnames(b) <- c('LR-', l, u, 'Cum. LR-', l, u)
    rownames(a) <- rownames(b) <- rep('', nrow(a))
    print(a)
    cat('\n')
    print(b)
    invisible()
  }
