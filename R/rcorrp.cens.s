## Computes rank correlation measures between a variable X and a possibly
## censored Surv variable Y
## Rank correlation is extension of Somers' Dxy = 2(Concordance Prob-.5)
## See Harrell et al JAMA 1984(?)
## Set outx=T to exclude ties in X from computations (-> Goodman-Kruskal
##  gamma-type rank correlation)
## No. This is the version extended to paired predictions
## method=1: concordance=delta x1 < delta x2
## method=2: concordance=x1 concordant and x2 discordant

rcorrp.cens <- function(x1, x2, S, outx=FALSE, method=1)
{
  if(inherits(S, 'Surv')) {
    if(attr(S, 'type') != 'right')
      stop('only handles right censored times')
  } else S <- cbind(S, rep(1, length(S)))

  y <- S[,1]
  event <- S[,2]

  if(length(x1)!=length(x2))
    stop("x1 and x3 must have same length")
  
  if(length(y)!=length(x1))
    stop("y must have same length as x")
  
  if(method!=1 & method!=2)
    stop("method must be 1 or 2")

  miss <- is.na(x1+x2+y+event)
  nmiss <- sum(miss)
  if(nmiss>0) {
    miss <- !miss
    x1 <- x1[miss]
    x2 <- x2[miss]
    y <- y[miss]
    event <- event[miss]
  }
  
  n <- length(x1)
  if(n<2)
    stop("<2 non-missing observations")
  
  ne <- sum(event)
  storage.mode(x1) <- "double"
  storage.mode(x2) <- "double"
  storage.mode(y)  <- "double"
  storage.mode(event)  <- "logical"
  storage.mode(method) <- "integer"
  storage.mode(outx)   <- "logical"

  z <-
      .Fortran(F_cidxcp,x1,x2,y,event,length(x1),method,outx,
               nrel=double(1),nuncert=double(1),
               c1=double(1),c2=double(1),gamma1=double(1),gamma2=double(1),
               gamma=double(1),sd=double(1),c12=double(1),c21=double(1))
  
  r <- c(z$gamma,z$sd,z$c12,z$c21,n,nmiss,ne,z$nrel,z$nuncert,z$c1,z$c2,
         z$gamma1,z$gamma2)
  names(r) <- c("Dxy","S.D.","x1 more concordant","x2 more concordant",
                "n","missing","uncensored",
                "Relevant Pairs","Uncertain","C X1","C X2","Dxy X1","Dxy X2")
  r
}

improveProb <- function(x1, x2, y)
  {
    s <- is.na(x1+x2+y)
    if(any(s))
      {
        s <- !s
        x1 <- x1[s]
        x2 <- x2[s]
        y  <- y[s]
      }
    n <- length(y)
    y <- as.numeric(y)
    u <- sort(unique(y))
    if(length(u) != 2 || u[1] != 0 || u[2] != 1)
      stop('y must have two values: 0 and 1')
    r <- range(x1,x2)
    if(r[1] < 0 || r[2] > 1)
      stop('x1 and x2 must be in [0,1]')

    a <- y==1
    b <- y==0
    na <- sum(a)
    nb <- sum(b)
    d  <- x2 - x1
   
    nup.ev   <- sum(d[a] > 0); pup.ev   <- nup.ev/na
    nup.ne   <- sum(d[b] > 0); pup.ne   <- nup.ne/nb
    ndown.ev <- sum(d[a] < 0); pdown.ev <- ndown.ev/na
    ndown.ne <- sum(d[b] < 0); pdown.ne <- ndown.ne/nb
    
    nri.ev <- pup.ev - pdown.ev
    # se.nri.ev <- sqrt((pup.ev + pdown.ev)/na)  # old est under H0
    v.nri.ev <- (nup.ev + ndown.ev)/(na^2) - ((nup.ev - ndown.ev)^2)/(na^3)
    se.nri.ev <- sqrt(v.nri.ev)
    z.nri.ev <- nri.ev/se.nri.ev
    
    nri.ne   <- pdown.ne - pup.ne
    # se.nri.ne <- sqrt((pdown.ne + pup.ne)/nb)  # old est under H0
    v.nri.ne <- (ndown.ne + nup.ne)/(nb^2) - ((ndown.ne - nup.ne)^2)/(nb^3)
    se.nri.ne <- sqrt(v.nri.ne)
    z.nri.ne <- nri.ne/se.nri.ne

    nri <- pup.ev - pdown.ev - (pup.ne - pdown.ne)
    # old estimate under H0:
    # se.nri <- sqrt((pup.ev + pdown.ev)/na + (pup.ne + pdown.ne)/nb)
    se.nri <- sqrt(v.nri.ev + v.nri.ne)
    z.nri  <- nri/se.nri
    

    improveSens <-  sum(d[a])/na
    improveSpec <- -sum(d[b])/nb
    idi <- mean(d[a]) - mean(d[b])
    var.ev <- var(d[a])/na
    var.ne <- var(d[b])/nb
    se.idi <- sqrt(var.ev + var.ne)
    z.idi <- idi/se.idi

    structure(llist(n, na, nb, pup.ev, pup.ne, pdown.ev, pdown.ne,
                    nri,    se.nri,    z.nri,
                    nri.ev, se.nri.ev, z.nri.ev,
                    nri.ne, se.nri.ne, z.nri.ne,
                    improveSens, improveSpec,
                    idi, se.idi, z.idi, labels=FALSE), class='improveProb')
  }

print.improveProb <- function(x, digits=3, conf.int=.95,  ...)
  {
    cat('\nAnalysis of Proportions of Subjects with Improvement in Predicted Probability\n\n')
    cat('Number of events:', x$na,'\tNumber of non-events:', x$nb, '\n\n')

    p <- matrix(c(x$pup.ev, x$pup.ne, x$pdown.ev, x$pdown.ne),
                dimnames=list(c(
                  'Increase for events     (1)',
                  'Increase for non-events (2)',
                  'Decrease for events     (3)',
                  'Decrease for non-events (4)'),
                  'Proportion'))
    cat('Proportions of Positive and Negative Changes in Probabilities\n\n')
    print(p, digits=digits)

    zpci <- function(m, se, conf.int)
      {
        z <- qnorm((1+conf.int)/2)
        cbind(m/se, 2 * pnorm(- abs(m/se)), m - z*se, m + z*se)
      }

    p <- cbind(c(x$nri, x$nri.ev, x$nri.ne),
               c(x$se.nri, x$se.nri.ev, x$se.nri.ne), 
               zpci(c(x$nri, x$nri.ev, x$nri.ne),
                   c(x$se.nri, x$se.nri.ev, x$se.nri.ne),
                   conf.int=conf.int))
    low <- paste('Lower', conf.int)
    hi  <- paste('Upper', conf.int)
    dimnames(p) <- list(c('NRI            (1-3+4-2)',
                          'NRI for events     (1-3)',
                          'NRI for non-events (4-2)'),
                        c('Index', 'SE', 'Z', '2P', low, hi))
    cat('\n\nNet Reclassification Improvement\n\n')
    print(p, digits=digits)

    cat('\n\nAnalysis of Changes in Predicted Probabilities\n\n')

    p <- matrix(c(x$improveSens, x$improveSpec),
                dimnames=list(c('Increase for events (sensitivity)',
                  'Decrease for non-events (specificity)'),
                  'Mean Change in Probability'))
    print(p, digits=digits)

    cat("\n\nIntegrated Discrimination Improvement\n (average of sensitivity and 1-specificity over [0,1];\n also is difference in Yates' discrimination slope)\n\n")
    p <- c(x$idi, x$se.idi, zpci(x$idi, x$se.idi, conf.int=conf.int))
    names(p) <- c('IDI', 'SE', 'Z', '2P', low, hi)
    print(p, digits=digits)
    invisible()
  }
