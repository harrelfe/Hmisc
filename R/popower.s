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
  structure(list(power=power, eff=eff, approx.se=1./sqrt(V)),
            class='popower')
}


print.popower <- function(x, ...)
{
  cat('Power:',round(x$power,3),
      '\nEfficiency of design compared with continuous response:',
      round(x$eff, 3),
      '\nApproximate standard error of log odds ratio:',
      round(x$approx.se, 4), '\n\n')
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
  if(length(x) && any(diff(x) <= 0)) stop('x is not sorted or has duplicates')

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

simPOcuts <- function(n, nsim=10, odds.ratio=1, p) {
  if(abs(sum(p) - 1.) > 1e-5)
    stop('probabilities in p must sum to 1')
  p0   <- p
  p1   <- pomodm(p=p0, odds.ratio=odds.ratio)
  lp   <- length(p)
  yval <- if(length(names(p))) names(p) else as.character(1 : lp)
  or <- matrix(NA, nrow=nsim, ncol=lp - 1,
               dimnames=list(paste('Simulation', 1 : nsim),
                             paste0('y>=', yval[-1])))
  for(i in 1 : nsim) {
    y0 <- sample(1 : lp, n / 2, prob=p0,  replace=TRUE)
    y1 <- sample(1 : lp, n / 2, prob=p1,  replace=TRUE)
    for(ycut in 2 : lp){
      prop0 <- mean(y0 >= ycut)
      prop1 <- mean(y1 >= ycut)
      or[i, ycut - 1] <- (prop1 / (1. - prop1)) / (prop0 / (1. - prop0))
    }
  }
  or
  }
  

simRegOrd <- function(n, nsim=1000, delta=0, odds.ratio=1, sigma,
                      p=NULL, x=NULL, X=x, Eyx, alpha=0.05, pr=FALSE) {
  if (!requireNamespace("rms", quietly = TRUE))
    stop("This function requires the 'rms' package.")
  
  if(length(x) && (length(x) != n))
    stop('x must be omitted or have length n')

  if((n %% 2) != 0) stop('n must be an even integer')
  
  treat <- c(rep(0, n / 2), rep(1, n / 2))
  X <- cbind(X, treat)

  betas <- se <- rep(NA, nsim)
  if(length(p)) {
    p1 <- pomodm(p=p, odds.ratio=odds.ratio)
    yordinal <- 0 : (length(p) - 1)
  }

  xb <- delta * treat + (if(length(x)) Eyx(x) else 0)

  for(i in 1 : nsim) {
    if(pr) cat('Iteration', i, '\r')
    y <- xb + rnorm(n, mean=0, sd=sigma)
    if(length(p)) {
      ## Override y with length(p) - 1 ordinal categories
      yo <- ifelse(treat == 0,
                   sample(yordinal, n, prob=p,  replace=TRUE),
                   sample(yordinal, n, prob=p1, replace=TRUE))
      ymax <- max(y)
      y <- ifelse(yo == 0, y, ceiling(ymax) + yo)
    }

    f <- rms::orm.fit(X, y, maxit=20)
    if(! f$fail) {
      k <- length(coef(f))
      betas[i] <- coef(f)[k]   ## coef of treatment
      vv       <- vcov(f, intercepts='none')
      k        <- nrow(vv)
      se[i]    <- sqrt(vv[k, k])
      }
  }
  if(pr) cat('\n')

  pvals <- 1 - pchisq((betas / se) ^ 2, 1)
  power <- mean(pvals < alpha, na.rm=TRUE)
  list(n=n, delta=delta, sigma=sigma, power=power, betas=betas, se=se,
       pvals=pvals)
}


propsPO <- function(formula, odds.ratio=NULL, ref=NULL, data=NULL,
                    ncol=NULL, nrow=NULL) {
  v  <- all.vars(formula)
  d  <- model.frame(formula, data=data)
  y  <- d[[v[1]]]
  yl <- label(y, default=v[1])
  y  <- as.factor(y)
  d[[v[1]]] <- y
  x  <- d[[v[2]]]
  xl <- label(x, default=v[2])
  s  <- sn <- NULL
  sl <- NULL
  if(length(v) > 2) {
    if(length(odds.ratio))
      stop('odds ratio may not be specified when a stratification variable is included')
    s <- d[[v[3]]]
    sl <- label(s, default=v[3])
    s <- as.factor(s)
    sn <- 's'
  }
  names(d) <- c('y', 'x', sn)
  ## ggplot2 bar chart puts first category at the top
  ## Let's put it at the bottom

  revo <- function(z) {
    z <- as.factor(z)
    factor(z, levels=rev(levels(as.factor(z))))
    }
  
  # For each x compute the vector of proportions of y categories
  # Assume levels are in order
  # Put numerators and denominators into a character string so that
  # data.table [ operator can operate on one variable
  # The delimiter is a single space
  g <- function(y) {
    tab <- table(y)
    structure(paste(tab, rep(sum(tab), length(tab))), names=names(tab))
  }
  atxt <- function(d, strat=NULL, or=FALSE) {
    if(! or) {
      z <- d$prop
      num   <- as.numeric(sub(' .*', '', z)) # get first number
      denom <- as.numeric(sub('.* ', '', z)) # get second number
      d$prop <- num / denom
      }
    d$txt <- paste0(sl, if(length(strat)) ': ', as.character(strat),
                  if(length(strat)) '<br>',
                  xl, ': ', as.character(d$x), '<br>',
                  yl, ': ', as.character(d$y), '<br>',
                  'Proportion: ', round(d$prop, 3),
                  if(! or) '\u2003',
                  if(! or) markupSpecs$html$frac(num, denom, size=90))
    d
    }

  d <- data.table(d)
  
  if(! length(s)) {   # stratification variable not present
    p  <- d[, as.list(g(y)), by=x]
    pm <- melt(p, id=1, variable.name='y', value.name='prop')
    pm <- atxt(pm)

    plegend <- guides(fill=guide_legend(title=yl))
    if(! length(odds.ratio)) {
      gg <- ggplot(pm, aes(x=as.factor(x), y=prop, fill=revo(y), label=txt)) +
        geom_col() + plegend + xlab(xl) + ylab('Proportion')
      return(gg)
    }
  } else {   # stratification variable present; odds ratio must be absent
    p  <- d[, as.list(g(y)), by=.(x, s)]
    pm <- melt(p, id=c('x', 's'), variable.name='y', value.name='prop')
    pm <- atxt(pm, pm$s)
    plegend <- guides(fill=guide_legend(title=yl))
    gg <- ggplot(pm, aes(x=as.factor(x), y=prop, fill=revo(y), label=txt)) +
        facet_wrap(~ s, ncol=ncol, nrow=nrow) +
        geom_col() + plegend + xlab(xl) + ylab('Proportion')
    return(gg)
    }

  ## odds.ratio is present
  
  if(! length(ref)) ref <- p$x[1]
  pfx <- as.matrix(p[x == ref, -1])
  propfirstx <- as.numeric(sub(' .*', '', pfx)) /
                as.numeric(sub('.* ', '', pfx))
  
  .g. <- function(x) {
    w <- pomodm(p=propfirstx, odds.ratio=odds.ratio(x))
    names(w) <- levels(y)
    w
  }
  pa <- d[, as.list(.g.(x)), by=x]
  pma <- melt(pa, id=1, variable.name='y', value.name='prop')
  pma <- atxt(pma, or=TRUE)
  w <- rbind(cbind(type=1, pm),
             cbind(type=2, pma))
  w$type <- factor(w$type, 1 : 2,
                   c('Observed', 'Asssuming Proportional Odds'))
  ggplot(w, aes(x=as.factor(x), y=prop, fill=revo(y), label=txt)) +
    geom_col() +
    facet_wrap(~ type, nrow=2) +
    plegend + xlab(xl) + ylab('Proportion')
}

utils::globalVariables(c('.', 'prop'))

propsTrans <- function(formula, data=NULL, labels=NULL, arrow='\u2794',
                       maxsize=12, ncol=NULL, nrow=NULL) {
  v  <- all.vars(formula)
  d  <- model.frame(formula, data=data)
  y  <- as.factor(d[[v[1]]])
  x  <- d[[v[2]]]
  xlab <- label(x, default=v[2])
  id   <- as.character(d[[v[3]]])
  uid  <- sort(unique(id))
  nid  <- length(uid)

  times <- if(is.factor(x)) levels(x) else sort(unique(x))
  nt <- length(times)

  itrans <- integer(0)
  Prev   <- Cur <- Frac <- character(0)
  prop   <- frq <- numeric(0)

  mu    <- markupSpecs$html
  arrowbr <- paste0(' ', arrow, '<br>')
  arrow   <- paste0(' ', arrow, ' ')

  for(it in 2 : nt) {
    prev <- cur <- rep(NA, nid)
    names(prev) <- names(cur) <- uid
    i <- x == times[it - 1]
    j <- x == times[it]
    prev[id[i]] <- as.character(y[i])
    cur [id[j]] <- as.character(y[j])
    tab   <- table(prev, cur)
    rowf  <- rowSums(tab)
    tab   <- as.data.frame(tab)
#    tab   <- subset(tab, Freq > 0)
    tab$denom <- rowf[tab$prev]
    tab$prop  <- tab$Freq / tab$denom
    frq   <- c(frq, tab$Freq)
    Prev  <- c(Prev, as.character(tab$prev))
    Cur   <- c(Cur,  as.character(tab$cur))
    prop  <- c(prop, tab$Freq / tab$denom)
    Frac  <- c(Frac, paste0(round(tab$Freq / tab$denom, 3),
                            mu$lspace,
                            mu$frac(tab$Freq, tab$denom, size=90)))

    k     <- length(tab$Freq)
    itrans <- c(itrans, rep(it, k))
  }

  Prev  <- factor(Prev, levels(y))
  Cur   <- factor(Cur,  levels(y))
  trans <- factor(itrans, 2 : nt,
                  labels=paste0(xlab, ' ',
                                times[1 : (nt - 1)], arrow, times[2 : nt]))
  transp <- factor(itrans, 2 : nt,
                  labels=paste0(xlab, ' ',
                                times[1 : (nt - 1)], arrow, times[2 : nt]))

  w <- data.frame(trans, transp, Prev, Cur, prop, Frac, frq,
                  txt=if(! length(labels))
                        ifelse(Prev == Cur,
                         paste0('Stay at:', as.character(Prev)),
                         paste0(as.character(Prev),
                               arrowbr, as.character(Cur)))
                  else
                    ifelse(Prev == Cur,
                      paste0('Stay at:', labels[as.integer(Prev)]),
                      paste0(labels[as.integer(Prev)],
                             arrowbr, labels[as.integer(Cur)])))
  w$txt <- paste0(w$transp, '<br>', w$txt, '<br>', w$Frac)
  w <- subset(w, frq > 0)
  
  ggplot(w, aes(x=Prev, y=Cur, size=prop, label=txt)) +
    facet_wrap(~ trans, ncol=ncol, nrow=nrow) +
    geom_point() + scale_size(range = c(0, maxsize)) + 
    xlab('Previous State') + ylab('Current State') +
    guides(size = guide_legend(title='Proportion'))

#  ggplot(w, aes(x=Prev, y=prop, fill=Cur)) +
#    facet_wrap(~ trans, ncol=ncol, nrow=nrow) +
#    geom_col() + #scale_size(range = c(0, 12)) + 
#    xlab('Previous State') + ylab('Proportion') +
#    guides(fill = guide_legend(title='Current State'))
  
}

multEventChart <-
  function(formula, data=NULL,
           absorb=NULL, sortbylast=FALSE,
           colorTitle=label(y), eventTitle='Event',
           palette='OrRd',
           eventSymbols=c(15, 5, 1:4, 6:10),
           timeInc=min(diff(unique(x))/2)) {

  v     <- all.vars(formula)
  d     <- model.frame(formula, data=data)
  y     <- as.factor(d[[v[1]]])
  y     <- factor(y, levels=rev(levels(y)))
  x     <- d[[v[2]]]
  xlab  <- label(x, default=v[2])
  id    <- as.factor(d[[v[3]]])

  # Optionally sort subjects by last status, assuming levels of y
  # are in ascending order of badness
  if(sortbylast) {
    last <- tapply(1 : length(y), id,
                   function(i) { 
                     times  <- x[i]
                     status <- y[i]
                     as.integer(status[which.max(times)])
                   })
    
    i <- order(last, levels(id), decreasing=TRUE)
    id <- factor(id, levels=levels(id)[i])
  } else  id    <- factor(id, levels=rev(levels(id)))
  ab    <- as.character(y) %in% absorb
  nay   <- setdiff(levels(y), absorb)
  event       <- y
  event[! ab] <- NA
  y[ab]       <- NA
  de <- subset(data.frame(id, x, y, event), ! is.na(event))
  ggplot(mapping=aes(x = id, y = x, fill = y)) +
    scale_fill_brewer(colorTitle, palette=palette, direction=-1, breaks=nay) +
    geom_segment(aes(x = id, xend = id, y = 0, yend = x - timeInc),
                 lty = 3) +
    geom_tile(width = timeInc) + 
    scale_y_continuous(breaks=min(x) : max(x)) +
    geom_point(aes(x = id, y = x - timeInc, shape = event), data=de) + 
    scale_shape_manual(eventTitle, values=eventSymbols[1 : length(absorb)],
                       labels=c(absorb)) +
    guides(fill=guide_legend(override.aes=list(shape=NA), order=1)) +
    coord_flip() + labs(y=xlab, x='')
  }

utils::globalVariables('txt')
