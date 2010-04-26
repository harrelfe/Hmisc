ldBands <- function(n=length(times), times=NULL,  alpha=.05,
                    sided=2, alphaLower=alpha/2, alphaUpper=alpha/2,
                    information=NULL,
                    spending=c('OBrien-Fleming','Pocock','alpha*t^phi',
                               'Hwang-Shih-DeCani'),
                    phi=1,
                    spending2=c('OBrien-Fleming','Pocock','alpha*t^phi',
                                'Hwang-Shih-DeCani'),
                    phi2=phi,
                    truncate=Inf, power=NULL, pr=TRUE)
{
  if(missing(n) && missing(times))
    stop('must specify n or times')
  
  if(!length(times))
    times <- seq(0,1,length=n+1)[-1]
  
  spending  <- match.arg(spending)
  spending2 <-
    if(missing(spending2))
      spending
    else
      match.arg(spending2)
  
  alpha <- alphaLower+alphaUpper
  if(length(power) && length(information))
    stop('information may not be specified when power is')
  
  sp <- c('OBrien-Fleming'=1,'Pocock'=2,'alpha*t^phi'=3,
          'Hwang-Shih-DeCani'=4)[spending]
  if(sided != 3)
    {
      spending2 <- spending
      sp2 <- sp
    }
  else
    sp2 <- c('OBrien-Fleming'=1,'Pocock'=2,'alpha*t^phi'=3,
             'Hwang-Shih-DeCani'=4)[spending2]

  if(phi==0)
    {
      warning('phi may not be zero.  Set to 1')
      phi <- 1
    }
  
  if(length(times)) times <- sort(times)
  
  if(length(information)) information <- sort(information)

  ## Note: times always has length>0 below
  ## When power is given, assumes spending function always determines
  ## bounds
  p <- if(under.unix) function(x) paste(x,'\\n',sep='',collapse='')
       else function(x) paste(x,'\n', sep='',collapse='')
  
  ## If running Linux/Unix can avoid creating an input file, just pipe
  ## echo output as stdin.  echo needs embedded '\n' hence output \\n
    
  w <- paste(if(under.unix) 'echo "' else '',
             p(0),
             p(if(length(power))
               2
             else 1),
             
             p(n),
             p(if(length(times))
               c(0,paste(times,collapse=' '))
             else 1),
             p(if(length(power))
               1
             else if(length(information))
               c(1,paste(information,collapse=' '))
             else 0),
             
             p(alpha), p(sided),
             if(sided==3)
             p(alphaLower)
             else '',
             
             p(sp),
             if(sp %in% 3:4)
             p(phi)
             else '',
             
             if(sided==3)
             p(c(sp2,
                 if(sp2 %in% 3:4)
                 phi2
                 else NULL))
             else '',
             
             p(if(is.infinite(truncate))
               0
             else c(1,truncate)),
             
             if(length(power))
             p(power)
             else '',
             
             p(0),p(0),'', if(under.unix)'"',
             sep='')

  com <- if(under.unix) paste(w, '| ld98') else
  {
    fin <- tempfile()
    cat(w, file=fin)
    paste('ld98 <',fin)
  }
  w <- sys(com)
  if(!under.unix) unlink(fin)
  
  
  if(pr)
    cat(w,sep='\n')
  w <- w[w != '']
  if(length(power)) {
    i <- grep('drift =',w)
    j <- substring.location(w[i], 'drift =')$last
    drift <- as.numeric(substring(w[i],j+1))
  } else drift <- NULL
  
  head <- grep(if(length(power)) 'cum exit pr' else 'cum alpha',
               w)
  
  w <- w[(head+1):length(w)]
  tail <- grep(if(length(power))
               'Would you like to start again'
  else 'Do you want to see a graph',
               w)
  
  w <- w[1:(tail-1)]
  z <- if(.R.) unPaste(w, ' +') else
   unPaste(sedit(w,'  ',' '),' ')

  if(length(power))
    {
      i <- 1
      tim        <- as.numeric(z[[i+2]])
      if(max(abs(tim-times)) > .01)
        stop('program logic error')
    
      low       <- as.numeric(z[[i+3]])
      hi        <- as.numeric(z[[i+4]])
      exit.prob <- as.numeric(z[[i+5]])
      cum.exit.prob <- as.numeric(z[[i+6]])
      data <- data.frame(time=times, lower=low,upper=hi,
                         exit.prob=exit.prob,cum.exit.prob=cum.exit.prob)
    }
  else
    {
      tim <- as.numeric(z[[2]])
      if(max(abs(tim-times)) > .01)
        stop('program logic error')
    
      i <- if(length(information))1
      else 0
    
      low       <- as.numeric(z[[3+i]])
      hi        <- as.numeric(z[[4+i]])
      alpha.inc <- as.numeric(z[[5+i]])
      cum.alpha <- as.numeric(z[[6+i]])
      data <- data.frame(time=times, lower=low,upper=hi,
                         alpha.inc=alpha.inc,cum.alpha=cum.alpha)
    }
  
  if(length(information))
    data$information <- information
  
  res <- structure(list(data=data, power=power, drift=drift,
                        type=if(length(power))
                        'power'
                        else 'boundaries',
                        
                        n=n, alpha=alpha, alphaLower=alphaLower,
                        alphaUpper=alphaUpper, sided=sided,
                        spending=spending, phi=phi,
                        spending2=spending2, phi2=phi2,
                        truncate=truncate),
                   class='ldBands')
  res
}


print.ldBands <- function(x, ...)
{
  if(x$sided < 3) {
    cat('alpha=',format(x$alpha),'\t',x$sided,
        '-sided  \tSpending function:',x$spending,sep='')
    if(x$spending=='alpha*t^phi')
      cat('\tExponent:',x$phi,sep='')
    
    if(x$spending=='Hwang-Shih-DeCani')
      cat('\tPhi:',x$phi,sep='')
  } else
  {
    cat('Lower bounds:\n\n')
    cat('alpha=',format(x$alphaLower),
        '\tSpending function:',x$spending,sep='')
    if(x$spending=='alpha*t^phi')
      cat('\tExponent:',x$phi,sep='')
    
    if(x$spending=='Hwang-Shih-DeCani')
      cat('\tPhi:',x$phi,sep='')
    
    cat('\n\nUpper bounds:\n\n')
    cat('alpha=',format(x$alphaUpper),
        '\tSpending function:',x$spending2,sep='')
    if(x$spending2=='alpha*t^phi')
      cat('\tExponent:',x$phi2,sep='')
    
    if(x$spending2=='Hwang-Shih-DeCani')
      cat('\tPhi:',x$phi2,sep='')
  }
  
  cat('\n\n')
  if(length(x$power))
    cat('Power:',x$power,'\tDrift:',x$drift,'\n\n')
  
  print(x$data)
  invisible()
}


plot.ldBands <- function(x, xlab='Time', ylab='Z', actual=NULL,
                         type='b', labels=NULL, ...)
{
  d <- x$data
  mfr <- par('mfrow')
  if(prod(mfr) != 1)
    {
      on.exit(par(mfrow=mfr))
      par(mfrow=c(2,1))
    }
  
  plot(d$time, d$lower, type=type, ylim=range(d$lower,d$upper),
       xlab=xlab, ylab=ylab, axes=length(labels)==0)
  if(length(labels))
    {
      axis(2)
      if(length(labels) != length(d$time))
        stop('length of labels not equal to length of times generated by ldBands')
      axis(1, at=d$time, labels=labels)
    }
  
  lines(d$time, d$upper, type=type)
  if(length(actual))
    points(actual[[1]],actual[[2]], pch=16)
  
  if(x$type=='power')
    labcurve(list(Instant   =list(d$time,d$exit.prob),
                  Cumulative=list(d$time,d$cum.exit.prob)),
             lty=2:1, pl=TRUE, type=type,
             xlab=xlab, ylab='Exit Probability')
  
  invisible()
}


summary.ldBands <- function(object, stdiff=NULL, n=NULL,
                            p1=NULL, p2=NULL,
                            hr=NULL, events=NULL,
                            pbar=NULL, sd=NULL, ...)
{  
  if(length(pbar) + length(sd) == 0)
    {
      drift <- object$drift
      if(!length(drift))
        stop('did not specify power= to ldBands')

      if(length(p1))
        stdiff <- (p1-p2)/sqrt(p1*(1-p1)+p2*(1-p2))
    
      if(length(events))
        hr <- exp(2*drift/sqrt(events))
    
      if(length(hr))
        events <- 4*((drift/log(hr))^2)
  
      if(length(stdiff)+length(n)+length(events)==0)
        stop('must specify stdiff, n, hr, or events')

      if(length(stdiff))
        n <- (drift/stdiff)^2
      else if(length(n))
        stdiff <- drift/sqrt(n)
    
      structure(list(stdiff=stdiff, n=n, p1=p1, p2=p2, hr=hr, events=events,
                     drift=drift, power=object$power),
                class='summary.ldBands')
    }
  else
    {
      if(length(n) != nrow(object$data))
        stop('length of n must equal number of looks')
      d <- object$data
      d$n <- n
      if(length(pbar))
        {
          sepdiff      <- sqrt(2*pbar*(1-pbar)/n)
          d$diff.lower <- d$lower*sepdiff
          d$diff.upper <- d$upper*sepdiff
          selogOR      <- sqrt(2/(pbar*(1-pbar)*n))
          d$or.lower   <- exp(d$lower*selogOR)
          d$or.upper   <- exp(d$upper*selogOR)
          object$data     <- d
          object
        }
      else
        {
          semeandiff   <- sd*sqrt(2/n)
          d$diff.lower <- d$lower*semeandiff
          d$diff.upper <- d$upper*semeandiff
          object$data     <- d
          object
        }
    }
}


print.summary.ldBands <- function(x, ...)
{
  cat('Drift:',x$drift,'\tPower:',x$power,sep='')
  if(length(x$p1))
    cat('\tp1:',x$p1,'\tp2:',x$p2,sep='')
  
  cat('\n\n')
  if(length(x$n))
    cat('Maximum sample size per treatment:', x$n,'\n',sep='')
  
  if(length(x$events))
    cat('Maximum number of events (both treatments combined):',
        x$events,'\n',sep='')
  ## Thanks: marcel wolbers <marcel.wolbers@gmx.ch>
  if(length(x$stdiff))
    cat('Detectible standardized effect:\t', x$stdiff,'\n',sep='')
  if(length(x$hr))
    cat('Hazard ratio:\t',x$hr,'\n',sep='')
  
  invisible()
}
