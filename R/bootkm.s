bootkm <- function(S, q=.5, B=500, times, pr=TRUE)
{
  sRequire('survival')
  tthere <- !missing(times)
  if(tthere && length(times)>1)
    stop('presently bootkm only works for a single time')
  
  S <- S[!is.na(S),]
  n <- nrow(S)
  stratvar <- factor(rep(1,nrow(S)))
  f <- survival::survfitKM(stratvar, S)
  tt <- c(0, f$time)
  ss <- c(1, f$surv)
  if(!tthere) {
    if(ss[length(ss)] > q) 
      stop(paste('overall Kaplan-Meier estimate does not fall below',q))
    
  } else {
    if(tt[length(tt)] < times)
      stop(paste('overall Kaplan-Meier estimate not defined to time',times))
  }

  ests <- double(B)

  for(i in 1:B) {
    if(pr && (i %% 10)==0)
      cat(i,'\r')
    
    f <- survival::survfitKM(stratvar, S[sample(n,n,replace=TRUE),],
                   se.fit=FALSE, conf.type='none')
    tt <- c(0, f$time)
    ss <- c(1, f$surv)
    ests[i] <- if(tthere)
                 approx(tt, ss, xout=times, method='constant', f=0)$y
               else
                 min(tt[ss <= q])  #is NA if none
  }
  if(pr)
    cat('\n')
  
  ests
}
