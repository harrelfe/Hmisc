##Dan Heitjan  dheitjan@biostats.hmc.psu.edu

ftupwr <- function(p1,p2,bign,r,alpha)
{
  ## Compute the power of a two-sided level alpha test of the
  ## hypothesis that pi1=pi2, when pi1=p1, pi2=p2, and there are
  ## bign observations, bign/(1+r) in group 1 and r*bign/(1+r) in
  ## group 2.  This is based on the two-tailed test version of
  ## formula (6) in Fleiss, Tytun and Ury (1980 Bcs 36, 343--346).
  ## This may be used for del not too small (del>=0.1) and r not
  ## too big or small (0.33<=r<=3).
  ##   Daniel F. Heitjan, 30 April 1991
  mstar <- bign/(r+1)
  del <- abs(p2-p1)
  rp1 <- r+1
  zalp <- qnorm(1-alpha/2)
  pbar <- (p1+r*p2)/(1+r)
  qbar <- 1-pbar
  num <- (r*del^2*mstar-rp1*del)^0.5-zalp*(rp1*pbar*qbar)^0.5
  den <- (r*p1*(1-p1)+p2*(1-p2))^0.5
  zbet <- num/den
  pnorm(zbet)
}


ftuss <- function(p1,p2,r,alpha,beta)
{
  ## Compute the approximate sample size needed to have power 1-beta
  ## for detecting significance in a two-tailed level alpha test of
  ## the hypothesis that pi1=pi2, when pi1=p1, pi2=p2, and there
  ## are to be m in group 1 and rm in group 2.  The calculation is
  ## based on equations (3) and (4) of Fleiss, Tytun and Ury (1980
  ## Bcs 36, 343--346).  This is accurate to within 1% for
  ## moderately large values of del(p2-p1) (del>=0.1) and sample
  ## sizes that are not too disproportionate (0.5<=r<=2).
  ##   Daniel F. Heitjan, 30 April 1991
  zalp <- qnorm(1-alpha/2)
  zbet <- qnorm(1-beta)
  rp1 <- (r+1)
  pbar <- (p1+r*p2)/rp1
  qbar <- 1-pbar
  q1 <- 1-p1
  q2 <- 1-p2
  del <- abs(p2-p1)
  num <- (zalp*(rp1*pbar*qbar)^0.5+zbet*(r*p1*q1+p2*q2)^0.5)^2
  den <- r*del^2
  mp <- num/den
  m <- 0.25*mp*(1+(1+2*rp1/(r*mp*del))^0.5)^2
  list(n1=floor(m+1),n2=floor(m*r+1))
}
