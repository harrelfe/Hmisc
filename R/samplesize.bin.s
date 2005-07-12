## Rick Chappell <> Asst. Professor, Depts. of Statistics and Human Oncology
## <> University of Wisconsin at Madison <> chappell@stat.wisc.edu
## (608) 263-5572 / 262-2733 <> take logs

samplesize.bin <- function(alpha, beta, pit, pic, rho=.5)
{

  ## alpha is the scalar ONE-SIDED test size, or two-sided size/2
  ## beta is a scalar or vector of powers
  ## pit is the hypothesized treatment probability of success
  ## pic is the hypothesized control probability of success
  ## returns required TOTAL sample size, using arcsin transformation
  ## rho is the proportion of the sample devoted to treated group (0 <rho < 1) 

  as <- function(x)
  {
    asin(sqrt(x))
  }
  
  invas <- function(x) {
    (sin(x))**2
  }

  Zalpha <- qnorm(1-alpha)
  Zbeta  <- qnorm(beta)
  n <- Zalpha + Zbeta
  n <- n/(as(pit) - as(pic))
  n <- (n**2)/(4*rho*(1-rho))
  round(n+.5,0)
}
