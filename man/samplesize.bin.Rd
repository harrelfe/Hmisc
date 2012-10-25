\name{samplesize.bin}
\alias{samplesize.bin}
\title{
  Sample Size for 2-sample Binomial
}
\description{
  Computes sample size(s) for 2-sample binomial problem given vector or
  scalar probabilities in the two groups.
}
\usage{
samplesize.bin(alpha, beta, pit, pic, rho=0.5)
}
\arguments{
  \item{alpha}{
    scalar ONE-SIDED test size, or two-sided size/2
  }
  \item{beta}{
    scalar or vector of powers
  }
  \item{pit}{
    hypothesized treatment probability of success
  }
  \item{pic}{
    hypothesized control probability of success
  }
  \item{rho}{
    proportion of the sample devoted to treated group (\eqn{0 <\code{rho} < 1})
  }
}
\value{
  TOTAL sample size(s)
}
\section{AUTHOR}{
  Rick Chappell\cr
  Dept. of Statistics and Human Oncology\cr
  University of Wisconsin at Madison\cr
  \email{chappell@stat.wisc.edu}
}
\examples{
alpha <- .05
beta <- c(.70,.80,.90,.95)


# N1 is a matrix of total sample sizes whose
# rows vary by hypothesized treatment success probability and
# columns vary by power
# See Meinert's book for formulae.


N1 <- samplesize.bin(alpha, beta, pit=.55, pic=.5)
N1 <- rbind(N1, samplesize.bin(alpha, beta, pit=.60, pic=.5))
N1 <- rbind(N1, samplesize.bin(alpha, beta, pit=.65, pic=.5))
N1 <- rbind(N1, samplesize.bin(alpha, beta, pit=.70, pic=.5))
attr(N1,"dimnames") <- NULL


#Accounting for 5\% noncompliance in the treated group
inflation <- (1/.95)**2
print(round(N1*inflation+.5,0))
}
\keyword{htest}
\keyword{category}
\concept{study design}
\concept{power}
