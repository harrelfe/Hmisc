require(Hmisc)

pm <- function(x, method=c('uniroot', 'optimize', 'bruteforce', 'cpp', 'fortran')) {
  method <- match.arg(method)
  if(method == 'fortran') return(pMedian(x))
  if(method == 'cpp') return(hlqest(x))
  if(method == 'bruteforce') { # good for N < 1000 but still slower than otheres
    w <- outer(x, x, '+')
    return(median(w[lower.tri(w, diag=TRUE)]) / 2)
  }
  r <- range(x)
  # Compute [0.1] normalized ranks just for numeric reasons
  nrank <- function(x) (rank(x) - 1) / (length(x) - 1)
  if(method == 'uniroot') {
    obj <- function(mu) sum(nrank(abs(x - mu)) * sign(x - mu))
    xs <- seq(r[1], r[2], length=100)
    o  <- sapply(xs, obj)
    zero <- approx(o, xs, xout=0)$y
    R <- uniroot(obj, r, tol=1e-6)
    if(abs(obj(zero)) < abs(R$f.root)) return(zero)
    R$root
  } else {
    obj <- function(mu) sum(nrank(abs(x - mu)) * sign(x - mu)) ^ 2
    optimize(obj, r, tol=1e-7)$minimum
    # optim(r[2], obj, method=if(FALSE)'L-BFGS-B' else 'Brent', lower=r[1], upper=r[3])$par
  }
}

# Compile C++ code from DescTools package
hl <- readLines('https://raw.githubusercontent.com/AndriSignorell/DescTools/refs/heads/master/src/hlqest.cpp')
Rcpp::sourceCpp(code=paste(hl, collapse='\n'))
# Code is also stored in ~/rr/pseudomedian/hlqest.cpp

set.seed(1)
w <- NULL
for(n in seq(5, 1000, by=5)) {
  x <- round(runif(n) * 100)
  w <- rbind(w, data.frame(n=n,
                           brute=pm(x, 'bruteforce'), cpp=pm(x, 'cpp'), fortran=pm(x, 'fortran'),
                           root=pm(x, 'uniroot'), mean=mean(x), median=median(x)))
}
for(i in 2: 4) for(j in (i + 1) : 5) cat(i, j, max(abs(w[,i] - w[,j])), '\n')
with(w, plot(n, root - cpp))
table(w$cpp >= pmin(w$mean, w$median) & w$cpp <= pmax(w$mean, w$median))
w[which.max(abs(w$cpp - w$root)),]
set.seed(1)
x <- round(runif(5) * 100)
x
x  <- c(20, 27, 37, 57, 91)
xcpp  <- pm(x, 'cpp')
xroot <- pm(x, 'uniroot')
pm(x, 'optimize')
c(xcpp, xroot)
xs <- seq(20, 91, by=0.1)
obj <- sapply(xs, function(z) sum(rank(abs(x - z)) * sign(x - z)))
approx(obj, xs, xout=0)
plot(xs, obj)
abline(v=xcpp,  col='red')
abline(v=xroot, col='blue')
abline(h=0, col=gray(0.5))

g <- function(method, n=10000) {
  for(i in 1 : 1000) {
    x <- runif(n)
    pm(x, method)
  }
}

system.time(g('bruteforce', 200))
system.time(g('uniroot', 1000))
system.time(g('cpp'))
system.time(g('fortran'))

x <- runif(5e6)
system.time(a <- pm(x, 'cpp'))
system.time(b <- pm(x, 'fortran'))
a; b

set.seed(1)
N <- integer(1000); dif <- numeric(1000)
for(i in 1 : 1000) {
  n <- sample(1 : 75000, 1)
  x <- round(runif(n) * 100)
  N[i] <- n
  dif[i] <- abs(pm(x, 'cpp') - pm(x, 'fortran'))
}
hist(dif, nclass=50)
plot(N, dif)


# Original Fortran function fails at N=46507
# The square root of the largest integer in R is (2^31 - 1) is 46341
# So the problem is in the computation of N*(N+1)/2 pairs in hlqest
# C++ function uses long integers; changed Fortran to use INTEGER*8 likewise
