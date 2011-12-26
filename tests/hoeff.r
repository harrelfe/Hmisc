require(Hmisc)
.Fortran('jrank', as.double(1:5), as.double(1:5), 5L,
         double(5), double(5), double(5))
hoeffd(1:6, c(1,3,2,4,5,6))
y <- 1:20; y[3] <- 17; y[17] <- 3
hoeffd(1:20, y)$D
set.seed(5)
x <- runif(800); y <- runif(800)
hoeffd(x,y)$D

for(n in c(50,100,200,400,1000)) {
  set.seed(1)
  x <- seq(-10,10,length=n)
  y <- x*sign(runif(n,-1,1))
  h <- hoeffd(x,y)
  print(c(h$D[1,2], h$aad[1,2], h$maxad[1,2]))
}
#[1] 0.06812286   in old version (real*4 in places)
#[1] 0.04667929
#[1] 0.05657654
#[1] 0.07048487
#[1] 0.06323746
