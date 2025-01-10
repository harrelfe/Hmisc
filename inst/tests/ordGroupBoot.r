# For a variety of sample sizes and distribution of number of ties solve
# for the value of aprob that makes ordGroupBoot(x, aprob=aprob) agree the
# most with ordGroupBoot(x, B=1000)
# Note that output is expected to include an Error

require(rms)
set.seed(1)
d <- NULL
for(i in 1 : 200) {   # use 2000 for production run
  if(i %% 100 == 0) cat(i, '')
  n <- sample(c(rep(20 : 300, 2), 21 : 3000, 1), 1)
  x <- runif(n)
  r <- sample(1 : 3, 1)
  x <- round(runif(n), r)
  ap <- sample(c(.9995, .99975, .9999, .99995), 1)
  mcandidates <- 7 : min(16, floor(n * 0.4))
  ma <- try(ordGroupBoot(x, aprob=ap, m = mcandidates, what='m', pr=FALSE))
  mb <- try(ordGroupBoot(x, B=1000,   m = mcandidates, what='m', pr=FALSE))
  if(inherits(ma, 'try-error')) ma <- NA
  if(inherits(mb, 'try-error')) mb <- NA
  w <- data.frame(n, r, ap, ma, mb, ad=abs(ma - mb))
  d <- rbind(d, w)
}
with(d, table(ad))
mn <- function(x) mean(x, na.rm=TRUE)
with(d, tapply(ad, r, mn))
with(d, tapply(ad, ap, mn))

dd <- datadist(d); options(datadist='dd')
f <- lrm(ad ~ n + r + log(ap), data=d)
f
ggplot(Predict(f))

# Need ma to be >= mb
with(d, tapply(ma >= mb, ap, mn))   # best aprob=0.9999; overshoots by 1.4 on avg.


