## Test from Matt Shotwell - rounding issue in describe.vector when values
## have very different magnitudes

require(Hmisc)
set.seed(42)
x <- c(runif(1000), runif(2)*1e7)
d <- describe(x)
d

