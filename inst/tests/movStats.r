require(Hmisc)
require(ggplot2)

set.seed(1)
n <- 1000000
x <- runif(n)
y <- x ^ 2 + runif(n)
system.time(f <- lowess(x, y))

system.time(m <- movStats(y ~ x, melt=TRUE))
ggplot(m, aes(x=x, y=y, color=Statistic)) + geom_line()

