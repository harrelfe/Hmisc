require(Hmisc)
require(ggplot2)

set.seed(1)
n <- 1000000
x <- runif(n)
y <- x ^ 2 + runif(n)
system.time(f <- lowess(x, y))   # 1.3s
plot(f, type='l')

system.time(m <- movStats(y ~ x, melt=TRUE))   # 0.4s
ggplot(m, aes(x=x, y=y, color=Statistic)) + geom_line()

