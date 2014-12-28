require(Hmisc)
set.seed(1)
p <- data.frame(x=1:10, y=1:10 + runif(10))
d <- data.frame(x=rnorm(1000, 5, 1))
ggplot(p, aes(x=x, y=y)) + geom_line() + histSpikeg(y ~ x, p, d, ylim=c(0,15))

p <- expand.grid(sex=c('male','female'), region=c('a','b','c'), x=1:10)
p$y <- with(p, x + runif(60) + 2*runif(60)*(sex=='female') + 3*(region=='c'))
g <- ggplot(p, aes(x=x, y=y, color=sex)) + geom_line() + facet_wrap(~ region)
g
d <- expand.grid(sex=c('male', 'female'), region=c('a','b','c'), reps=1:300)
d$x <- rnorm(nrow(d), 5, 2)
d$x[d$sex == 'male'] <- rnorm(sum(d$sex == 'male'), 7, .4)
d$x[d$region == 'b'] <- rnorm(sum(d$region == 'b'), 2, 1)
g + histSpikeg(y ~ x + sex + region, p, d, ylim=c(0, 15), frac=.04)

