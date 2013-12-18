require(Hmisc)
n <- 100
f <- function(na=FALSE) {
  x <- sample(c('N', 'Y'), n, TRUE)
  if(na) x[runif(100) < .1] <- NA
  x
}
set.seed(1)
d <- data.frame(x1=f(), x2=f(), x3=f(), x4=f(), x5=f(), x6=f(), x7=f(TRUE),
                age=rnorm(n, 50, 10),
                race=sample(c('Asian', 'Black/AA', 'White'), n, TRUE),
                sex=sample(c('Female', 'Male'), n, TRUE),
                treat=sample(c('A', 'B'), n, TRUE),
                region=sample(c('North America','Europe'), n, TRUE))
d <- upData(d, labels=c(x1='MI', x2='Stroke', x3='AKI', x4='Migraines',
                 x5='Pregnant', x6='Other event', x7='MD withdrawal',
                 race='Race', sex='Sex'))
dasna <- subset(d, region=='North America')
with(dasna, table(race, treat))

png('/tmp/summaryP.png', width=550, height=550)
pdf('/tmp/z.pdf')
z=summaryP(race + sex + yn(x1, x2, x3, x4, x5, x6, x7, label='Exclusions') ~
         region, groups=treat, data=d, col=c('black', 'blue'),
  gformula=val ~ freq | region * var)
print(z)   # default lattice

require(latticeExtra)
useOuterStrips(z)   # looks much better
dev.off()

summaryP(race + sex ~ region, data=d, col='green')
summaryP(race + sex ~ region, data=d, gformula=val ~ freq | region * var)
summaryP(race + sex ~ region, groups = treat, data=d)
