require(Hmisc)
set.seed(135)
maj <- factor(c(rep('North',13),rep('South',13)))
g <- paste('Category',rep(letters[1:13],2))
n <- sample(1:15000, 26, replace=TRUE)
y1 <- runif(26)
y2 <- pmax(0, y1 - runif(26, 0, .1))
png('/tmp/summaryD.png', width=550, height=800)
spar(mfrow=c(3,2))
f <- function(x) sprintf('%4.2f', x)
summaryD(y1 ~ maj + g, xlab='Mean', auxtitle='', fmtvals=f)
summaryD(y1 ~ maj + g, groupsummary=FALSE)
summaryD(y1 ~ g, fmtvals=f, auxtitle='')
Y <- cbind(y1, y2)
summaryD(Y  ~ maj + g, fun=function(y) y[1,], pch=c(1,17))
rlegend(.1, 26, c('y1','y2'), pch=c(1,17))

summaryD(y1 ~ maj, fun=function(y) c(mean(y), n=length(y)),
         auxvar='n')
dev.off()

png('/tmp/summaryD2.png', width=300, height=100)
# Or: pdf('/tmp/z.pdf', width=3.5, height=1.25)
spar()
summaryD(y1 ~ maj, fmtvals=function(x) round(x,4),
         xlab=labelPlotmath('Velocity', 'm/s'))
dev.off()

