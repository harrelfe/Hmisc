## See Paul T von Hippel, The American Statistician 58:160-164, 2004

mvector <- c(0,0)
msigma <- matrix(c(1,0.5,0.5,1), nrow=2)
library(mvtnorm)
library(Hmisc)

# XZ <- rmvnorm(1000, mvector, msigma)
mvrnorm <- function(n, p = 1, u = rep(0, p), S = diag(p)) {
  Z <- matrix(rnorm(n * p), p, n)
  t(u + t(chol(S)) %*% Z)
}

XZ <- mvrnorm(1000, 2, mvector, msigma)
              
U <- rnorm(1000)
Y <- XZ[,1]+XZ[,2]+U
summary(lm(Y ~ XZ))
X <- XZ[,1]
Z <- XZ[,2]
Z.ni <- Z

type <- c('random','X<0','Y<0','Z<0')[3]
i <- switch(type,
            random= runif(1000) < .5,
            'X<0' = X<0,
            'Y<0' = Y<0,
            'Z<0' = Z<0)

Zna <- Z
Zna[i] <- NA
summary(lm(Y ~ X + Zna))

#w <- aregImpute(~monotone(Y)+monotone(X)+monotone(Zna))
#w <- aregImpute(~I(Y)+I(X)+I(Zna),fweight=.75)
w <- aregImpute(~monotone(Y)+monotone(X)+monotone(Zna), n.impute=5,
                type='regression') 

plot(w)
ecdf(Zna, add=T, col='red')
ecdf(Z, add=T, col='green')
# plot(w$imputed$Zna, Z[is.na(Zna)])  # use if n.impute=1
# abline(a=0,b=1,lty=2)
# lm(Z[is.na(Zna)] ~ w$imputed$Zna)

coef(fit.mult.impute(Y~X+Zna, lm, w, data=data.frame(X,Zna,Y),pr=F))


