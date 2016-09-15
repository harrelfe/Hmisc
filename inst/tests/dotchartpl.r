require(Hmisc)
set.seed(1)
d <- expand.grid(major=c('Alabama', 'Alaska', 'Arkansas'),
                 minor=c('East', 'West'),
                 group=c('Female', 'Male'),
                 city=0:2)
n <- nrow(d)
d$x <- (1 : nrow(d)) + runif(n)
d$num <- round(100*runif(n))
d$denom <- d$num + round(100*runif(n))
d

with(d, dotchartpl(x, major, minor, group, city, big=city==0, num=num, denom=denom))


n <- 500
set.seed(1)
d <- data.frame(
  race         = sample(c('Asian', 'Black/AA', 'White'), n, TRUE),
  sex          = sample(c('Female', 'Male'), n, TRUE),
  treat        = sample(c('A', 'B'), n, TRUE),
  smoking      = sample(c('Smoker', 'Non-smoker'), n, TRUE),
  hypertension = sample(c('Hypertensive', 'Non-Hypertensive'), n, TRUE),
  region       = sample(c('North America','Europe','South America',
                          'Europe', 'Asia', 'Central America'), n, TRUE))

d <- upData(d, labels=c(race='Race', sex='Sex'))

dm <- addMarginal(d, region)
s <- summaryP(race + sex + smoking + hypertension ~
                region + treat,  data=dm)

## add exclude1=FALSE to include female category
ggplot(s, groups='treat', exclude1=TRUE, abblen=12)
ggplot(s, groups='region')

s$region <- ifelse(s$region == 'All', 'All Regions', as.character(s$region))

with(s, 
 dotchartpl(freq / denom, major=var, minor=val, group=treat, mult=region,
            big=region == 'All Regions', num=freq, denom=denom)
)

s2 <- s[- attr(s, 'rows.to.exclude1'), ]
with(s2, 
     dotchartpl(freq / denom, major=var, minor=val, group=treat, mult=region,
                big=region == 'All Regions', num=freq, denom=denom)
)




