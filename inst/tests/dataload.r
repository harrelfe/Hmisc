# Compare dataload with Stat/Transfer, the latter producing binary S+
# data frames and the former binary R data frames

library(Hmisc)
setwd('/tmp')
ds <- list.files('~/projects/consulting/gsk/REDUCE/oct03/data/sass')

for(i in 1:1) {
  sys(paste('dataload fh10sep.xpt z.rda',ds[i]))
  load('z.rda')
  s <-
    read.S(paste('~/projects/consulting/gsk/REDUCE/oct03/data/sass',
                 ds[i]))
  sys(paste('dataload fh10sep.xpt z.rda',ds[i]))
  load('z.rda')
  s <-
    read.S(paste('~/projects/consulting/gsk/REDUCE/oct03/data/sass',
                 ds[i],sep='/'))
}

  
  
  
