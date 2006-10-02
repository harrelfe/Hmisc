require('chron', character.only=TRUE)

## round.chron <- function(x, units=c("minutes", "hours", "days", "months", "years")) {
##   if(missing(units)) {
##     return(floor(unclass(x)))
##   }

##   units <- match.arg(units)

##   time.units <- c("minutes", "hours", "days")
##   date.units <- c("months", "years")
  
##   if(units %in% time.units && !inherits(x, what=c('dates', 'times'))) {
##     stop("when 'units' is 'minutes', or 'hours' 'x' must be of class 'times' or 'dates'")
##   }
  
##   if(units %in% date.units  && !inherits(x, what=c('dates'))) {
##     stop("when 'units' is 'days', 'months', or 'years' 'x' must be of class 'dates'")
##   }

##   attribs <- attributes(x)

##   switch(units,
##          minutes = x + times('0:0:30', format='h:m:s')
##          hours = x + times('0:30:0', format='h:m:s')
##          days = x + times('12:0:0', format='h:m:s')
##          months = x + dates(paste('0:0',nlevels(days)/2,sep=':'), format='y:m:d')
##          years = x + dates(paste('0:

##   time <- c(seconds(x), minutes(x), hours(x), as.integer(days(x)),
##             as.integer(months(x)), as.integer(as.character(years(x))))

##   max.time <- c(59,59,23,nlevels(days),nlevels(months
##   set <- switch(units,
##          minutes = if(time[1] >= 30) {
##            if(time[4,
##          hours = time[2]/30 >= 1,
##          days = if(time[3]/12 >= 1) { 
##            if(time[4] == nlevels(days)) {
##              if(time[5] == nlevels(months)) {
##                time[6] <- time[6] + 1
##                5
##              } else {
##                time[5] <- time[5] + 1
##                4
##              }
##            } else {
##              time[4] <- time[4] + 1
##              3
##            }
##          },
## }

floor.chron <- function(x, units=c("minutes", "hours", "days", "months", "years")) {
  if(missing(units)) {
    return(floor(unclass(x)))
  }
  
  units <- match.arg(units)

  time.units <- c("minutes", "hours", "days")
  date.units <- c("months", "years")
  
  if(units %in% time.units && !inherits(x, what=c('dates', 'times'))) {
    stop("when 'units' is 'minutes', or 'hours' 'x' must be of class 'times' or 'dates'")
  }
  
  if(units %in% date.units  && !inherits(x, what=c('dates'))) {
    stop("when 'units' is 'days', 'months', or 'years' 'x' must be of class 'dates'")
  }

  attribs <- attributes(x)

  time <- c(seconds(x), minutes(x), hours(x), as.integer(days(x)),
            as.integer(months(x)), as.integer(as.character(years(x))))

  switch(units,
         minutes = time[1] <- 0,
         hours = time[1:2] <- 0,
         days = time[1:3] <- 0,
         months = time[1:4] <- 0,
         years = time[1:5] <- 0)

  time[c(4,5)] <- ifelse(time[c(4,5)] == 0, 1, time[c(4,5)])

  args <- list(format=c(dates='d:m:y', times='s:m:h'),
               out.format=attribs$format, origin=attribs$origin)

  
  if(!inherits(x, what='dates')) {
    time[4:6] <- NA
  } else if(length(attribs$format) == 1) {
    time[1:3] <- NA
  }

  if(! all(is.na(time[4:6]))) {
    args$dates. <- paste(time[4:6], collapse=':')
  }

  if(! all(is.na(time[1:3]))) {
    args$times. <- paste(time[1:3], collapse=':')
  }

  do.call('chron', args)
}

  
ceiling.chron <- function(x, units=c("minutes", "hours", "days", "months", "years")) {
  if(missing(units)) {
    return(ceiling(unclass(x)))
  }
  
  units <- match.arg(units)

  time.units <- c("minutes", "hours", "days")
  date.units <- c("months", "years")
  
  if(units %in% time.units && !inherits(x, what=c('dates', 'times'))) {
    stop("when 'units' is 'minutes', or 'hours' 'x' must be of class 'times' or 'dates'")
  }
  
  if(units %in% date.units  && !inherits(x, what=c('dates'))) {
    stop("when 'units' is 'days', 'months', or 'years' 'x' must be of class 'dates'")
  }

  attribs <- attributes(x)
  days <- days(x)
  months <- months(x)
  years <- as.integer(as.character(years(x)))
  
  time <- c(seconds(x), minutes(x), hours(x), as.integer(days),
            as.integer(months), years)

  switch(units,
         minutes = time[1] <- NA,
         hours = time[1:2] <- NA,
         days = time[1:3] <- NA,
         months = time[1:4] <- NA,
         years = time[1:5] <- NA)

  time[1:5] <- ifelse(is.na(time[1:5]), c(59,59,23,nlevels(days),nlevels(months)), time)

  args <- list(format=c(dates='d:m:y', times='s:m:h'),
               out.format=attribs$format, origin=attribs$origin)

  
  if(!inherits(x, what='dates')) {
    time[4:6] <- NA
  } else if(length(attribs$format) == 1) {
    time[1:3] <- NA
  }

  if(! all(is.na(time[4:6]))) {
    args$dates. <- paste(time[4:6], collapse=':')
  }

  if(! all(is.na(time[1:3]))) {
    args$times. <- paste(time[1:3], collapse=':')
  }

  do.call('chron', args)
}
