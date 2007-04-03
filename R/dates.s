## .CronSetup <- FALSE
## .NeededCronFuns <- list("month.day.year", "leap.year")
## month.length <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

## .SetUpCron <- function() {
##   if(.CronSetup) {
##     return()
##   }
##                                         #  .ImportFrom("chron", "month.day.year", "leap.year")
##                                         #  do.call(".ImportForm", c(list("chron"), .NeededCronFuns))
##   .CronSetup <- TRUE
## }

## ## round.chron <- function(x, units=c("minutes", "hours", "days", "months", "years")) {
## ##   if(missing(units)) {
## ##     return(floor(unclass(x)))
## ##   }

## ##   units <- match.arg(units)

## ##   time.units <- c("minutes", "hours", "days")
## ##   date.units <- c("months", "years")

## ##   if(units %in% time.units && !inherits(x, what=c('dates', 'times'))) {
## ##     stop("when 'units' is 'minutes', or 'hours' 'x' must be of class 'times' or 'dates'")
## ##   }

## ##   if(units %in% date.units  && !inherits(x, what=c('dates'))) {
## ##     stop("when 'units' is 'days', 'months', or 'years' 'x' must be of class 'dates'")
## ##   }

## ##   attribs <- attributes(x)

## ##   switch(units,
## ##          minutes = x + times('0:0:30', format='h:m:s')
## ##          hours = x + times('0:30:0', format='h:m:s')
## ##          days = x + times('12:0:0', format='h:m:s')
## ##          months = x + dates(paste('0:0',nlevels(days)/2,sep=':'), format='y:m:d')
## ##          years = x + dates(paste('0:

## ##   time <- c(seconds(x), minutes(x), hours(x), as.integer(days(x)),
## ##             as.integer(months(x)), as.integer(as.character(years(x))))

## ##   max.time <- c(59,59,23,nlevels(days),nlevels(months
## ##   set <- switch(units,
## ##          minutes = if(time[1] >= 30) {
## ##            if(time[4,
## ##          hours = time[2]/30 >= 1,
## ##          days = if(time[3]/12 >= 1) { 
## ##            if(time[4] == nlevels(days)) {
## ##              if(time[5] == nlevels(months)) {
## ##                time[6] <- time[6] + 1
## ##                5
## ##              } else {
## ##                time[5] <- time[5] + 1
## ##                4
## ##              }
## ##            } else {
## ##              time[4] <- time[4] + 1
## ##              3
## ##            }
## ##          },
## ## }
## .checkRoundChron <- function(x, units) {
##   given <- list(dates=FALSE, times=FALSE)

##   if(is(x, 'chron')) {
##     given[c("dates", "times")] <- TRUE
##   } else if(is(x, 'dates')) {
##     if(any(units %in% c("seconds", "minutes", "hours"))) {
##       return("x is a dates object unable to work with a unit values of 'seconds','minutes', or 'hours'")
##     }
##     given$dates <- TRUE
##   } else if(is(x, 'times')) {
##     if(any(units %in% c('months', 'years'))) {
##       return("x is a times object unable to work with a unit value of 'months', 'years'")
##     }
##     given$times <- TRUE
##   } else {
##     return("x is not a chron object")
##   }

##   return(given)
## }

## hour.minute.second <- function(x) {
##   .SetUpCron()
  
##   ## get the total number of seconds
##   X <- as.numeric(x)

##   if(inherits(x, what='dates')) {
##     X <- X - floor(X)
##   }
  
##   seconds <- 86400 * abs(X - floor(X))

##   ## get total number of whole seconds
##   sec <- floor(seconds)

##   ## get number of hours in sec
##   hh <- sec %/% 3600
##   ## place those seconds from these hours in consumed.sec
##   consumed.sec <- hh * 3600

##   ## get number of minutes in remainint seconds
##   mm <- (sec - consumed.sec) %/% 60
##   ## add those seconds from these minutes in consumed.sec
##   consumed.sec <- consumed.sec + mm * 60

##   ## subtract the number consumed seconds from seconds
##   ss <- seconds - consumed.sec

##   list(hour = hh, minute = mm, second = ss)
## }

## second.minute.hour.day.month.year <- function(x, origin.) {
##   .SetUpCron()
  
##   mdy <- month.day.year(x, origin.)

##   hms <- hour.minute.second(x)

##   list(second = hms$second, minute = hms$minute, hour = hms$hour,
##        day = mdy$day, month = mdy$month, year = mdy$year)
## }

## ## month.days <- function(x) {
## ##   non.na <- !is.na(x)
## ##   mdy <- month.day.year(x)
## ##   year <- as.numeric(mdy$year)
## ##   month <- as.numeric(mdy$month)

## ##   non.na <- !is.na(mo)
## ##   bad <- seq(along = mo)[non.na][mo[non.na] < 1 |
## ##                mo[non.na] >  12]
## ##   if (n.bad <- length(bad)) {
## ##     if (n.bad > 10) 
## ##       msg <- paste(n.bad, "months out of range set to NA")
## ##     else {
## ##       if(n.bad > 1) {
## ##         msg <- paste("month(s) out of range in positions", 
## ##                      paste(bad, collapse = ","))
## ##       } else {
## ##         msg <- paste("month out of range in position", bad)
## ##       }
## ##       msg <- paste(msg, "set to NA")
## ##     }
## ##     warning(msg)
## ##     mo[bad] <- NA
## ##     non.na[bad] <- FALSE
## ##   }


## ##   month.days <- month.length[month]
## ## }  

## floor.chron <- function(x,
##                         units=c("seconds", "minutes", "hours", "days", "months", "years")) {
##   .SetUpCron()

##   if(missing(units)) {
##     return(floor(unclass(x)))
##   }
  
##   units <- match.arg(units, several.ok=TRUE)

##   given <- .checkRoundChron(x, units)
##   if(is.character(given)) {
##     stop(given)
##   }

##   if(given$dates) {
##     ncol <- 6
##   } else {
##     ncol <- 4
##   }

##   nrow <- length(x)
##   ## save attribes
##   attribs <- attributes(x)

##   ## Get in individual components of the date
##   time <- do.call(cbind, second.minute.hour.day.month.year(x)[1:ncol])

##   index <- match(units, c("seconds", "minutes", "hours", "days", "months", "years"))

##   if(index == 1) {
##     ## floor on seconds which is a integer so floor it.
##     time[,1] <- floor(time[,1])
##   } else {
##     min.vals <- c(0,0,0,1,1)
##     length(min.vals) <- ncol - 1
    
##     rep.seq <- seq(from=1, to=index - 1)
##     min.vals <- matrix(rep(min.vals[rep.seq], each=nrow), nrow=nrow)

##     ## Create a matrix that is true for each element that should be replaced
##     ## with its minimum value
##     not.rep <- matrix(logical(length(time)), nrow=nrow)
##     not.rep[,rep.seq] <- time[,rep.seq] > min.vals

##     ## if there is no change return orginal value
##     if(! any(not.rep)) {
##       return(x)
##     }
    
##     ## replace time values that are less significant then the index
##     ## with the minimum values.
##     time[not.rep] <- min.vals[not.rep]

##   }

##   ## Find the interger representation of the date
##   if(given$dates) {
##     result <- julian(x=time[,5], d=time[,4], y=time[,6], origin.=attribs$origin)
##   } else {
##     result <- 0
##   }

##   ## Find the decimal representation of the time and add it the the result
##   if(given$times) {
##     result <- result + ((3600 * time[,3] + 60 * time[,2] + time[,1]) / (24 * 3600))
##   }

##   ## set the existing attributes on the result
##   attributes(result) <- attribs

##   return(result)
## }

## ceiling.chron <- function(x,
##                           units=c("seconds", "minutes", "hours", "days", "months", "years"),
##                           inclusive=TRUE) {
##   .SetUpCron()
  
##   if(missing(units)) {
##     return(ceiling(unclass(x)))
##   }
  
##   units <- match.arg(arg=units, several.ok=TRUE)

##   given <- .checkRoundChron(x=x, units=units)

##   if(is.character(given)) {
##     stop(given)
##   }

##   if(given$dates) {
##     ncol <- 6
##   } else {
##     ncol <- 4
##   }

##   nrow <- length(x)
##   ## save attribes
##   attribs <- attributes(x)

##   ## Get in individual components of the date
##   time <- do.call(cbind, second.minute.hour.day.month.year(x)[1:ncol])

##   index <- match(units, c("seconds", "minutes", "hours", "days", "months", "years"))

##   if(index == 1) {
##     ## ceiling on seconds which is a integer so ceiling it
##     time[,1] <- ceiling(time[,1])
##   } else {
##     ## set the min and max vales for each of the date components
##     max.vals <- c(59, 59, 23, NA, 12)
##     length(max.vals) <- ncol - 1
##     max.vals <- matrix(rep(max.vals, each=nrow), nrow=nrow)

##     if(given$dates) {
##       ## if this is a date set the max days according to leap year.
##       max.vals[,4] <- ifelse(time[,5] == 2 & leap.year(time[,6]), 29, month.length[time[,5]])
##     }

##     min.vals <- c(0,0,0,1,1)
##     length(min.vals) <- ncol - 1
##     min.vals <- matrix(rep(min.vals, each=nrow), nrow=nrow)
    
##     rep.seq <- seq(from=1, to=index - 1)

##     if(inclusive) {
##       replacement.vals <- min.vals[,rep.seq]
##     } else {
##       replacement.vals <- max.vals[,rep.seq]
##     }
    
##     ## if all of the less sigificate values are equal to
##     ## the replacement values then return x unchanged
##     not.rep <- matrix(FALSE, ncol=ncol, nrow=nrow)
##     not.rep[,rep.seq] <- time[,rep.seq] != replacement.vals
##     if(! any(not.rep[,rep.seq])) {
##       return(x)
##     }
    
##     time[not.rep] <- replacement.vals[not.rep]

##     if(inclusive) {
##       rep.indx <- sapply(split(not.rep[,rep.seq], 1:nrow), any)
##       time[rep.indx, index] <- time[rep.indx, index] + 1

##       test <- rep.int(TRUE, times=nrow)
##       for(i in seq(from=index, to=ncol - 1)) {
##         ## find which rows have larger then max vals
##         test[test] <- time[test,i] > max.vals[test,i]

##         if(any(test)) {
##           ## For all rows where test is true
##           ## increment the value of the next col and
##           ## set the cols value to the minium value.
##           time[test, i + 1] <- time[test, i + 1] + 1
##           time[test, i] <- min.vals[test, i]
##         } else {
##           break
##         }        
##       }

##       ## remove the test var
##       rm(test)
      
##       ## remove the rep.indx var
##       rm(rep.indx)
##     }
##   }


##   ## Find the interger representation of the date
##   if(given$dates) {
##     result <- julian(x=time[,5], d=time[,4], y=time[,6], origin.=attribs$origin)
##   } else {
##     result <- 0
##   }

##   ## Find the decimal representation of the time and add it the the result
##   if(given$times) {
##     result <- result + ((3600 * time[,3] + 60 * time[,2] + time[,1]) / (24 * 3600))
##   }

##   ## set the existing attributes on the result
##   attributes(result) <- attribs

##   return(result)
## }

## is.leap.year <- function(time) {
##   year <- as.POSIXlt(time)$year

##   ((year %% 4 == 0)
##    && (year %% 100 != 0
##        || ((year / 100) %% 4) == (-(1900 / 100) %% 4))) 
## }  

yearDays <- function(time) {
  time <- as.POSIXlt(time)

  time$mon[] <- time$mday[] <- time$sec[] <- time$min <- time$hour <- 0
  time$year <- time$year + 1

  return(as.POSIXlt(as.POSIXct(time))$yday)
}

monthDays <- function(time) {
  time <- as.POSIXlt(time)
  time$mday[] <- time$sec[] <- time$min <- time$hour <- 0
  time$mon <- time$mon + 1

  return(as.POSIXlt(as.POSIXct(time))$mday)
}

round.POSIXt <- function(x, digits=c("secs", "mins", "hours", "days", "months", "years"))
  {
    ## this gets the default from the generic, as that has two args.
    if(is.numeric(digits) && digits == 0.0) digits <-"secs"
    units <- match.arg(digits)

    month.length <- monthDays(x)
    x <- as.POSIXct(x)
    x <- x + switch(units,
                    "secs" = 0.5, "mins" = 30, "hours"= 1800, "days" = 43200,
                    "months" = 43200 * monthDays(x),
                    "years" = 43200 * yearDays(x))

    trunc.POSIXt(x, units = units)
  }

trunc.POSIXt <- function(x, units=c("secs", "mins", "hours", "days", "months", "years")) {
    units <- match.arg(units)

    x <- as.POSIXlt(x)

    isdst <- x$isdst
    if(length(x$sec) > 0)
      switch(units,
             "secs" = {x$sec <- trunc(x$sec)},
             "mins" = {x$sec <- 0},
             "hours"= {x$sec <- 0; x$min <- 0},
             "days" = {x$sec <- 0; x$min <- 0; x$hour <- 0; isdst <- x$isdst <- -1},
             "months" = {
               x$sec <- 0
               x$min <- 0
               x$hour <- 0
               x$mday <- 1
               isdst <- x$isdst <- -1
             },
             "years" = {
               x$sec <- 0
               x$min <- 0
               x$hour <- 0
               x$mday <- 1
               x$mon <- 0
               isdst <- x$isdst <- -1
             }
             )

    x <- as.POSIXlt(as.POSIXct(x))
    if(isdst == -1) {
      x$isdst <- -1
    }
    return(x)
  }

ceil <- function(x, units) {
  UseMethod('ceil', x)
}

ceil.default <- function(x, units) {
  ceiling(x)
}

ceil.POSIXt <- function(x, units=c("secs", "mins", "hours", "days", "months", "years")) {
  units <- match.arg(units)

  x <- as.POSIXlt(x)

  isdst <- x$isdst
  if(length(x$sec) > 0 && x != trunc(x, units)) {
    switch(units,
           "secs" = {
             x$sec <- ceiling(x$sec)
           },
           "mins" = {
             x$sec <- 0
             x$min <- x$min + 1
           },
           "hours"= {x$sec <- 0; x$min <- 0; x$hour <- x$hour + 1},
           "days" = {
             x$sec <- 0
             x$min <- 0
             x$hour <- 0
             x$mday <- x$mday + 1
             isdst <- x$isdst <- -1
           },
           "months" = {
             x$sec <- 0
             x$min <- 0
             x$hour <- 0
             x$mday <- 1
             x$mon <- x$mon + 1
             isdst <- x$isdst <- -1
           },
           "years" = {
             x$sec <- 0
             x$min <- 0
             x$hour <- 0
             x$mday <- 1
             x$mon <- 0
             x$year <- x$year + 1
             isdst <- x$isdst <- -1
           }
           )

    x <- as.POSIXlt(as.POSIXct(x))
    if(isdst == -1) {
      x$isdst <- -1
    }
  }    
  return(x)  
}
