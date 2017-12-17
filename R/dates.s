yearDays <- function(time) {
  time <- as.POSIXlt(time)

  time$mon[] <- time$mday[] <- time$sec[] <- time$min <- time$hour <- 0
  time$year <- time$year + 1

  return(as.POSIXlt(as.POSIXct(time))$yday + 1)
}

monthDays <- function(time) {
  time <- as.POSIXlt(time)
  time$mday[] <- time$sec[] <- time$min <- time$hour <- 0
  time$mon <- time$mon + 1

  return(as.POSIXlt(as.POSIXct(time))$mday)
}

roundPOSIXt <- function(x, digits=c("secs", "mins", "hours", "days", "months", "years"))
  {
    ## this gets the default from the generic, as that has two args.
    if(is.numeric(digits) && digits == 0.0) digits <-"secs"
    units <- match.arg(digits)

    month.length <- monthDays(x)
    x <- as.POSIXlt(x)

    if(length(x$sec) > 0)
      switch(units,
             "secs"   = {x$sec <- x$sec + 0.5},
             "mins"   = {x$sec <- x$sec + 30},
             "hours"  = {x$sec <- 0; x$min <- x$min + 30},
             "days"   = {x$sec <- 0; x$min <- 0; x$hour <- x$hour + 12
                         isdst <- x$isdst <- -1},
             "months" = {x$sec <- 0; x$min <- 0; x$hour <- 0;
                         x$mday <- x$mday + trunc(monthDays(x)/2);
                         isdst <- x$isdst <- -1},
             "years"  = {x$sec <- 0; x$min <- 0; x$hour <- 0;
                         x$mday <- 0; x$mon <- x$mon + 6;
                         isdst <- x$isdst <- -1}
             )

    return(truncPOSIXt(as.POSIXct(x), units=units))
  }

truncPOSIXt <- function(x, units=c("secs", "mins", "hours", "days", "months", "years"), ...) {
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

ceil <- function(x, units, ...) {
  UseMethod('ceil', x)
}

ceil.default <- function(x, units, ...) {
  ceiling(x)
}

ceil.POSIXt <- function(x, units=c("secs", "mins", "hours", "days", "months", "years"), ...) {
  units <- match.arg(units)

  x <- as.POSIXlt(x)

  isdst <- x$isdst
  if(length(x$sec) > 0 && x != truncPOSIXt(x, units=units)) {
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
