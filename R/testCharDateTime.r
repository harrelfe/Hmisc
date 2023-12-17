##' Test Character Variables for Dates and Times
##'
##' For a vector `x`, if it is already a date-time, date, or time variable, the type is returned if `convert=FALSE`, or a list with that type, the original vector, and `numna=0` is returned.  Otherwise if `x` is not a character vector, a type of `notcharacter` is returned, or a list that includes the original `x` and `type='notcharacter'`.  When `x` is character, the main logic is applied.  The default logic (when `m=0`) is to consider `x` a date-time variable when its format is YYYY-MM-DD HH:MM:SS (:SS is optional) in more than 1/2 of the non-missing observations.  It is considered to be a date if its format is YYYY-MM-DD or MM/DD/YYYY or DD-MMM-YYYY in more than 1/2 of the non-missing observations (MMM=3-letter month).  A time variable has the format HH:MM:SS or HH:MM.  Blank values of `x` (after trimming) are set to `NA` before proceeding.
##' @title testCharDateTime
##' @param x input vector of any type, but interesting cases are for character `x`
##' @param p minimum proportion of non-missing non-blank values of `x` for which the format is one of the formats described before considering `x` to be of that type
##' @param m if greater than 0, a test is applied: the number of distinct illegal values of `x` (values containing a letter or underscore) must not exceed `m`, or type `character` will be returned.  `p` is set to `1.0` when `m` > 0.
##' @param convert set to `TRUE` to convert the variable under the dominant format.  If all values are `NA`, `type` will be set to `'character'`.
##' @param existing set to `TRUE` to return a character string with the current type of variable without examining pattern matches
##' @return if `convert=FALSE`, a single character string with the type of `x`: `"character", "datetime", "date", "time"`.  If `convert=TRUE`, a list with components named `type`, `x` (converted to `POSIXct`, `Date`, or `chron` times format), and `numna`, the number of originally non-`NA` values of `x` that could not be converted to the predominant format.  If there were any non-covertible dates/times,
##' the returned vector is given an additional class `special.miss` and an
##' attribute `special.miss` which is a list with original character values
##' (`codes`) and observation numbers (`obs`).  These are summarized by
##' `describe()`.
##' @md
##' @author Frank Harrell
##' @examples
##' for(conv in c(FALSE, TRUE)) {
##'   print(testCharDateTime(c('2023-03-11', '2023-04-11', 'a', 'b', 'c'), convert=conv))
##'   print(testCharDateTime(c('2023-03-11', '2023-04-11', 'a', 'b'), convert=conv))
##'   print(testCharDateTime(c('2023-03-11 11:12:13', '2023-04-11 11:13:14', 'a', 'b'), convert=conv))
##'   print(testCharDateTime(c('2023-03-11 11:12', '2023-04-11 11:13', 'a', 'b'), convert=conv))
##'   print(testCharDateTime(c('3/11/2023', '4/11/2023', 'a', 'b'), convert=conv))
##' }
##' x <- c(paste0('2023-03-0', 1:9), 'a', 'a', 'a', 'b')
##' y <- testCharDateTime(x, convert=TRUE)$x
##' describe(y)  # note counts of special missing values a, b

testCharDateTime <- function(x, p=0.5, m=0, convert=FALSE, existing=FALSE) {
  ret <- function(type, x, numna=0)
    if(convert) list(type=type, x=x, numna=numna) else type

  cl <- class(x)
  if(any(cl %in% c('POSIXt', 'POSIXct', 'chron'))) return(ret('datetime', x))
  if(any(cl %in% c('Date', 'dates')))              return(ret('date',     x))
  if(any(cl == 'times'))                           return(ret('time',     x))
  if(! is.character(x))                            return(ret('notcharacter', x))

  if(existing) return('character')
  
  y                <- x
  y[trimws(y) == ''] <- NA
  x                <- x[! is.na(x)]
  if(! length(x)) return(ret('character', y, numna=length(y)))
  if(m > 0) {
    p      <- 1.0
    ischar <- grep('[a-z,A-Z,_]', x)
    uchar  <- unique(x[ischar])
    lu     <- length(uchar)
    if(lu) {
      if(lu > m) return(ret('character', y)) # more than m unique char values
      x <- x[- ischar]                       # values with no alpha characters
      if(! length(x)) return(ret('character', y))
    }
  }
  # If p=1.0 all non-NA non-blank values must be legal date/time
  # Minimum length for those is 5 characters, max is 19
  # Save some time for this case
  if(p == 1.0) {
    len <- range(nchar(x))
    if(len[1] < 5 | len[2] > 19) return(ret('character', y))
    }
  
  rex <- c(
    '^[0-9]{4}-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]$',
    '^[0-9]{4}-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]$',
    '^[0-9]{4}-[0-1][0-9]-[0-3][0-9]$',
    '^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$',
    '^[0-3][0-9]-(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)-[0-9]{4}$',
    '^[0-2][0-9]:[0-5][0-9]:[0-5][0-9]$',
    '^[0-2][0-9]:[0-5][0-9]$' )
  types <- c('datetime', 'datetime', 'date', 'date', 'date', 'time', 'time')
  nx <- length(x)
  for(i in 1 : 6) {
    ty <- types[i]
    rx <- rex[i]
    ngood <- sum(grepl(rx, x, ignore.case=TRUE))
    if(ngood / nx >= p) {
      if(! convert) return(ty)
      nna  <- ! is.na(y)
      good <- grepl(rx, y, ignore.case=TRUE)
      bad  <- nna & ! good
      j    <- nna & good
      z    <- rep(NA, length(y))
      fm <- c('', '', '%Y-%m-%d', '%m/%d/%Y', '%d-%b-%Y', '')[i]
      if(i > 4) if(! requireNamespace('chron', quietly=TRUE))
                  stop('chron package is required for pure time variables')
      sec <- if(i %in% c(2, 7)) ':00' else ''
      zj <- switch(ty,
                   datetime = as.POSIXct(y[j]),,
                   date     = as.Date(y[j], format=fm),
                   time     = chron::chron(times.=paste0(y[j], sec))   )
      z[j]     <- zj
      class(z) <- class(zj)
      lab      <- label(y)
      if(lab != '') label(z) <- lab
      un                     <- units(y)
      if(un  != '') units(z) <- un
      if(any(bad)) {
        class(z) <- c('special.miss', class(z))
        attr(z, 'special.miss') <- list(codes=y[bad], obs=which(bad))
        }
      return(list(type=ty, x=z, numna=nx - ngood))
    }
  }
  ret('character', y)
}
