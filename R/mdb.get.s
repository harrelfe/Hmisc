mdb.get <- function(file, tables=NULL, lowernames=FALSE, allow=NULL,
                    dateformat='%m/%d/%y', mdbexportArgs='-b strip', ...)
{
  rettab <- length(tables) && is.logical(tables)
  if(rettab) tables <- NULL
  if(!length(tables))
    tables <- system(paste('mdb-tables -1', file), intern=TRUE)
  if(rettab) return(tables)

  f <- tempfile()
  D <- vector('list', length(tables))
  names(D) <- tables

  for(tab in tables) {
    s <- system(paste('mdb-schema -T', shQuote(tab), file), intern=TRUE)
    start <- grep('^ \\($', s) + 1
    end   <- grep('^\\);$', s) - 1
    s <- s[start:end]
    s <- strsplit(s, '\t')
    vnames <- sapply(s, function(x) {
      bracketed = x[2]
      if(substr(bracketed, 1, 1) == '[')
        substr(bracketed, 2, nchar(bracketed) - 1) else bracketed
    })
    vnames <- makeNames(vnames, unique=TRUE, allow=allow)
    if(lowernames) vnames <- casefold(vnames)
    types  <- sapply(s, function(x)x[length(x)])
    datetime <- vnames[grep('DateTime', s)]
    system2(command = 'mdb-export',
            args = paste(mdbexportArgs, file, shQuote(tab)), stdout = f)
    d <- csv.get(f, datetimevars=datetime,
                 lowernames=lowernames, allow=allow,
                 dateformat=dateformat, ...)
    if(length(tables) == 1) return(d)
    else D[[tab]] <- d
  }
  D
}
