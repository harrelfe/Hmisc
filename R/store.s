##This has code from Bill Dunlap's "set.work" function
if(.R.) {
  store <- function(object, name=as.character(substitute(object)), 
                    where=if(under.unix || .SV4.)".Data" else "_Data")
    stop('function not available for R')
  stores <- function(...) stop('function not available for R')
} else {
  store <- function(object, name=as.character(substitute(object)), 
                    where=if(under.unix || .SV4.)".Data" else "_Data"){

  if(missing(object)) {
#    if(.R.) attach(NULL, name='.GlobalTemp', pos=1)
	temp <- if(under.unix) paste(".Data.temp",
								 unix("echo $$"), sep="") else tempfile()
	sys(paste("mkdir",temp), minimized=FALSE)
    if(.SV4.) sys(paste('mkdir ',temp,
                        if(under.unix)'/' else '\\',
                        '__Meta',sep=''))  ## 20jun02
	attach(temp, 1)
	options(.store.temp=temp, TEMPORARY=FALSE)
	l <- function() { detach(1, FALSE); 
					  sys(paste(if(under.unix)"rm -r" else "deltree /Y",.Options$.store.temp),
						  minimized=TRUE) }
	assign(".Last", l, where=1)
	return(invisible())
  }

  assign(name,object,where=where,immediate=TRUE)
  invisible()
}

  stores <- function(...) {
    nams <- as.character(sys.call())[-1]
    dotlist <- list(...)
    for(i in 1:length(nams)) assign(nams[i], dotlist[[i]],
                                    where=if(under.unix || .SV4.)".Data" else "_Data",
                                    immediate=TRUE)
    invisible()
  }
  NULL
}

storeTemp <- if(.R.) function(object,
                              name=deparse(substitute(object))) {
    pos <- match('.GlobalTemp', search())
    if(is.na(pos)) {
      attach(NULL,name='.GlobalTemp')
      pos <- match('.GlobalTemp', search())
    }
    assign(name, object, pos)
    invisible()
  } else function(object, name=deparse(substitute(object))) {
    assign(name, object, frame=0)
    invisible()
  }


