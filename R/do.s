do <- function(condition, expressions, device=NULL, file, append=FALSE,
               multiplot=FALSE, ...) {

if(!condition) return(invisible())

# The following function is courtesy of Bill Dunlap, StatSci

strip.comments <- function(expr) {
  if (mode(expr) == "comment.expression") {
	not.comment <- sapply(expr, function(ei)mode(ei)!="comment")
	if (sum(not.comment)!=1)
	  stop("unexpected result: no non-comment in expression")
	  else {
		Recall(expr[not.comment][[1]])
	  }
  } else expr
}


condition <- as.character(substitute(condition))
scondition <- if(under.unix) condition else
  substring(sedit(condition, '.', ''), 1,8)
pcondition <- if(multiplot) substring(scondition,1,7) else scondition

do.file <- if(missing(file)) {
  if(length(ds <- .Options$do.file)==0) '' else ds
} else file

do.prefix <- .Options$do.prefix

if(do.file!='') {
  if(do.file=='condition') 
	sink(sink.file <- paste(if(length(do.prefix))
          paste(do.prefix,if(under.unix)'.' else '/',sep=''), 
          paste(scondition, 'lst',sep='.'), 
            sep=''), append=append) else
    sink(sink.file <- paste(do.file, '.lst',sep=''), append=append)
}

if(missing(device)) device <- .Options$do.device

if(length(device)) {
  suffix <- if(device %in% c('postscript','ps','ps.slide')) 'ps' else 
    if(device %in% c('win.slide','win.printer')) 'wmf' else 'gr'
  file   <- paste(if(length(do.prefix))
                    paste(do.prefix,if(under.unix) '.' else '/',sep=''),
                if(device!='ps.slide' && device!='win.slide')
                  paste(pcondition, suffix, sep='.') else
                   pcondition, sep='')
  if(multiplot) {
    if(under.unix) stop('multiplot=T not meaningful under UNIX')
    if(!(device %in% c('win.slide','win.printer')))
      stop('multiplot only meaningful for device=win.slide,win.printer')
    file <- paste(file,'#',sep='')
  }
  get(device)(file, ...)
}

do.echo <- .Options$do.echo
if(length(do.echo)==0) do.echo <- TRUE

do.comments <- .Options$do.comments
if(length(do.comments)==0) do.comments <- FALSE

invis.fctns <- c('plot','lines','points','abline','text','mtext','title',
                 'impute', 'survplot')
## generic functions whose body ends in UseMethod but are invisible
## this list should grow

for(ex in substitute(expressions)) {

  lv <- eval(ex, local=1)
  exs <- strip.comments(ex)
  m <- mode(exs)
  if(m == 'name' || (m=='call' && (length(exs$pl)==0 || 
	(is.logical(exs$pl) && !exs$pl)))) {
	## some functions called to plot (pl=T) - don't auto print results
	inv <- if(m != 'call') FALSE else  { # see if expression is call to function
									 # with body ending in invisible()
	  ex1 <- as.character(exs[1])
	  inv <- if(any(ex1==invis.fctns)) TRUE else
	  if(exists(ex1, mode='function')) {
		f <- get(ex1, mode='function')
		f <- f[[length(f)]]
		f1 <- as.character(f)[1]
		if(f1=='invisible' || f1=='.Cur.pic') TRUE else {
		  m <- mode(f)
		  if(m=='{') {f <- f[[length(f)]]; f1 <- as.character(f)[1]}
		  f1=='invisible' || f1=='.Cur.pic'
		}
	  } else FALSE
	}
	if(!inv) {
	  if(do.echo) { cat('\n'); dput(if(do.comments) ex else exs); cat('\n') }
	  print(lv)
	}
  }
}

if(length(device)) dev.off()

if(do.file!='') {
  sink()
  cat('Print output ',if(append)'appended' else 'written',' to file "',
	  sink.file,'".\n',sep='')
  all.files <- unique(c(.Options$.all.do.files, sink.file))
  options(.all.do.files=all.files, TEMPORARY=FALSE)
  if(under.unix) {
    pwd.home <- unix('pwd;echo $HOME')
    cat('$1', paste(paste(pwd.home[1],all.files,sep='/'), collapse=' '),' &\n',
        file=paste(pwd.home[2],'/.lst',sep=''))
    unix('chmod +x $HOME/.lst')
  }
}

invisible()
}


