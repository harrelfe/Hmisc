"[.factor" <- function(x, i, drop=TRUE) {  ## was ... 4nov02
## Jens Oehlschlaegel generalized to handle drop 12Oct97
  atx <- attributes(x)
  nam <- atx$names
  atx$levels <- atx$names <- NULL
  if(missing(i)) i <- TRUE  ## 4nov02
  y <- as.integer(x)[i]     ## 4nov02
  ln <- length(nam)
  nam <- if(ln) nam[i] else NULL  ## 4nov02
  opt <- .Options$drop.factor.levels
  if(!length(opt)) opt <- .Options$drop.unused.levels
  ## !missing(drop) added 31jul02
  if(drop && (!missing(drop) || (length(opt)==0 || opt))) {
	oldClass(y) <- NULL
	j <- sort(unique(y))
	y[] <- match(y,j)
	levels(y) <- levels(x)[j]
  } else if(length(y)) levels(y) <- levels(x)
  attributes(y) <- c(attributes(y), atx, if(ln)list(names=nam))
  y
}

## Replaced with one more like default R  3nov02
## With R 1.6 was getting error with ... arguments
if(FALSE) '[.factor' <- function (x, i, drop = TRUE)
{
    y <- NextMethod("[")
    class(y) <- class(x)
    attr(y, "contrasts") <- attr(x, "contrasts")
    attr(y, "levels") <- attr(x, "levels")
    opt <- .Options$drop.factor.levels
    if(!length(opt)) opt <- .Options$drop.unused.levels
    if(drop && (!missing(drop) || (length(opt)==0 || opt)))
      reFactor(y)
    else y
}


##For compatibility with SV4
if(!exists('oldUnclass'))  oldUnclass  <- unclass
if(!exists('oldClass'))    oldClass    <- class
if(!exists('oldClass<-'))
  'oldClass<-' <- function(x, value) {
    class(x) <- value
    x
  }
if(!exists('logb'))        logb        <- log

if(!exists('existsFunction')) existsFunction <- function(...)
  exists(..., mode='function')
if(!exists('getFunction')) getFunction <- function(...)
  get(..., mode='function')

if(!exists('is.category'))
  is.category <- function(x) length(attr(x,'levels')) > 0 && mode(x)=='numeric'
# R doesn't have this

if(!exists('as.category'))
  as.category <- function(x) {
    x <- as.factor(x)
    class(x) <- NULL
    x
  }


termsDrop <- function(object, drop, data) {
  trm <- terms(object, data=data)
  if(is.numeric(drop)) {
    vars <- attr(trm, 'term.labels')
    if(any(drop > length(vars))) stop('subscript out of range')
    drop <- vars[drop]
  }
  form <- update(trm,
                 as.formula(paste('~ . ',
                                  paste('-',drop,collapse=''))))
  terms(form, data=data)
}


untangle.specials <- function (tt, special, order = 1) {
  ## From survival5
  spc <- attr(tt, "specials")[[special]]
  if (length(spc) == 0)
    return(list(vars = character(0), terms = numeric(0)))
  facs <- attr(tt, "factor")
  fname <- dimnames(facs)
  ff <- apply(facs[spc, , drop = FALSE], 2, sum)
  list(vars = (fname[[1]])[spc], terms = seq(ff)[ff & match(attr(tt,
                                   "order"), order, nomatch = 0)])
}    

var.inner <- function(formula) {
  if(!inherits(formula,"formula")) formula <- attr(formula,"formula")
  if(!length(formula)) stop('no formula object found')
	if(length(formula) > 2)
		formula[[2]] <- NULL  # remove response variable
  av <- all.vars(formula)
  ## Thanks to Thomas Lumley <tlumley@u.washington.edu> 28Jul01 :
  unique(sapply(attr(terms(formula),"term.labels"),
         function(term,av)
	av[match(all.vars(parse(text=term)),av)][1],
                av=av) )
}
