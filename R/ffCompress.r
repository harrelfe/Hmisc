ffCompress <- function(obj, float=c('single', 'double'), print=FALSE) {
  float <- match.arg(float)
  if(! requireNamespace('ff', quietly=TRUE))
    stop('ff package not available')
  nam <- names(obj)
  p   <- length(nam)
  if(print) cat(p, 'variables; Processing variable:')
  vmode <- label <- units <- character(p)
  names(vmode) <- names(label) <- names(units) <- nam

  vm <- function(m, na, neg) {
    ## For integers, given maximum absolute value, whether or not NAs
    ## are present, and whether negative values are present, computes
    ## the ff vmode; compute # bits needed:
    b <- if(m == 0) 0 else 1 + floor(log2(m))
    if(b > 32) 'double'
    else if(na || neg || b > 16) {   ## NAs or negatives present
      if(b > 16) 'integer' else if(b > 8) 'short' else 'byte'
    }
    else { ## no NAs or negatives, 16 or fewer bits
      if(b > 8) 'ushort' else if(b > 4) 'ubyte' else
      if(b > 2) 'nibble' else 'quad'
    }
  }
    
  for(i in 1 : p) {
    if(print) cat(i,'')
    x <- obj[[i]]
    at <- attributes(x)
    if(length(lab <- at$label) | length(un <- at$units)) {
      class(x) <- setdiff(class(x), 'labelled')
      if(length(lab)) {
        label[i] <- lab
        attr(x, 'label') <- NULL
      }
      if(length(un)) {
        units[i] <- un
        attr(x, 'units') <- NULL
      }
      if(is.matrix(x)) class(x) <- c('AsIs', class(x))
      obj[[i]] <- x
    }
    else if(is.matrix(x)) {
      ## As above, e.g. if x is Surv() ffdf will break it into separate
      ## variables if not I(Surv())
      class(x) <- c('AsIs', class(x))
      obj[[i]] <- x
    }
    x <- unclass(x)
    n  <- length(x)
    x  <- x[! is.na(x)]
    lx <- length(x)
    s  <- storage.mode(x)
    vmode[i] <-
      if(s %in% c('character', 'logical')) s
      else if(! length(x)) 'logical'  ## 2 bits if all NAs
        else {
        frac <- any(floor(x) != x)
        if(frac) float
        else vm(max(abs(x)), lx < n, any(x < 0))
      }
  }
  if(print) cat('\n')
  if(all(c(label, units) == ''))
    ff::as.ffdf(obj, vmode=vmode, VERBOSE=print)
  else {
    f <- ff::as.ffdf(obj, vmode=vmode, VERBOSE=print)
    class(f) <- c('ffdflabel', 'ffdf')
    if(any(label != '')) attr(f, 'label') <- label
    if(any(units != '')) attr(f, 'units') <- units
    f
  }
}

as.data.frame.ffdflabel <- function(x, ...) {
  if(! requireNamespace('ff', quietly=TRUE))
    stop('ff package not available')
  nam <- names(x)
  lab <- attr(x, 'label')[nam]
  un  <- attr(x, 'units')[nam]
  class(x) <- setdiff(class(x), 'ffdflabel')
  x <- ff::as.data.frame.ffdf(x)
  for(n in nam) {
    if(lab[n] != '') label(x[[n]]) <- lab[n]
    if(length(un) && un[n]  != '') attr(x[[n]], 'units') <- un[n]
  }
  x
}
  
'[.ffdflabel' <- function(x, ...) {
  if(! requireNamespace('ff', quietly=TRUE))
    stop('ff package not available')
  nam <- names(x)
  lab <- attr(x, 'label')[nam]
  un  <- attr(x, 'units')[nam]
  class(x) <- setdiff(class(x), 'ffdflabel')
  x <- NextMethod('[')
  cx  <- class(x)
  nam <- names(x)
  lab <- lab[nam]
  if(length(un)) un <- un[nam]
  if('ffdf' %in% cx) {
    attr(x, 'label') <- lab
    attr(x, 'units') <- un
    class(x) <- c('ffdflabel', cx)
  }
  else if(is.data.frame(x)) {
    for(n in nam) {
      if(lab[n] != '')               label(x[[n]])         <- lab[n]
      if(length(un) && un[n]  != '') attr(x[[n]], 'units') <- un[n]
    }
  }
  x
}

# From ff documentation:

# byte 8 bit signed integer with NA
# short 16 bit signed integer with NA
# integer 32 bit signed integer with NA

# quad 2 bit unsigned integer without NA
# nibble 4 bit unsigned integer without NA
# ubyte 8 bit unsigned integer without NA
# ushort 16 bit unsigned integer without NA
