discrete <- function(x, levels=sort(unique.default(x), na.last=TRUE),
                     exclude = NA) {
  if(!is.numeric(x)) {
    stop('x must be a numeric vairable')
  }

  exclude <- as.vector(exclude, typeof(x))
  levels <- levels[is.na(match(levels, exclude))]
  
  f <- x[!(x %in% exclude)]
  
  attr(f, 'levels') <- levels
  class(f) <- "discrete"
  f
}

as.discrete <- function(x, ...) UseMethod("as.discrete")

as.discrete.default <- function(x, ...) {
  if(is.discrete(x)) x else discrete(x)
}

is.discrete <- function(x) inherits(x, 'discrete')

"[.discrete" <- function(x, ..., drop=FALSE) {
  y <- NextMethod("[")

  attr(y, 'levels') <- attr(x, 'levels')
  class(y) <- class(x)
  if( drop ) {
    factor(y)
  } else {
    y
  }
}

"[<-.discrete" <- function(x, ..., value) {
  lx <- levels(x)
  cx <- class(x)

  m <- match(value, lx)

  if (any(is.na(m) & !is.na(value))) {
    warning("invalid factor level, NAs generated")
  }

  class(x) <- NULL
  x[...] <- m

  attr(x,"levels") <- lx
  class(x) <- cx
  x
}

"[[.discrete" <- function(x, i)
{
    y <- NextMethod("[[")

    attr(y,"levels")<-attr(x,"levels")
    class(y) <- class(x)
    y
}

"is.na<-.discrete" <- function(x, value)
{
    lx <- levels(x)
    cx <- class(x)
    class(x) <- NULL
    x[value] <- NA
    structure(x, levels = lx, class = cx)
}

"length<-.discrete" <- function(x, value)
{
    cl <- class(x)
    levs <- levels(x)
    x <- NextMethod()
    structure(x, levels=levs, class=cl)
}
