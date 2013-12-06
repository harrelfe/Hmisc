## $Id$
GetModelFrame <- function(formula, specials, default.na.action=NULL) {
  if(missing(formula) || !inherits(formula, "formula"))
    stop("GetModelFrame needs a function argument specified",
         "as a forumula or terms object")

  ## get the function call of the calling function
  fun.call <- match.call(sys.function(sys.parent()),
                         call=sys.call(sys.parent()),
                         expand.dots=FALSE)

  args.needed <- c("formula", "data", "weights", "subset", "na.action")
  m <- structure(match(args.needed, names(fun.call), nomatch=0), names=args.needed)

  ## get the envronment of the formula
  env <- environment(formula)
  if (is.null(env))
    env <- parent.frame()

  ## If formula is not a terms object then
  ## the formula must be turned into a terms object using the
  ## 'terms' function
  if(!inherits(formula, "terms")) {
    ## Check for precence of args needed for terms call
    has.arg <- c(formula=TRUE, data=FALSE)
    if(m["data"])
      has.arg["data"] <- TRUE

    junk <- lapply(fun.call, print)
    new.call <- fun.call[c(1,has.arg)]
    new.call[[1]] <- as.name('terms')

    names(new.call)[2] <- "x"
    
    if(!missing(specials) && !is.null(specials))
      new.call$specials=specials
    
    ## convert the formula to a terms object
    print(new.call)
    formula <- eval(new.call, envir=env)
#    formula <- do.call("terms", args=list(x=formula,
#                                  data=if(m["data"]) fun.call[m["data"]] else NULL,
#                                  specials=specials)[has.arg],
#                       envir=env)
  }
  
  new.call <- fun.call[c(1, m)]
  new.call[[1]] <- as.name("model.frame")
  new.call$formula <- formula
  
  if("na.action" %nin% names(fun.call) && !is.null(default.na.action))
    new.call$na.action <- default.na.action

  return(eval(new.call, env, parent.frame()))
}

## Replaced with one more like default R  3nov02
## With R 1.6 was getting error with ... arguments
## if(FALSE) '[.factor' <- function (x, i, drop = TRUE)
## {
##   y <- NextMethod("[")
##   class(y) <- class(x)
##   attr(y, "contrasts") <- attr(x, "contrasts")
##   attr(y, "levels") <- attr(x, "levels")
##   opt <- .Options$drop.factor.levels
##   if(!length(opt))
##     opt <- .Options$drop.unused.levels
  
##   if(drop && (!missing(drop) || (length(opt)==0 || opt)))
##     reFactor(y)
##   else y
## }


termsDrop <- function(object, drop, data)
{
  trm <- terms(object, data=data)
  if(is.numeric(drop)) {
    vars <- attr(trm, 'term.labels')
    if(any(drop > length(vars)))
      stop('subscript out of range')
    
    drop <- vars[drop]
  }
  form <- update(trm,
                 as.formula(paste('~ . ',
                                  paste('-', drop, collapse=''))))
  terms(form, data=data)
}


var.inner <- function(formula)
{
  if(!inherits(formula,"formula"))
    formula <- attr(formula,"formula")
  
  if(!length(formula))
    stop('no formula object found')
  
  if(length(formula) > 2)
    formula[[2]] <- NULL  # remove response variable
  
  av <- all.vars(formula, max.names=1e6)
  ## Thanks to Thomas Lumley <tlumley@u.washington.edu> 28Jul01 :
  unique(sapply(attr(terms(formula),"term.labels"),
                function(term,av)
                  av[match(all.vars(parse(text=term), max.names=1e6),av)][1],
                  av=av))
}
