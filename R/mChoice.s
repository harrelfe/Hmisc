mChoice <- function(..., label='', 
                    sort.levels=c('original','alphabetic'),
                    add.none=FALSE, drop=TRUE, ignoreNA=TRUE)
{
  sort.levels <- match.arg(sort.levels)
  dotlist <- list(...)
  if(label == '') label <- label(dotlist[[1]])
  if(label == '') label <- as.character(sys.call())[2]

  if (drop)
    lev <- unique(as.character(unlist(dotlist)))
  else
    lev <- unique(unlist(lapply(dotlist, function(x) levels(as.factor(x)))))
  if(ignoreNA) lev <- setdiff(lev, NA)
  if(sort.levels=='alphabetic') lev <- sort(lev)

  lev <- lev[trimws(lev) != '']
  dotlist <- lapply(dotlist, FUN=match, table=lev) #, nomatch=0)

  g <- function(...) {
    set <- c(...)
    set <- set[!is.na(set)]
    if(! length(set)) return(NA)   # was return('')
    paste(sort(unique(set)), collapse=';')
  }
  
  Y <- do.call(mapply,
               c(list(FUN=g, SIMPLIFY=TRUE, USE.NAMES=FALSE, MoreArgs=NULL),
                 dotlist))

  if(add.none && any(Y=='') && 'none' %nin% lev) {
    lev <- c(lev, 'none')
    Y[Y==''] <- as.character(length(lev))
  }
  if(add.none && any(is.na(Y)) && 'none' %nin% lev) {
    lev <- c(lev, 'none')
    Y[is.na(Y)] <- as.character(length(lev))
  }

  
  structure(Y, label=label, levels=lev, class=c('mChoice','labelled'))
}

Math.mChoice <- function(x, ...) {
    stop(.Generic, " not meaningful for mChoice")
}

Summary.mChoice <- function(..., na.rm) {
  .NotYetImplemented()
}

Ops.mChoice <- function(e1, e2)
{
    ok <- switch(.Generic, "=="=, "!="=TRUE, FALSE)
    if(!ok) {
        warning(.Generic, " not meaningful for mChoice")
        return(rep.int(NA, max(length(e1), if(!missing(e2))length(e2))))
    }
    nas <- is.na(e1) | is.na(e2)
    if (nchar(.Method[1])) {
        l1 <- levels(e1)
        e1 <- l1[e1]
    }
    if (nchar(.Method[2])) {
        l2 <- levels(e2)
        e2 <- l2[e2]
    }
    if (all(nchar(.Method)) && (length(l1) != length(l2) ||
                                !all(sort.int(l2) == sort.int(l1))))
        stop("level sets of factors are different")
    value <- NextMethod(.Generic)
    value[nas] <- NA
    value
}

format.mChoice <- function(x, minlength=NULL, sep=";", ...)
{
  lev <- attr(x, 'levels')
  if(length(minlength)) lev <- abbreviate(lev, minlength)
  w <- strsplit(x, ';')
  sapply(w, function(x, lev, sep)
         paste(lev[as.numeric(x)], collapse=sep), lev=lev, sep=sep)
}

'[.mChoice' <- function(x, ..., drop=FALSE) {
  if(drop) stop('drop=TRUE not implemented')
  atr <- attributes(x)
  atr$names <- NULL
  x <- NextMethod('[')
  consolidate(attributes(x)) <- atr
  x
}

print.mChoice <- function(x, quote=FALSE, max.levels=NULL, width = getOption("width"),
                          ...) {
  if (length(x) <= 0)
    cat("mChoice", "(0)\n", sep = "")
  else {
    xx <- x
    class(xx) <- NULL
    levels(xx) <- NULL
    xx[] <- as.character(x)
    print(xx, quote=quote, ...)
  }
  maxl <- if (is.null(max.levels)){
    TRUE
  }else max.levels

  if (maxl) {
    n <- length(lev <- encodeString(levels(x),
                                    quote = ifelse(quote, "\"", "")))
    colsep <- " "
    T0 <- "Levels: "
    if(is.logical(maxl))
      maxl <- {
        width <- width - (nchar(T0, "w") + 3 + 1 + 3)
        lenl <- cumsum(nchar(lev, "w") + nchar(colsep, "w"))
        if(n <= 1 || lenl[n] <= width)
          n
        else max(1, which(lenl > width)[1] - 1)
      }
    drop <- n > maxl
    cat(if(drop) paste(format(n), ""), T0,
        paste(if(drop) {c(lev[1:max(1, maxl - 1)], "...", if (maxl > 1) lev[n])
              }else lev, collapse = colsep), "\n", sep = "")
  }
  invisible(x)
}

as.character.mChoice <- function(x, ...) {
  lev <- levels(x)
  sapply(strsplit(x=x, split=';'),
         function(z) paste(lev[as.integer(z)], collapse=';'))
}

as.double.mChoice <- function(x, drop=FALSE, ...) {
  lev <- attr(x,'levels')
  X <- matrix(0, nrow=length(x), ncol=length(lev),
              dimnames=list(names(x), lev))
  unused <- numeric(0)
  for(i in 1:length(lev)) {
    xi <- 1*inmChoice(x, i)
    if(sum(xi)==0) unused <- c(unused, i)
    X[,i] <- xi
  }
  if(drop && length(unused)) X <- X[, -unused, drop=FALSE]
  X
}

nmChoice <- function(object) {
  y <- gsub('[^;]', '', object)
  nchoices <- nchar(y) + 1
  nchoices[object == ''] <- 0
  nchoices
}

summary.mChoice <- function(object, ncombos=5, minlength=NULL,
                            drop=TRUE, short=FALSE, ...) {

  levels <- NULL
  if(short) {
    levels <- attr(object, 'levels')
    attr(object, 'levels') <- paste0('(', 1 : length(levels), ')')
    }

  nunique <- length(unique(object))
  y <- gsub('[^;]', '', object)
  nchoices <- nchar(y) + 1
  nchoices[object == ''] <- 0
  nchoices <- table(nchoices, dnn=NULL)
  
  X <- as.numeric(object, drop=drop)
  if(length(minlength))
    dimnames(X)[[2]] <- abbreviate(dimnames(X)[[2]],minlength)
  crosstab <- crossprod(X)

  combos <- table(format(object, minlength))
  i <- order(-combos)
  combos <- combos[i[1:min(ncombos,length(combos))]]
  
  structure(list(nunique=nunique, nchoices=nchoices,
                 crosstab=crosstab, combos=combos,
                 label=label(object), levels=levels),
            class='summary.mChoice')
}

print.summary.mChoice <- function(x, prlabel=TRUE, render=TRUE, ...) {
  levels <- x$levels
  crosstab <-format(x$crosstab)
  crosstab[lower.tri(crosstab)] <- ''
  s <- if(length(x$combos)==x$nunique) 'Frequencies of All Combinations' else
   paste('Frequencies of Top', length(x$combos), 'Combinations')

  R <- character(0)

  if(prType() == 'html') {
    lev <- x$levels
    if(length(lev)) {   # short was in effect
      lev <- paste0('(', 1 : length(lev), ') ', lev)
      ml <- max(nchar(lev))
      sz <- if(ml > 45) round(0.825 * 85) else 85
      half <- ceiling(length(lev) / 2)
      left <- lev[1 : half]
      rt   <- lev[(half + 1) : length(lev)]
      if(length(rt) < length(left)) rt <- c(rt, '')
      tab <- paste0('<tr><td>', left,
                    '</td><td>&nbsp;</td><td>', rt,
                    '</td></tr>')
      R <- paste0('<table style="font-size: ', sz, '%";>',
                  paste(tab, collapse=' '), '</table>')
      }
    
    y <- list('', x$nchoices, crosstab, x$combos)
    names(y) <- c(paste(x$nunique, 'unique combinatons'),
                  'Frequencies of Numbers of Choices Per Observation',
                  'Pairwise Frequencies (Diagonal Contains Marginal Frequencies)',
                  s)
    R <- c(R, do.call(htmltabv, y))
    return(if(render) rendHTML(R) else htmltools::HTML(R))
  }

  if(length(levels)) {
    lev <- paste(paste0('(', 1 : length(levels), ') ', levels), collapse='; ')
    cat('', strwrap(lev), '', sep='\n')
    }

  cat('\n', x$nunique, ' unique combinations\n\n', sep='')
  if(prlabel) cat(x$label, '\n\n', sep='')
  cat('Frequencies of Numbers of Choices Per Observation\n\n')
  print(x$nchoices)
  cat('\nPairwise Frequencies (Diagonal Contains Marginal Frequencies)\n\n')
  print(crosstab, quote=FALSE)
  cat('\n', s, '\n')
  print(x$combos)
  invisible()
}

match.mChoice <- function(x, table, nomatch = NA,
                          incomparables = FALSE) {
  if (!is.logical(incomparables) || incomparables) {
    .NotYetUsed("incomparables != FALSE")
  }

  lev <- attr(table, 'levels')
  if(is.factor(x) || is.character(x)) {
    x <- match(as.character(x), lev, nomatch=0)
  }
  return(.Call("do_mchoice_match", as.integer(x), table, as.integer(nomatch)))
}

# inmChoice <- function(x, values) {
#  match.mChoice(values, x, nomatch=0) > 0
# }
inmChoice <- function(x, values, condition=c('any', 'all')) {
  condition <- match.arg(condition)
  lev <- attr(x, 'levels')
  if(is.character(values)) {
    v <- match(values, lev)
    if(any(is.na(v))) stop(paste('values not in levels:',
                                 paste(values[is.na(v)],collapse=';')))
    values <- v
  }
  x <- paste(';', unclass(x), ';', sep='')
  values <- paste(';', values, ';', sep='')
  res <- rep(condition != 'any', length(x))
  for(j in 1:length(values)) {
    i <- grep(values[j], x)
    if(length(i)) {
      if(condition == 'any') res[i] <- TRUE
      else
        res[-i] <- FALSE
      } else if(condition == 'all') res[] <- FALSE
  }
  res
}

inmChoicelike <- function(x, values, condition=c('any', 'all'),
                          ignore.case=FALSE, fixed=FALSE) {
  condition <- match.arg(condition)
  if(! is.character(values)) stop('values must be a character vector')
  x <- as.character(x)
  res <- rep(condition != 'any', length(x))
  for(j in 1 : length(values)) {
    i <- grep(values[j], x, ignore.case=ignore.case, fixed=fixed)
    if(length(i)) {
      if(condition == 'any') res[i] <- TRUE
      else
        res[-i] <- FALSE
      } else if(condition == 'all') res[] <- FALSE
  }
  res
}

      
is.mChoice <- function(x) inherits(x, 'mChoice')
