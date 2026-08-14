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

## -----------------------------------------------------------------------
## Typst rendering support for print.summary.mChoice().
##
## Depends on (assumed already present elsewhere in Hmisc):
##   - markupSpecs$typst  (bold used here)
##   - typstTranslate()
##   - typstAsis()
##
## Contains:
##   NEW helper   psum_typst_freq_table    (1-D table/named vector ->
##                                           native Typst #table(), no
##                                           tinytable dependency, same
##                                           convention as desc.r's
##                                           typst_counts_table)
##   NEW helper   psum_typst_matrix_table  (2-D matrix with row/col
##                                          dimnames -> native Typst
##                                          #table() with a blank corner
##                                          cell and row/column headers
##                                          -- needed for the pairwise
##                                          crosstab, which
##                                          typst_counts_table's flat
##                                          vector shape can't handle)
##   MODIFIED     print.summary.mChoice    (one new branch, marked below,
##                                          parallel to the existing
##                                          prType()=='html' branch)
##
## Naming: the two new helpers use underscores, not dots, per the
## convention established for desc.r's new functions -- so they read
## unambiguously as plain functions, not S3 methods.
##
## Note on render=FALSE: this matters specifically because
## formatdescribeSingle (in desc.r) calls
## print(x$mChoice, render=FALSE) when a describe()'d variable is an
## mChoice column -- it needs back a plain, unfenced Typst markup
## string to append into its own accumulating character vector (which
## typst_describe() later wraps in exactly ONE outer typstAsis() call),
## not an immediately-emitted asis object. This mirrors exactly how the
## existing html branch returns htmltools::HTML(R) (classed, not yet
## emitted) for render=FALSE vs. rendHTML(R) (immediately asis-emitted)
## for render=TRUE. Before this fix, calling this function with
## render=FALSE under prType()=='typst' fell through to the plain-text
## fallback, which cat()s directly to the console and returns nothing
## usable -- a real bug for the typst path, not just a missing feature.
## -----------------------------------------------------------------------


## =========================================================================
## NEW: psum_typst_freq_table
## Renders a simple 1-D table/named vector (x$nchoices, x$combos) as a
## native Typst table: one header row of names, one row of values.
## =========================================================================
psum_typst_freq_table <- function(tab) {
  nms  <- names(tab)
  if(! length(nms)) nms <- as.character(seq_along(tab))
  nms  <- typstTranslate(nms)
  vals <- typstTranslate(as.character(unclass(tab)))
  k    <- length(tab)
  cols <- paste(rep('auto', k), collapse = ', ')
  header <- paste(paste0('[*', nms,  '*]'), collapse = ', ')
  body   <- paste(paste0('[',  vals, ']'),  collapse = ', ')
  paste0('#table(\n  columns: (', cols, '),\n  align: center,\n  ',
         header, ',\n  ', body, '\n)')
}


## =========================================================================
## NEW: psum_typst_matrix_table
## Renders a matrix with row/column dimnames (the pairwise crosstab) as
## a native Typst table: blank corner cell, column headers across the
## top, row headers down the left. Values are used as-is (already
## formatted/character, including the blanked-out lower triangle) --
## this function only builds table structure, not numeric formatting.
## =========================================================================
psum_typst_matrix_table <- function(m) {
  rn <- typstTranslate(rownames(m))
  cn <- typstTranslate(colnames(m))
  k  <- ncol(m)
  cols <- paste(rep('auto', k + 1), collapse = ', ')
  header <- paste(c('[ ]', paste0('[*', cn, '*]')), collapse = ', ')
  rows <- character(nrow(m))
  for(i in seq_len(nrow(m))) {
    cellvals <- typstTranslate(trimws(m[i, ]))
    cells <- c(paste0('[*', rn[i], '*]'), paste0('[', cellvals, ']'))
    rows[i] <- paste(cells, collapse = ', ')
  }
  paste0('#table(\n  columns: (', cols, '),\n  align: center,\n  ',
         header, ',\n  ', paste(rows, collapse = ',\n  '), '\n)')
}


## =========================================================================
## MODIFIED: print.summary.mChoice
## Change from the existing version: one new branch (marked below),
## parallel to the existing prType()=='html' branch. Everything else
## (the html branch itself, and the plain-text fallback) is unchanged.
## =========================================================================
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

  ## --- NEW: typst branch, parallel to the html branch above ---
  if(prType() == 'typst') {
    m   <- markupSpecs$typst
    lev <- x$levels
    if(length(lev)) {   # short was in effect
      lev  <- paste0('(', 1 : length(lev), ') ', typstTranslate(lev))
      half <- ceiling(length(lev) / 2)
      left <- lev[1 : half]
      rt   <- lev[(half + 1) : length(lev)]
      if(length(rt) < length(left)) rt <- c(rt, '')
      rows <- paste0('[', left, '], [', rt, ']')
      R <- c(R, paste0('#table(\n  columns: (auto, auto),\n  ',
                       paste(rows, collapse = ',\n  '), '\n)'))
    }

    R <- c(R,
           paste0(x$nunique, ' unique combinations'),
           '',
           m$bold('Frequencies of Numbers of Choices Per Observation'),
           psum_typst_freq_table(x$nchoices),
           '',
           m$bold('Pairwise Frequencies (Diagonal Contains Marginal Frequencies)'),
           psum_typst_matrix_table(crosstab),
           '',
           m$bold(s),
           psum_typst_freq_table(x$combos))

    content <- paste(R, collapse = '\n\n')
    return(if(render) typstAsis(content) else content)
  }
  ## ---------------------------------------------------------------

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
