if(!exists("string.bounding.box")) {
  string.bounding.box <- function(string, type=c("chars", "width")) {
    thisfun <- function(x, type) {
      height <- length(x)
      # get rid of ':' on last string 
      x[height] <- substr(x[height], start=1, stop=nchar(x[height], type='chars') - 1)

      c(height = height, width = max(nchar(x, type=type)))
    }

    mode(string) <- "character"

    type <- match.arg(type)

    ## Add remove '\n' if it is ends the string and add a ':' so that string split
    ## functions the way I want it to.
    string <- paste(string, ':', sep='')

    ans <- sapply(strsplit(string, '\n', fixed=TRUE), FUN=thisfun, type=type, USE.NAMES=FALSE)
    return(list(columns = ans[2,], rows = ans[1,]))
  }
}

equalBins <- function(widths, subwidths) {
  ## The length of widths and subwidths must be the same
  if(length(widths) != length(subwidths)) {
    stop("width and subwidth must be of the same length")
  }

  ## adjust width for column spacers
  widths <- widths - unlist(lapply(subwidths, length)) + 1
  unlist(mapply(function(width, subwidths) {
    if(sum(subwidths) < width) {
      div <- width %/% length(subwidths)
      mod <- width %% length(subwidths)
      c(rep.int(div + 1, mod), rep.int(div, length(subwidths) - mod))
    } else {
      subwidths
    }
  }, widths, subwidths, SIMPLIFY = FALSE))
}

stringDims <- function(string) {
  if(is.null(string)) {
    return(height = 0, width = 0)
  }
  
  dims <- dim(string)

  bbox <- string.bounding.box(string)
  height <- bbox$rows
  width <- bbox$columns

  if(any(dims)) {
    dim(height) <- dims
    dim(width) <- dims
  }
  
  list(height = height, width = width)
}

simplifyDims <- function(x) {
  if(any(sapply(x, FUN=is.matrix)))
    do.call(rbind, x)
  else
    do.call(c, x)
}

partition.vector <- function(x, sep, ...) {
  if(missing(sep)) {
    stop("sep is a required arg")
  }

  if(sum(sep) != length(x)) {
    stop("sep must sum to the number of columns in x")
  }

  split(x, rep(seq(along.with=sep), times=sep))
}


partition.matrix <- function(x, rowsep, colsep, ...) {  
  colmissing <- missing(colsep)
  rowmissing <- missing(rowsep)
  
  if(rowmissing && colmissing) {
    stop("Atleast one of rowsep or colsep args must be specified")
  }
  
  ## If length of group is equal to length of x assume that this is a
  ## a vector of group numbers
  if(!rowmissing) {
    if(sum(rowsep) != NROW(x)) {
      stop("rowsep must sum to the number of columns in x")
    }
    if(!is.numeric(rowsep)) {
      stop("the rowsep vector must be numeric")
    }
  }

  if(!colmissing) {
    if(sum(colsep) != NCOL(x)) {
      stop("colsep must sum to the number of rows in x")
    }
    if(!is.numeric(colsep)) {
      stop("the colsep vector must be numeric")
    }
  }

  ## Separate x into row chunks
  if(!rowmissing) {
    set <- lapply(split(seq(NROW(x)), rep(seq(along.with=rowsep), times=rowsep)), function(index) x[index,,drop=FALSE])
  } else {
    set <- NULL
  }

  if(!colmissing) {
    FUN <- function(x)
      lapply(split(seq(NCOL(x)), rep(seq(along.with=colsep), times=colsep)), function(index) x[,index,drop=FALSE])
    
    if(is.null(set)) {
      FUN(x)
    } else {
      lapply(set, FUN)
    }
  } else {
    set
  }
} 
  

print.char.list <- function(x, ..., hsep = c("|"), vsep = c("-"), csep = c("+"),
                            print.it = TRUE, rowname.halign = c("left", "centre", "right"),
                            rowname.valign = c("top", "centre", "bottom"),
                            colname.halign = c("centre", "left", "right"),
                            colname.valign = c("centre", "top", "bottom"),
                            text.halign = c("right", "centre", "left"),
                            text.valign = c("top", "centre", "bottom"), rowname.width,
                            rowname.height, min.colwidth = .Options$digits, max.rowheight = NULL,
                            abbreviate.dimnames = TRUE, page.width = .Options$width,
                            colname.width, colname.height, prefix.width,
                            superprefix.width = prefix.width) {

  vjustText <- function(char.matrix, fieldHeight, vjust = c("top", "center", "bottom")) {
    if(!is.matrix(char.matrix))
      stop("char.matrix must be of type matrix")

    d <- dim(char.matrix)

    vjust <- match.arg(vjust)

    if(! is.character(char.matrix))
      char.matrix <- as.character(char.matrix)

    # split the matrix strings up into multi lines.
    char.matrix <- ifelse(is.na(char.matrix), NA, string.break.line(char.matrix))

    # determine veritcal differentials
    vdiff <- fieldHeight - unlist(lapply(char.matrix, length))

    ans <- mapply(function(element, vdiff, target) {
      if(is.na(element) || vdiff == 0) {
        return(element)
      }
      
      if(vdiff < 0) {
        # Trim trailing extra lines
        lines <- rev(seq(along.with=element)[element != ''])
        if(lines[1] <= target) {
          return(element[1:target])
        }

        length(element) <- lines[1]

        element <- element[element != '']

        if(length(element) == target)
          return(element)
        
        vdiff <- target - length(element)
      }
      
      switch(vjust,
             top = c(element, character(vdiff)),
             bottom = c(character(vdiff), element),
             center = c(character(half <- vdiff%/%2), element, character(vdiff - half)))
     
    }, char.matrix, vdiff, fieldHeight, USE.NAMES=FALSE)
    matrix(unlist(ans), ncol=d[[2]])
  }

  hjustText <- function(char.matrix, fieldWidth, hjust=c("left","right","center")) {
    if(!is.matrix(char.matrix))
      stop("text must be of type matrix")

    d <- dim(char.matrix)
    
    
    hjust <- match.arg(hjust)

    ans <- mapply(function(column, target) {
      column <- unlist(column)
      column <- ifelse(is.na(column), NA, format(column, justify=hjust, width=target))

      column <- ifelse(is.na(column) || target - nchar(column, type="width"), column, strtrim(column, target))
    }, split(char.matrix, col(char.matrix)), fieldWidth, USE.NAMES=FALSE)
#    dim(ans) <- d
    ans
  }
    
  justText <- function(char.matrix, fieldWidth, fieldHeight, hjust=c("left","right","centre"),
                       vjust = c("top", "centre", "bottom"), trim=FALSE) {
    if(!is.matrix(char.matrix))
      stop("text must be of type matrix")

    ## Get the original dims of the matrix
    d <- dim(char.matrix)

    ## Determin the wanted justification.
    hjust <- match.arg(hjust)
    vjust <- match.arg(vjust)

    ## If this is a charater matrix then break in on the lines
    if(is.character(char.matrix)) {
      ## split the matrix strings up into multi lines.
      ans <- ifelse(is.na(char.matrix), NA, string.break.line(char.matrix))
    } else {
      ans <- char.matrix
    }

    ## format the text horizontaly.
    ans <- mapply(function(column, target) {
      fun <- function(x) ifelse(is.na(x), NA, format(x, justify=hjust, width=target))
      
      if(is.list(column)) {
        lapply(column, fun)
      } else {
        fun(column)
      }
    }, split(ans, col(char.matrix)), fieldWidth, USE.NAMES=FALSE)
    
    spacer <- makeNstr(' ', fieldWidth)
    ## Add extra rows to justify the text vericaly.
    ans <- mapply(function(row, target) {
      fun <- function(element, spacer) {
        vdiff <- target - length(element)
        if(is.na(element) || vdiff == 0) {
          return(element)
        }
      
        if(vdiff < 0) {
          ## Trim trailing extra lines
          lines <- rev(seq(along.with=element)[element != ''])
          if(lines[1] <= target) {
            return(element[1:target])
          }

          length(element) <- lines[1]
          
          element <- element[element != '']

          if(length(element) == target)
            return(element)
        
          vdiff <- target - length(element)
        }

        switch(vjust,
               top = c(element, rep(spacer, vdiff)),
               bottom = c(rep(spacer, vdiff), element),
               centre = c(rep(spacer, half <- vdiff%/%2), element, rep(spacer, vdiff - half)))
      }        

      mapply(fun, row, spacer, USE.NAMES=FALSE, SIMPLIFY=FALSE)
    }, split(ans, row(char.matrix)), fieldHeight, USE.NAMES=FALSE, SIMPLIFY=FALSE)
    matrix(unlist(unsplit(ans, row(char.matrix))), ncol=d[[2]])
  }

  printRow <- function(entries, widths, sep) {
    if(length(entries) != length(widths))
      stop("arguments must be the same length")

    first <- TRUE
    last <- TRUE
    env <- environment()
    nval <- ' '
    sep <- hsep

    out <- mapply(function(entry, width) {
      if(is.na(entry)) {
        if(is.null(last)) {
          out <- c(nval, makeNstr(nval, width))
        } else {
          out <- c(sep, makeNstr(nval, width))
          assign("last", NULL, envir=env)
        }
      }else{
        if(is.null(last)) {
          assign("last", TRUE, envir=env)
        }
        out <- c(sep, entry)
      }
      out
    }, entries, widths)
    paste(c(out, sep), collapse='')
  }

  printBars <- function(entries, blank, widths, hsep, csep, vsep) {
    bars <- character(length(entries) + 1)
    alt <- rep(c(1,2), length.out=length(widths))
#    blank <- c(list(rep(TRUE, length(widths))), blank)
    for(i in seq(along.with=entries)) {
      len <- length(entries[[i]])

      comp <- entries[[i]][-len]
      comp.last <- entries[[i]][len]
      
      bnk <- blank[[i]]
      bnk.last <- bnk[length(bnk)]
      
      dividers <- ifelse(comp & bnk, hsep, ifelse(bnk, ' ', ifelse(comp, csep, vsep)))
      dividers <- c(dividers, ifelse(bnk.last, hsep, csep))
      betweens <- c(makeNstr(ifelse(bnk, ' ', vsep), widths), '')

      bars[i] <- paste(dividers, betweens, sep='', collapse='')
    }
    dividers <- ifelse(entries[[length(entries)]], csep, vsep)
    betweens <- c(makeNstr(vsep, widths), '')
    bars[length(bars)] <- paste(dividers, betweens, sep='', collapse='')
    
    bars
  }
  

  rjustText <- function(text, fieldWidth, trim=FALSE) justText(text, fieldWidth, 'right', trim)
  ljustText <- function(text, fieldWidth, trim=FALSE) justText(text, fieldWidth, 'left', trim)
  centerText <- function(text, fieldWidth, trim=FALSE) justText(text, fieldWidth, 'center', trim)
    

  colnames <- NULL
  rownames <- NULL
  colDims  <- NULL
  rowDims  <- NULL
  supercols <- NULL
  superrows <- NULL
  supercolDims <- NULL
  superrowDims <- NULL
  colsets  <- NULL
  rowsets  <- NULL

  if(is.list(x)) {
    rownames <- lapply(x, function(x) {
      if(is.null(rownames <- dimnames(x)[[1]])) {
        rep(NA, NROW(x))
      } else {
        rownames
      }
    })

    rowsets <- unlist(lapply(rownames, length), use.names=FALSE)
    superrows <- names(rownames)
    rownames <- matrix(unlist(rownames, use.names=FALSE), ncol=1)

    if(all(is.na(rownames))) {
      rownames <- NULL
    }

    colnames <- lapply(x[[1]][1,], names)

    colsets   <- unlist(lapply(colnames, length), use.names=FALSE)
    supercols <- names(colnames)
    colnames  <- matrix(unlist(colnames, use.names=FALSE), nrow=1)

    if(all(is.na(colnames))) {
      colnames <- NULL
    }

    ## Convert to a matrix
    matrix <- do.call(rbind, x)
    matrix <- do.call(rbind, tapply(matrix, row(matrix), FUN='unlist'))

  } else {
    rownames <- dimnames(x)[[1]]
    colnames <- dimnames(x)[[2]]
    matrix <- x
  }
  
  ## get widths of each column in table.
  listDims <- stringDims(matrix(sapply(matrix, format),ncol=ncol(matrix)))

  ## find the widths and heights of the row names and col names
  ## if any elements do not have rownames the set them equal to 0
  if(length(colnames)) {
    colDims <- stringDims(colnames)
  } else {
    colDims <- list(height = integer(nrow(matrix)), width = integer(nrow(matrix)))
  }

  if(length(rownames)) {
    rowDims <- stringDims(rownames)
  } else {
    rowDims <- list(height = integer(ncol(matrix)), width = integer(ncol(matrix)))
  }
  
  ## calculate the superdim info
  ## do it for the supercols
  if(length(supercols)) {
    supercolDims <- stringDims(supercols)
  } else {
    supercolDims <- list(height = 0, width = 0)
  }

  ## do it for the superrows
  if(length(superrows)) {
    superrowDims <- stringDims(superrows)
  } else {
    superrowDims <- list(height = 0, width = 0)
  }

  ## Calculate the max column width
  if(!missing(colname.width)) {
    colwidth <- pmin(colDims$width, colname.width)
    supercolwidth <- pmin(supercolDims$width, colname.width*colsets)
  } else {
    colwidth <- colDims$width
    supercolwidth <- supercolDims$width
  }

  ## Calculate the max row hight
  if(!missing(rowname.height)) {
    rowheight <- pmin(rowDims$height, rowname.height)
    superrowheight <- pmin(superrowDims$height, rowname.height*rowsets)
  } else {
    rowheight <- rowDims$height
    superrowheight <- superrowDims$height
  }

  ## Find the overall height of the matrix
  height <- pmax(tapply(listDims$height, row(matrix), max), rowheight)
  height <- equalBins(superrowheight, partition.vector(height, rowsets))

  ## Find the overall width of the matrix
  width  <- pmax(tapply(listDims$width, col(matrix), max), colwidth)
  width  <- equalBins(supercolwidth, partition.vector(width, colsets))
                
  ## Calculate actual supercol widths that is the sum of the subcol or total lenght to supercol
  ## which ever is greater
  supercolwidth <- tapply(width, rep.int(seq(along.with=colsets), times=colsets), sum) + colsets - 1
  supercolheight <- max(superrowDims$height)
  colheight <- max(colDims$height)

  superrowheight <- tapply(height, rep.int(seq(along.with=rowsets), times=rowsets), sum)

  if(missing(prefix.width)) {
    if(!is.null(rownames)) {
      prefix.width <- max(max(na.rm = TRUE, as.integer(median(width)), max(rowDims$width)))
    } else {
      prefix.width <- 0
    }
  }

  if(missing(superprefix.width)) {
    if(!is.null(superrows)) {
      superprefix.width <- max(na.rm = TRUE, as.integer(median(width)), max(superrowDims$width))
    } else {
      superprefix.width <- 0
    }
  }

  header <- NULL
  headerwidth <- NULL
  rows <- NULL
  entries <- list()
  blanks <- list()

  
  ## Figure out the centering of the cells.
  rowNameHalign <- match.arg(rowname.halign)
  rowNameValign <- match.arg(rowname.valign)
  colNameHalign <- match.arg(colname.halign)
  colNameValign <- match.arg(colname.valign)
  cellHalign <- match.arg(text.halign)
  cellValign <- match.arg(text.valign)

  ## create the superrowname column
  superrow <- if(!is.null(superrows)) {
    superrows <- matrix(superrows, ncol=1)

    header <- NA
    headerwidth <- superprefix.width
    ## perform verical and horizontal centering.
    justText(superrows, superprefix.width, superrowheight,
             rowNameHalign, rowNameValign)
  }
  
  row <- if(!is.null(rownames)) {
    header <- cbind(header, NA)
    headerwidth <- c(headerwidth, prefix.width)
    justText(rownames, prefix.width, height, rowNameHalign, rowNameValign)
  }

  body <- cbind(superrow, row,
                justText(matrix, width, height, cellHalign, cellValign))

  width <- c(headerwidth, width)

  body <- split(body, row(body))
  
  ## Create the super column name row and the column name row
  if(!is.null(supercols)) {
    supercols <- matrix(supercols, nrow=1)
    
    supercolwidth <- c(headerwidth, supercolwidth)
    entry <- c(header, rep(seq(along.with=colsets), colsets), 0)
    entries <- c(entries, list(ifelse(is.na(entry), FALSE, !duplicated(entry))))

    blank <- ifelse(is.na(c(header, rep(supercols, colsets))), TRUE, FALSE)
    blanks <- c(blanks, list(blank))
    
    rows <- printRow(justText(cbind(header, supercols), supercolwidth, supercolheight,
                              colNameHalign, colNameValign), width=supercolwidth, sep=hsep)
  }

  if(!is.null(colnames)) {
    entry <- c(header, rep(seq(along.with=colnames), 1), 0)
    entries <- c(entries, list(ifelse(is.na(entry), FALSE, !duplicated(entry))))
    
    blank <- ifelse(is.na(c(header, colnames)), TRUE, FALSE)
    blanks <- c(blanks, list(blank))

    rows <- c(rows,
              printRow(justText(cbind(header, colnames), width, colheight,
                                colNameHalign, colNameValign), width=width, sep=hsep))
  }
  

  env <- environment()

  rows <- c(rows, unlist(lapply(split(body, rep(seq(along.with=rowsets), rowsets)), function(set) {
    index <- seq(along.with=set)

    mapply(FUN = function(line, index) {
      entry <- c(ifelse(is.na(line), NA, rep(seq(along.with=line), 1)), 0)
      entry <- ifelse(is.na(entry), FALSE, !duplicated(entry))
      
      assign('entries', c(entries, list(entry)), env)

      blank <- ifelse(is.na(line), FALSE, FALSE)
      if(index != 1) {
        blank[1] <- TRUE
      }
      
      assign('blanks', c(blanks, list(blank)), env)
      printRow(line, width=width, sep=hsep)
    }, set, index)
  }), use.names=FALSE))

  blanks[[1]] <- logical(length(width))
  entries <- lapply(entries, function(entry) {entry[1] <- TRUE; entry})

  bars <- printBars(entries, blanks, width, hsep=hsep, vsep=vsep, csep=csep)
  total <- paste(bars, c(rows, ""), sep='\n', collapse='\n')

  if(print.it) {
    cat(total)
    invisible(x)
  } else {
    total
  }
}

