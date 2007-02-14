.ESC <- '\\'
.BS <- paste(.ESC, .ESC, sep='')
.NL <- paste(.BS, .BS, sep='')

attrCheck <- function(x, which) {
  ans <- attr(x=x, which=which)
  if(is.null(ans)){
    FALSE
  } else {
    ans
  }
}


'setLatexClass<-' <- function(x, value) {
  class(x) <- c(value, 'latexObj')
  x
}

joinLatex <- function(...) {
  elements <- list(...)
  newObj <- list()

  for(element in elements) {
    if(! inherits(element, what='latexObj.list')) {
      newObj <- c(newObj, list(element))
    } else {
      newObj <- c(newObj, element)
    }
  }

  latexMakeObj(newObj)
}

'concatstr<-' <- function(x, value) {
  if(!length(x) || is.na(x)) {
    value
  } else {
    paste(x, value, sep = '')
  }
}

coerceLatex <- function(args) {
  if(inherits(args, what = 'latexObj')) {
    args
  } else if(is.character(args) || is.numeric(args) && is.vector(args) && length(args) < 2) {
    latexMakeString(args)
  } else if(is.list(args)) {  
    lapply(args, function(x) {
      if(inherits(x, what = 'latexObj')) {
        return(x)
      } else if ((is.character(x) || is.numeric(x)) && is.vector(x) && length(x) < 2) {
        latexMakeString(x)
      } else {
        str(x)
        stop('object x is an unhandled object type')
      }
    })
  } else {
    str(args)
    stop('object args is an unhadled object type')
  }
}

checkLatex <- function(args) {
  if(is.null(args)) return(TRUE)

  if(inherits(args, what='latexObj') ||
     ((is.character(args) || is.numeric(args)) &&
      is.vector(args) && length(args) < 2)) {
    TRUE
  } else if(is.list(args)) {
    any(sapply(args, function(x) {
      inherits(x, what = 'latexObj') || ((is.character(x) || is.numeric(x)) && is.vector(x) && length(x) < 2)
    }))
  } else {
    FALSE
  }
}

checkCoords <- function(coords) {
  if(is.null(coords)) return(TRUE)
  
  any(sapply(coords, function(x) inherits(x, what = c('numeric')) && length(x) == 2))
}

listify <- function(arg) {
  if(missing(arg) || is.list(arg) || is.null(arg)) {
    arg
  } else {
    list(arg)
  }
}

checkArgs <- function(obj, args, body, space) {
  if(!missing(space) && (!is.vector(space) || !is.logical(space))) {
    stop("the space argument must be a logical value")
  }
  
  if(!missing(obj) && !inherits(obj, what='latexObj')) {
    stop("the object obj must be of class latexObj")
  }
  
  if(!missing(args) && !checkLatex(args)) {
    stop("All objects in args must be of class latexObj")
  }

  if(!missing(body) && !checkLatex(body)) {
    stop("All objects in body must be of class latexObj or Character")
  }
}  

'mergeLatexAttribs<-' <- function(x, value) {
  new.attribs <- x

  if(!is.null(new.attribs$usepackages)) {
    new.attribs$usepackages <- unique(c(new.attribs$usepackages, value$usepackages))
  }

  if(is.null(new.attribs$space)) {
    if(is.null(value$space)) {
      new.attribs$space <- FALSE
    } else {
      new.attribs$space <- value$space
    }
  }

  new.attribs
}

'setLatexArgs<-' <- function(x, value) {
  checkArgs(args = value)

  if(!is.null(value)) {
    if(! all(sapply(value, FUN=inherits, what=c('latexObj.argument')))) {
      x[[2]] <- list(latexMakeArg(value))
    } else {
      x[[2]] <- value
    }
  }

  x
}

getLatexArgs <- function(obj) {
  if(length(obj) < 2) {
    return(NULL)
  }

  obj[[2]]
}

'setLatexBody<-' <- function(x, value) {
  checkArgs(body = value)

  if(!is.null(value)) {
    if(! inherits(value, what='latexObj.list')) {
      x[[3]] <- latexMakeObj(value)
    } else {
      x[[3]] <- value
    }
  }

  x
}

getLatexBody <- function(obj) {
  if(length(obj) < 3) {
    return(NULL)
  }
  
  obj[[3]]
}

'setLatexAttribs<-' <- function(x, value) {
  if(is.null(value$space)) {
    value$space <- FALSE
  }

  attributes(x) <- value

  x
}

setLatexAttribs <- function(obj, usepackages, space) {
  if(!missing(space)) {
    attr(obj, 'space') <- space
  } else {
    attr(obj, 'space') <- FALSE
  }
    
  if(!missing(usepackages)) {
    attr(obj, 'usepackages') <- usepackages
  }
  
  obj
}

latexMakeObj <- function(body, usepackages) {
  checkArgs(body = body)
  body <- coerceLatex(body)

  if(inherits(body, what='latexObj') && !inherits(body, what='latexObj.list')) {
    body <- list(body)
  }
  
  newObj <- structure(body)

  setLatexClass(newObj) <- 'latexObj.list'
  
  setLatexAttribs(newObj, usepackages = usepackages)
  newObj
}

latexMakeString <- function(body, space, verbatim = FALSE) {
  if(length(body) != 1) {
    stop("strings must be of length one")
  }

  ## split the string on spaces and newlines
  body <- gsub('\\s*\n\\s*', escapestr(paste(" ", .BS, " ", sep='')), body)
  newObjs <- lapply(strsplit(body, '\\s+')[[1]],
                    function(word) {
                      if(word == .BS) {
                        obj <- structure(word, space=FALSE)
                      } else {
                        obj <- structure(word, space=TRUE)
                      }
                      
                      setLatexClass(obj) <- 'latexObj.character'
                      obj
                    })


  newObjs[[1]] <- setLatexAttribs(newObjs[[1]], space = space)

  
  latexMakeObj(newObjs)
}

## creates a object that is a non optional latex argument for an argument option
latexMakeGroup <- function(body, space = FALSE) {
  checkArgs(body = body)

  body <- coerceLatex(body)
  newObj <- structure(body, optional = FALSE)

  setLatexClass(newObj) <- "latexObj.group"
  
  setLatexAttribs(newObj, space = space)
}

## Create a latex argument which is a list of objects of latexObj
## a latexObj.argument is just a latexObj.group with a diffrent class
latexMakeArg <- function(body, optional = FALSE) {
  checkArgs(body = body)

  if(inherits(body, what='latexObj.argument')) {
    if(! missing(optional)) {
      attr(body, 'optional') <- optional
    }
    
    return(body)
  }

  if(length(body) > 1 &&
     all(sapply(body, FUN=inherits, what='latexObj.argument'))) {
    return(body)
  }
  
  if(! inherits(body, what='latexObj.group')) {
    newObj <- latexMakeGroup(body = body, space = FALSE)
  } else {
    newObj <- body
  }
  
  attr(newObj, 'optional') <- optional

  setLatexClass(newObj) <- c('latexObj.argument', 'latexObj.group')
  newObj
}

## Creates an latex coords argument for feeding into a argument option
latexMakeCoord <- function(coords) {
  checkArgs(coords = coords)

  newObj <- structure(coords)

  setLatexClass(newObj) <- "latexObj.coordinate"
  newObj
}

## latex Object for enviroments
latexMakeEnv <- function(envName, args, body, usepackages) {
  checkArgs(args = args, body = body)

  newObj <- list(envName)

  setLatexClass(newObj) <- "latexObj.enviroment"

  setLatexArgs(newObj) <- if(missing(args)) {
    NULL
  } else {
    args
  }
    
  setLatexBody(newObj) <- if(missing(body)) {
    NULL
  } else {
    body
  }

  setLatexAttribs(newObj, usepackages = usepackages)
}

## Creates latex code for assignment functions
latexMakeAssign <- function(cmd, target, args) {
  checkArgs(args = args)

  args <- latexMakeArg(body = args, optional=FALSE)
  if(length(args) > 1) {
    stop('multiple arguments cannot be passed to an assignment')
  }
  
  newObj <- latexMakeCmd(cmd, args = c(latexMakeArg(latexMakeString(paste(.BS, target, sep=''))), args))
}

## makes latex object for functions
latexMakeCmd <- function(cmdName, args, usepackages, space) {
  checkArgs(args = args, space = space)

  
  newObj <- list(cmdName)
  
  setLatexClass(newObj) <- 'latexObj.command'

  if(missing(args)) {
    setLatexArgs(newObj) <- NULL
  } else {
    setLatexArgs(newObj) <- args
  }

  setLatexAttribs(newObj, usepackages = usepackages,
                  space=space)
}

latexConvertTable <- function(matrix, row.names=TRUE, col.names=TRUE) {
  if(!is.matrix(matrix) && !is.data.frame(matrix)) {
    stop("matrix must be a data.frame or a matrix")
  }

  colnames <- dimnames(matrix)[[2]]
  rownames <- dimnames(matrix)[[1]]

  if(row.names && !is.null(rownames)) {
    matrix <- cbind(rownames, matrix)
  }

  string <- apply(matrix, 1, paste, collapse='&')

  if(col.names) {
    colnames.line <- paste(colnames, collapse='&')
    if(length(colnames) < ncol(matrix)) {
      colnames.line <- paste('&', colnames.line, sep='')
    }

    string <- c(colnames.line, string)
  }

  latexMakeString(paste(string, '\n', sep='', collapse=''))
}

print.latexObj <- function(x, indent=0, delta = 2, ...) {
  lines <- format(x, indent = indent, delta = delta)
  max.len <- nchar(length(lines), type='w')
  blank <- makeNstr(' ', max.len)
  
  line.num <- seq(along.with=lines)
  
  
  cat(paste(substring(blank, 1, max.len - nchar(line.num, type='w')),
            '[', line.num,'] "', lines, '"', sep='', collapse='\n'),
      '\n', sep='')

  invisible(x)
}

processLatexThings <- function(obj, lines = character(), indent, delta, ...) {
  pagewidth <- options('width')[[1]]
  textwidth <- pagewidth - indent

  line <- lines[length(lines)]
  lines <- lines[-length(lines)]

  results <- lapply(obj, FUN = format, indent = indent, delta = delta, ...)
  new.attribs <- list()

  for(i in seq(along.with = results)) {
    attribs <- attributes(results[[i]])

    if(attribs$space && length(line) && line != '') {
      concatstr(line) <- ' '
    }

    concatstr(line) <- results[[i]][1]

    if(length(results[[i]]) > 1) {
      if(nchar(line, type = 'width') >= textwidth) {
        line <- strwrap(line, indent = indent, exdent = indent, width=pagewidth)
      }
      
      lines <- c(lines, line, results[[i]][-1])
      line <- ''
    } else if(results[[i]] == .BS) {
      line <- strwrap(line, indent = indent, exdent = indent, width = pagewidth)

      lines <- c(lines, line)
      line <- ''
    }

    mergeLatexAttribs(new.attribs) <- attribs
  }

  if(length(lines) > 1 || nchar(line, type='width') >= textwidth) {
    line <- strwrap(line, indent = indent, exdent = indent, width = pagewidth)
  }

  result <- c(lines,line)
  setLatexAttribs(result) <- new.attribs

  result
}

format.latexObj <- function(x, indent = 0, delta = 2, concat=TRUE, ...) {
  processLatexThings(x, indent=indent, delta=delta, ...)
}

format.latexObj.character <- function(x, indent = 0, ...) {
  unclass(x)
}

format.latexObj.coordinate <- function(x, indent = 0, ...) {
  pagewidth <- 80 - indent
  totallen <- 0

  len <- indent
  coords <- ''
  line <- 1

  ## run loop for coords
  for(i in seq(along.with=x)) {
    new.coords <- paste('(', x[[i]][1], ',', x[[i]][2], ')', sep = '')
    newlen <- nchar(new.coords, type='width')

    if(newlen + len > 80) {
      if(len == indent) {
        ## If the len has not changed then suck it up and just put it there and advance to the next line
        coords <- new.coords
        line <- line + 1

        ## Zero newlen so that len does not change
        newlen <- 0
      } else {
        # go to next line and place text there
        line <- line + 1
        coords[line] <- new.coords

        ## Zero len
        len <- 0
      }
    } else {
      if(len == indent) {
        ## First part of this line
        coords <- new.coords
      } else {
        ## Add new.cords on the end of the current line
        concatstr(coords[line]) <- new.coords
      }
    }

    ## add newlen to cordlen to find current line length
    len <- len + newlen
  }

  ## If we have multiple lines add indent and collapse on \n
  if(length(coords) > 1) {
    ## add indent and collapse on \n
    coords[length(coords)] <- paste(makeNstr(' ', indent), coords, sep='')
  }

  ## else return with total length
  setLatexAttribs(coords)
}
  
format.latexObj.group <- function(x, indent = 0, delta = 2, ...) {
  pagewidth <- options('width')$width
  textwidth <- pagewidth - indent

  result <- format.latexObj(x, indent = indent + delta, delta = delta)    
    
  if(attr(x, 'optional')) {
    delim <- c('[',']')
  } else {
    delim <- c('{', '}')
  }

  if(length(result) == 1 && nchar(result, type='width') >= textwidth) {
    result <- strwrap(result, indent=indent, exdent=indent, width = pagewidth)
  }
  
  ## add open and closing delim
  if(length(result) == 1) {
    result <- paste(delim[1], result, delim[2], sep='')
  } else {
    result <- c(delim[1], result, paste(makeNstr(' ', indent), delim[2], sep = ''))
  }

  setLatexAttribs(result)
}

format.latexObj.enviroment <- function(x, indent=0, delta=2, ...) {
  attribs <- attributes(x)
  pagewidth <- options('width')[[1]]
  textwidth <- pagewidth - indent

  env.name <- x[[1]]
  body <- getLatexBody(x)
  args <- getLatexArgs(x)

  lines <- paste(.ESC, "begin{", env.name, "}", sep = '')

  ## Process args
  if(!is.null(args)) {
      lines <- processLatexThings(args, lines, indent = indent,
                                    delta = delta, ...)
  }
  ## Insert new line
  lines <- c(lines, '')

  ## Run body
  lines <- processLatexThings(body, lines, indent = indent + delta, delta = delta, ...)
  
  setLatexAttribs(c(lines, paste(.ESC, 'end{', env.name, '}', sep='')))
}

format.latexObj.command <- function(x, indent=0, delta = 2, ...) {
  attribs <- attributes(x)

  pagewidth <- options('width')[[1]]
  textwidth <- pagewidth - indent

  string <- paste(.ESC, x[[1]], sep='')

  args <- getLatexArgs(x)
  if(! is.null(args)) {
    for(i in seq(along.with = args)) {
      result <- format.latexObj.group(args[[i]], indent = indent, delta = delta, ...)

      space <- attr(result, 'space')

      if(is.null(space)) {
        space <- FALSE
      }

      if(space) {
        concatstr(string[length(string)]) <- ' '
      }

      concatstr(string[length(string)]) <- result[1]

      if(length(result) > 1) {
        if(nchar(string[length(string)], type='width') >= textwidth) {
          string <- c(string[-length(string)],
                      strwrap(string[length(string)], indent = indent, exdent = indent,
                              width = pagewidth))
        }

        string <- c(string, result[-1])
      }
    }     
  } else {
    ## Add trailing {} to finish command
    concatstr(string) <- "{}"
  }

  if(nchar(string[length(string)], type='width') >= textwidth) {
    string <- c(string[-length(string)],
                strwrap(string[length(string)], indent = indent, exdent = indent,
                        width = pagewidth))
  }
  
  setLatexAttribs(string)
}

        
'latexInsert<-' <- function(x, pos, value) {
  if(! checkLatex(x, which='latexObj')) {
    stop("x must be an object of call latexObj")
  }

  if(! checkLatex(value, which='latexObj')) {
    stop("valus must be an object of call latexObj")
  }

  xlen <- length(x)

  if(xlen == 0) {
    return(x)
  }
  if(length(pos) != 1 || pos > xlen) {
    stop("pos must be an integer less or equal to length of x")
  }

  pos <- pos - 1
  splits <- unname(split(x, rep(c(0,1), times = c(pos, length(x) - pos))))

  obj <- structure(c(splits[[1]], listify(value), splits[[2]]))
  attribs <- attributes(x)
  attribs[["names"]] <- NULL
  mostattributes(obj) <- attribs

  obj
}

'latexAdd<-' <- function(x, value) {
  if(! checkLatex(x, which='latexObj')) {
    stop("x must be an object of call latexObj")
  }

  if(! checkLatex(value, which='latexObj')) {
    stop("valus must be an object of call latexObj")
  }

  obj <- c(x, value)
  attribs <- attributes(x)
  attribs[["names"]] <- NULL
  mostattributes(obj) <- attribs

  obj
}
