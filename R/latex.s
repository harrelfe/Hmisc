first.word <- function(x, i=1, expr=substitute(x)) {
  words <- if(! missing(x)) as.character(x)[1]
           else
             as.character(unlist(expr))[1]
  
  if(i > 1) stop('i > 1 not implemented')
  
  chars <- substring(words, 1 : nchar(words), 1 : nchar(words))
  legal.chars <- c(letters, LETTERS, '.',
                   '0','1','2','3','4','5','6','7','8','9')
  non.legal.chars <- (1 : length(chars))[chars %nin% legal.chars]
  if(! any(non.legal.chars)) return(words)
  
  if(non.legal.chars[1] == 1) return(character(0))
  
  substring(words, 1, non.legal.chars[1] - 1)
}


##1. if x is a data.frame, then do each component separately.
##2. if x is a matrix, but not a data.frame, make it a data.frame
##   with individual components for the columns.
##3. if a component x$x is a matrix, then do all columns the same.
##4. Use right justify by default for numeric columns.
##5. Use left justify for non-numeric columns.

## The following are made complicated by matrix components of data.frames:
##6. vector cdec must have number of items equal to number of columns
##   of input x.
##7. matrix dec must have number of columns equal to number of columns
##   of input x.
##8. scalar dec is expanded to a vector cdec with number of items equal
##   to number of columns of input x.
##9. vector rdec must have number of items equal to number of rows of input x.
##   rdec is expanded to matrix dec.
##10. col.just must have number of columns equal to number of columns
##    of output cx.

## Value:
## character matrix with character images of properly rounded x.
## matrix components of input x are now just sets of columns of character matrix.
## attr(,col.just) repeats input col.just when provided.
##	Otherwise, recommended justification for columns of output.
##	Default is "l" for characters and factors, "r" for numeric.
##	When dcolumn==T, numerics will have ".".


## FEH 21May96 - changed default for numeric.dollar to cdot
## FEH  5Jun96 - re-written to not rely on as.data.frame,
##               converted data frames to matrices the slow way
##               added matrix.sep 
##     12Aug99 - allowed # decimal places=NA (no rounding, just use format())
##    27May02 - added booktabs FEH
## 13Dec02 - added ctable   FEH
## arguments included check.names=TRUE 23jan03
##
## 16Jan15 (A. Kiermeier) pass "..." to formt() and format()

format.df <- function(x,
                      digits, dec=NULL, rdec=NULL, cdec=NULL,
                      numeric.dollar=! dcolumn, na.blank=FALSE,
                      na.dot=FALSE, blank.dot=FALSE, col.just=NULL,
                      cdot=FALSE, dcolumn=FALSE, matrix.sep=' ',
                      scientific=c(-4,4),
                      math.row.names=FALSE,
                      already.math.row.names=FALSE,
                      math.col.names=FALSE,
                      already.math.col.names=FALSE,
                      double.slash=FALSE,
                      format.Date='%m/%d/%Y',
                      format.POSIXt="%m/%d/%Y %H:%M:%OS", ...)
{
  sl <- ifelse(double.slash, "\\\\", "\\")

  cleanLatex <- function(string) {
    if(! is.character(string))
      string <- as.character(string)
    
    ## Find strings not in math mode (surrounded by $)
    s <- gsub("(^[[:space:]]+)|([[:space:]]+$)", "", string)
    k <- ! (substring(s, 1, 1) == '$' & substring(s, nchar(s)) == '$')
    k <- k & ! is.na(k)
    
    if(! any(k)) return(string)

    inn <- c('< =', '> =', '<=', '>=', '<', '>',
             '\\\\%', '%', 
             '\\\\&', '&')
    out <- c('<=',
             '>=',
             paste('$', sl, sl, 'leq$', sep=''),
             paste('$', sl, sl, 'geq$', sep=''),
             paste(sl, sl, 'textless ', sep=''),
             paste(sl, sl, 'textgreater ', sep=''),
             '%', paste(sl, sl, '%', sep=''),
             '&', paste(sl, sl, '&', sep=''))
    for(i in 1 : length(inn))
      string[k] <- gsub(inn[i], out[i], string[k])
    string
  }

  if(numeric.dollar && dcolumn)
    stop('cannot have both numeric.dollar=TRUE and dcolumn=TRUE')
  
  if(missing(digits))
    digits <- NULL
  
  if((! length(digits))+(! length(dec))+(! length(rdec))+(! length(cdec)) < 3)
    stop('only one of digits, dec, rdec, cdec may be given')
  
  if(! length(digits) && ! length(dec) && ! length(rdec) && ! length(cdec)) {
    digits <- 15
  }

  if(length(digits)) {
	oldopt <- options('digits')
    options(digits=digits)
    on.exit(options(oldopt))
  }
  
  formt <- function(x, decimal.mark='.', nsmall=0,
                    scientific=c(-4,4), digits=NULL, na.blank=FALSE, ...) {
    y <- format(x, nsmall=nsmall, decimal.mark=decimal.mark,
                digits=digits, ...)
    if(decimal.mark != '.') y <- gsub('\\.', decimal.mark, y)
    if(na.blank) y <- ifelse(is.na(x), '', y)
    y
  }
  
  dot <- if(cdot && numeric.dollar)
    paste(sl,sl,'cdotp',sl,sl,'!',sep='')
  else getOption('OutDec')
  
  decimal.point <- if(cdot && dcolumn)
    paste(sl,'cdot',sep='')
  else dot

  if(is.data.frame(x)) x <- unclass(x)
  
  xtype <- if(is.list(x)) 1 else if(length(dim(x))) 2 else 3
  
  ncx  <- if(xtype == 1) length(x) else if(xtype == 2) ncol(x) else 1
  
  nams <- if(xtype == 1) names(x)  else if(xtype == 2) dimnames(x)[[2]] else ''

  if(! missing(col.just) && (length(col.just) < ncx))
    stop('col.just needs the same number of elements as number of columns')
  
  if(! length(nams)) nams <- rep('', ncx)
  
  nrx <-
    if(xtype == 1) {
      if(length(d <- dim(x[[1]]))) d[1] else length(x[[1]])
    } else if(xtype == 2) nrow(x)
    else
      length(x)
  
  rnams <- if(xtype == 1) attr(x,'row.names') else if(xtype == 2)
      dimnames(x)[[1]] else names(x)
  
  if(length(dec) + length(rdec) + length(cdec)  ==  0) rtype <- 1
  
  if(length(rdec)) {
    rtype <- 2
    dec <- matrix(rdec, nrow=nrx, ncol=ncx)
  }
  
  if(length(dec)) {
    rtype <- 3
    if(length(dec) == 1) cdec <- rep(dec, ncx)
  }
  
  if(length(cdec)) rtype <- 4
  
  cx    <- NULL
  nam   <- NULL
  cjust <- NULL
  
  if(blank.dot) sas.char <- function(x) {
    n.x <- nchar(x)
    blanks.x <-
      sapply(n.x, function(n.x.i) paste(rep(" ", n.x.i), collapse=""))
    ifelse(x == blanks.x, ".", x)
  }

  nams <- if(math.col.names) paste('$', nams, '$', sep='')
   else if(already.math.col.names) nams else cleanLatex(nams)

  rnams <- if(math.row.names) paste('$', rnams, '$', sep='')
   else if(already.math.row.names) rnams else cleanLatex(rnams)

  for(j in 1 : ncx) {
    xj <- if(xtype == 1) x[[j]] else if(xtype == 2) x[,j] else x
    
    num <- is.numeric(xj) || all(is.na(xj))
    if(testDateTime(xj)) num <- FALSE
    
    ## using xtype avoids things like as.matrix changing special characters 
    ncxj <- max(1, dim(xj)[2], na.rm=TRUE)

    for(k in 1 : ncxj) {
      xk <-
        if(ld <- length(dim(xj)) == 2) xj[, k] else xj
      
      names(xk) <- NULL
      ## gets around bug in format.default when 
      ## nsmall is given and there are NAs
      
      namk <-
        if(ld) {
          dn <- dimnames(xj)[[2]][k]
          if(length(dn) == 0)
            dn <- as.character(k)
          
          if(math.row.names) {
            paste('$', dn, '$', sep='')
          } else if(already.math.row.names) dn else cleanLatex(dn)
        } else ''
      
      namk <- paste(nams[j],
                    if(nams[j] != '' && namk != '')
                      matrix.sep
                    else '',
                    namk, sep='')
      
      if(num) {
        cj <-
          if(length(col.just))
            col.just[j]
          else 'r'

        if(rtype == 1)
          cxk <- formt(xk, decimal.mark=dot, scientific=scientific,
                       digits=digits, na.blank=na.blank, ...)
        else if(rtype == 3) {
          cxk <- character(nrx)
          for(i in 1 : nrx)
            cxk[i] <-
              if(is.na(dec[i,j]))
                formt(xk[i], decimal.mark=dot, scientific=scientific,
                      digits=digits, na.blank=na.blank, ...)
              else
                formt(round(xk[i], dec[i,j]), decimal.mark=dot,
                      digits=digits, nsmall=dec[i,j], scientific=scientific,
                      na.blank=na.blank, ...)
        } else if(rtype == 4)
          cxk <-
            if(is.na(cdec[j]))
              formt(xk, decimal.mark=dot, scientific=scientific, digits=digits,
                    na.blank=na.blank, ...)
            else
              formt(round(xk, cdec[j]), decimal.mark=dot, nsmall=cdec[j],
                    digits=digits, scientific=scientific,
                    na.blank=na.blank, ...)
        
        if(na.dot) cxk[is.na(xk)] <- '.'  # SAS-specific
        
        if(blank.dot) cxk <- sas.char(cxk)
        
        if(numeric.dollar) cxk <- paste("$",cxk,"$",sep="")
        
        ## These columns get real minus signs in LaTeX, not hyphens,
        ## but lose alignment unless their col.just="r"
        if(dcolumn | (length(col.just) && col.just[j] == 'c')) {
          cxk <- sedit(cxk, " ", "~")
          if(dcolumn)
            cj <- paste("D{.}{",decimal.point,"}{-1}",sep='')
        } 
      } else {   #ended if(num)
        cj <-
          if(length(col.just))
            col.just[j]
          else 'l'
        
        if(inherits(xk, "Date")) {
          cxk <- cleanLatex(format(xk, format=format.Date))
        } else if(inherits(xk, "POSIXt")) {
          cxk <- cleanLatex(format(xk, format=format.POSIXt))
        } else {
          cxk <- cleanLatex(xk)
        }
        if(na.blank) cxk <- ifelse(is.na(xk), '', cxk)
      }
      
      cx    <- cbind(cx, cxk)
      nam   <- c(nam, namk)
      cjust <- c(cjust, cj)
    }    # end k
  } #end j

  dimnames(cx)        <- list(rnams, nam)
  attr(cx,"col.just") <- cjust
  cx
}


##first.hline.double added FEH 11Jun95
##Usage:
##	latex(x) # for x any S object

##Value is a file object of class=c("latex","file") which is
##automatically printed by print.latex(), which constructs a file objecT
##of class=c("dvi","file"), and automatically prints it using
##print.dvi().  print.latex() returns an invisible file object.


## dcolumn numeric.dollar cdot
##
## dc cd nd  format.df latex.default  # comment
## F  F  T	   $		     # LaTeX usage
## F  T  T   \cdot! $		     # LaTeX usage
## T  F  F   . ~	      .	    dcolumn  # LaTeX usage
## T  T  F   . ~	      \cdot dcolumn  # LaTeX usage
##        
## F  F  F    			     # non-TeX (hyphens in TeX)
##        
## F  T  F   \cdot!		     # TeX errors, hyphens
## T  F  T   . ~	   $  .	    dcolumn  # TeX errors
## T  T  T   . ~	   $  \cdot dcolumn  # TeX errors
latex.default <-
  function(object,
           title=first.word(deparse(substitute(object))),
           file=paste(title, ".tex", sep=""),
           append=FALSE, label=title,
           rowlabel=title, rowlabel.just="l",
           cgroup=NULL, n.cgroup=NULL,
           rgroup=NULL, n.rgroup=NULL,
           cgroupTexCmd="bfseries",
           rgroupTexCmd="bfseries",
           rownamesTexCmd=NULL, 
           colnamesTexCmd=NULL,
           cellTexCmds=NULL,
           rowname, cgroup.just=rep("c", length(n.cgroup)),
           colheads=NULL,
           extracolheads=NULL, extracolsize='scriptsize',
           dcolumn=FALSE, numeric.dollar=! dcolumn, cdot=FALSE,
           longtable=FALSE, draft.longtable=TRUE, ctable=FALSE, booktabs=FALSE,
           table.env=TRUE, here=FALSE, lines.page=40,
           caption=NULL, caption.lot=NULL, caption.loc=c('top','bottom'),
           star=FALSE,
           double.slash=FALSE,
           vbar=FALSE, collabel.just=rep("c",nc), na.blank=TRUE,
           insert.bottom=NULL, insert.bottom.width=NULL,
           insert.top=NULL,
           first.hline.double=! (booktabs | ctable),
           where='!tbp', size=NULL,
           center=c('center','centering','centerline','none'),
           landscape=FALSE,
           multicol=TRUE, ## to remove multicolumn if no need
           math.row.names=FALSE,
           already.math.row.names=FALSE,
           math.col.names=FALSE,
           already.math.col.names=FALSE,
           hyperref=NULL, continued='continued',
           ...)
{
  if(length(hyperref)) hyperref <- sprintf('\\hyperref[%s]{', hyperref)
  center <- match.arg(center)
  caption.loc <- match.arg(caption.loc)
  cx <- format.df(object, dcolumn=dcolumn, na.blank=na.blank,
                  numeric.dollar=numeric.dollar, cdot=cdot,
                  math.row.names=math.row.names,
                  already.math.row.names=already.math.row.names,
                  math.col.names=math.col.names,
                  already.math.col.names=already.math.col.names,
                  double.slash=double.slash, ...)

  if(missing(rowname)) rowname <- dimnames(cx)[[1]]

  nocolheads <- length(colheads) == 1 && is.logical(colheads) && ! colheads
  
  if (! length(colheads)) colheads <- dimnames(cx)[[2]]

  col.just <- attr(cx, "col.just")
  nc       <- ncol(cx)
  nr       <- nrow(cx)

  if (length(cgroup)) {
    k <- length(cgroup)
    if(! length(n.cgroup))
      n.cgroup <- rep(nc / k, k)
    
    if(sum(n.cgroup) != nc)
      stop("sum of n.cgroup must equal number of columns")
    
    if(length(n.cgroup) != length(cgroup))
      stop("cgroup and n.cgroup must have same lengths")
  }

  if(! length(rowname)) rgroup <- NULL
  
  if(! length(n.rgroup) && length(rgroup))
    n.rgroup <- rep(nr / length(rgroup), length(rgroup))
  
  if(length(n.rgroup) && sum(n.rgroup) != nr)
    stop("sum of n.rgroup must equal number of rows in object")
  
  if(length(rgroup) && length(n.rgroup) && (length(rgroup) != length(n.rgroup)))
    stop("lengths of rgroup and n.rgroup must match")
  
  if (length(rgroup) && rowlabel.just == "l")
    rowname <- paste("~~", rowname, sep="")

  sl <- ifelse(double.slash, "\\\\", "\\")

  if(ctable && !booktabs) {
      eol <- paste(sl, 'NN\n', sep='')
      eog <- ""
    } else if(ctable) {
      eol <- paste(sl, 'NN\n', sep='')
      eog <- paste(sl, 'NN\n', sep='')
    } else if(longtable && length(n.rgroup)) {
      eol <- paste(sl,"tabularnewline*\n", sep='')
      eog <- paste(sl, "tabularnewline\n", sep='')
    } else {
      eol <- paste(sl,"tabularnewline\n",  sep='')
      eog <- paste(sl, "tabularnewline\n", sep='')      
  }
  
  if(booktabs) {
    toprule    <- paste(sl, "toprule\n",sep="")
    midrule    <- paste(sl, "midrule\n",sep="")
    bottomrule <- paste(sl, "bottomrule\n",sep="")
  } else if(ctable) {
    toprule    <- paste(sl, 'FL\n', sep='')
    midrule    <- paste(sl, 'ML\n', sep='')
    bottomrule <- paste(sl, 'LL\n', sep='')
  } else {
    toprule <-
      if(first.hline.double)
        paste(sl, "hline", sl, "hline\n", sep="")
      else
        paste(sl, "hline\n", sep="")
    
    midrule <- bottomrule <- paste(sl, "hline\n", sep="")
  }


  ## ################ CELL AND ROWNAMES FORMATS ###################
  ## If no formats are specified for the rownames and cells there is
  ## nothing to do. If only one is specified then the other must
  ## faked. But rownamesTexCmd should only be faked if rownames is
  ## not NULL.

  ## Check to make sure the dimensions of the cell formats
  ## match the dimensions of the object to be formatted.
  if (length(cellTexCmds) &
      ! (all(dim(cx) == dim(cellTexCmds)) &
        length(dim(cx)) == length(dim(cellTexCmds)))) {
    msg  <- "The dimensions of cellTexCmds must be:"
    msg1 <- paste(dim(cx), collapse=" x ")
    msg  <- paste(msg, msg1)
    msg  <- paste(msg, ", but you gave me: ")
    msg1 <- paste(dim(cellTexCmds), collapse=" x ")
    msg  <- paste(msg, msg1, sep="")
    stop(msg)
  }
  
  if (length(cellTexCmds) | length(rownamesTexCmd)) {
    ## LaTeX commands have been specified for either the rownames or
    ## the cells.
    ## Fake rownamesTexCmd if it is NULL and if rowname exists.
    if (! length(rownamesTexCmd) & length(rowname))
      rownamesTexCmd <- rep("", nr)
    
    ## Fake cellTexCmds if it is NULL.
    if (! length(cellTexCmds)) cellTexCmds <- array('', dim=dim(cx))
    
    ## Create a combined rowname and cell format object
    rcellTexCmds <- cbind(rownamesTexCmd, cellTexCmds)
    thisDim <- dim(rcellTexCmds)
    ## Prefix the latex commands with slashes.
    rcellTexCmds <- paste(sl, rcellTexCmds, sep="")
    ## Remove slashes from elements where no format was specified.
    rcellTexCmds[rcellTexCmds == sl] <- ""
    ## Restore the dimensions of the matrix (paste loses them).
    dim(rcellTexCmds) <- thisDim
  } else rcellTexCmds <- NULL

  

################ END OF CELL AND ROWNAMES FORMATS ###############
  
  
  if (length(cgroup)) {
    last.col    <- cumsum(n.cgroup)
    first.col   <- c(1, 1 + last.col[- length(last.col)])
    cgroup.cols <- cbind(first.col,last.col)
    col.subs    <- split(seq(length.out=nc),
                         rep.int(seq_along(n.cgroup), times=n.cgroup))
    
    cxi <- rctci <- list()
    ## Initialize with row name column and first column group:
    rctcx <- if(length(rcellTexCmds)) rcellTexCmds[, 1]
#    rctci <- if(length(rcellTexCmds))
#               list(cbind(rcellTexCmds[, 1], rcellTexCmds[1 + col.subs[[1]]
    for (i in seq(along=col.subs)) {
      cxi[[i]] <- cx[, col.subs[[i]], drop=FALSE]
      if(length(rctcx))
        rctcx <- cbind(rctcx, rcellTexCmds[, 1 + col.subs[[i]], drop=FALSE],
                       if(i < length(col.subs)) '')
    }
    if(length(rctcx)) rcellTexCmds <- rctcx
    
    cxx             <- cxi[[1]]
    col.justxx      <- col.just[col.subs[[1]]]
    collabel.justxx <- collabel.just[col.subs[[1]]]
    colheadsxx      <- colheads[col.subs[[1]]]
    extracolheadsxx <- extracolheads[col.subs[[1]]]

    cgroupxx   <- cgroup[1]
    n.cgroupxx <- n.cgroup[1]
    for(i in seq(along=col.subs)[-1]) {
      cxx <- cbind(cxx, "", cxi[[i]])
      col.justxx <- c(col.justxx, "c", col.just[col.subs[[i]]])
      collabel.justxx <- c(collabel.justxx, "c",
                           collabel.just[col.subs[[i]]])
      cgroupxx   <- c(cgroupxx,   "", cgroup[i])
      n.cgroupxx <- c(n.cgroupxx,  1, n.cgroup[i])
      colheadsxx <- c(colheadsxx, "", colheads[col.subs[[i]]])
      if(length(extracolheads))
        extracolheadsxx <- c(extracolheadsxx, "",
                             extracolheads[col.subs[[i]]])
    }
    
    cgroup.colsxx <- cgroup.cols + 0 : (nrow(cgroup.cols) - 1)

    cx            <- cxx
    col.just      <- col.justxx
    collabel.just <- collabel.justxx
    n.cgroup      <- n.cgroupxx
    cgroup.cols   <- cgroup.colsxx[cgroup != "", , drop=FALSE]
    cgroup        <- cgroupxx
    colheads      <- colheadsxx
    extracolheads <- extracolheadsxx
    nc <- ncol(cx)
  }

  cline <- NULL
  if (length(rowname)) {
    cx       <- cbind(rowname, cx)
    col.just <- c(rowlabel.just, col.just)

    if(length(extracolheads)) extracolheads <- c('', extracolheads)
    
    collabel.just <- c(rowlabel.just, collabel.just)
    if (length(cgroup) == 0L)
      colheads <- c(rowlabel, colheads)
    else {
      colheads <- c('', colheads)
      cgroup   <- c(rowlabel, cgroup)

      rlj <- ifelse(rowlabel.just == "l", "l", "c")
      cgroup.just <- c(rlj, cgroup.just)
      n.cgroup <- c(1, n.cgroup)
      cgroup.cols <- 1+cgroup.cols
      cline <- paste(sl, "cline{", cgroup.cols[,1],"-", cgroup.cols[,2], "}",
                     sep="", collapse=" ")
    }
    
    nc <- 1 + nc
  } else if(length(cgroup) > 0L) {
      cline <- paste0(sl, "cline{", cgroup.cols[,1], "-", cgroup.cols[,2], "}", collapse=" ")
  }

  vbar <- ifelse(vbar, "|", "")

  if(! append) cat("", file=file)	#start new file

  ## pandoc used by R Markdown gets fooled by LaTeX comments
  olc <- getOption('omitlatexcom')
  if(length(olc) && olc)
    cat("%", deparse(sys.call()), "%\n", file=file, append=file != '', sep='')

  if(dcolumn) {
    decimal.point <- ifelse(cdot, paste(sl, "cdot", sep=""), ".")
    cat(sl,"newcolumntype{.}{D{.}{",decimal.point,"}{-1}}\n",
        sep="", file=file, append=file != '')
  }

  { # tabular.cols
    tabular.cols <- paste(vbar, col.just, sep="")
    if (! length(n.cgroup))
      tabular.cols <- c(tabular.cols, vbar)
    else {
      vv2 <- cumsum(n.cgroup)
      tabular.cols[vv2] <- paste(tabular.cols[vv2],vbar,sep="")
    }
    
    tabular.cols <- paste(tabular.cols, collapse="")
  }

  intop <- function() {
    if(! length(insert.top)) return(NULL)
    paste(if(center == 'none') '\n\\vspace{1ex}\n\n',
          paste('\\textbf{', insert.top, '}', sep=''),
 #         if(center %in% c('centerline', 'centering')) '\\\\',
          if(center != 'center') '\n\\vspace{1ex}\n\n', sep='')
  }
  
  if(length(caption) && ! ctable) {
    caption <- paste(sl, "caption",
                     if(length(caption.lot))
                       paste("[", caption.lot, "]", sep=""),
                     "{", caption,
                     if(! longtable)
                       paste(sl, "label{", label, "}", sep=""),
                     "}", sep="")
    
    table.env <- TRUE
  }

  if(ctable) {
    latex.begin <-
      latexBuild(
        if(length(size)) paste('{', sl, size, sep=''), '{',
        intop(), '',
        paste(sl, 'ctable[', sep=''), '',
        if(length(caption) && caption.loc == 'bottom') 'botcap,', '',
        if(length(caption)) paste('caption={', caption, '},', sep=''),
         '',
        if(length(caption.lot)) paste('cap={', caption.lot, '},',
                                      sep=''), '',
        if(length(caption)) paste('label=', label, ',', sep=''), '',
        if (star) 'star, ', '',
        if(! landscape) paste('pos=', where, ',', sep=''), '',
        if(landscape) 'sideways', '',
        paste(']{', tabular.cols, '}', sep=''), '',
        if(length(insert.bottom)) 
         paste('{',
              paste(sl,'tnote[]{',
                    sedit(insert.bottom,'\\\\',' '),'}',
                    sep='', collapse=''),
              '}',
              sep=''), '',
        if(! length(insert.bottom)) '{}', '',
        ## tnote does not allow \\ in its argument
        paste('{', toprule, sep=''), '{')
    
    latex.end <- attr(latex.begin, 'close')
    
  } else if(! longtable) {
    latex.begin <-
      latexBuild(
        if(landscape) paste(sl, "begin{landscape}", sep=""), 'landscape',
        if(table.env) paste(sl, "begin{table}", if(here) "[H]"
         else paste('[', where, ']', sep=''), "\n", sep=""), 'table',
        if(length(size)) paste('{', sl, size, '\n', sep=''), '{',
        if(caption.loc == 'top' && length(caption)) paste(caption, "\n"), '',
        intop(), '',
        if(center == 'center') paste(sl, "begin{center}\n", sep=""), 'center',
        if(center == 'centering') paste('{', sl, 'centering\n', sep=''), '{',
        if(center == 'centerline') paste(sl, 'centerline{', sep=''),'{',
        hyperref, '{',
        paste(sl, "begin{tabular}{", tabular.cols, "}\n", toprule, sep=""),
         'tabular',
        insert=list(if(! table.env && length(insert.bottom))
                      list('tabular', 'after', paste('\\par', insert.bottom)),
                    if(table.env)
                      list('table',   'before', paste(insert.bottom, collapse = ' ')),
                    if(caption.loc == 'bottom' && length(caption))
                      list('tabular', 'after', caption)
                   ) )
    
    latex.end <- attr(latex.begin, 'close')

  } else {           ## longtable, not ctable
    latex.begin <-
      latexBuild(
        if(! draft.longtable) 
          paste(sl,"let",sl,"LTmulticolumn=",sl,"multicolumn", sep=""),
          '',
        paste(sl, "setlongtables", sep=""), '',
        if(landscape) paste(sl, "begin{landscape}",sep=""), 'landscape',
        if(length(size)) paste('{', sl, size, '\n', sep=''), '{',
        intop(), '',
        paste(sl,"begin{longtable}{", tabular.cols, "}", sep=""),
          'longtable',
        if(caption.loc == 'top' && length(caption)) paste(caption, eog),
          '',
        toprule, '',
        insert=list(
          if(caption.loc == 'bottom' && length(caption))
           list('longtable', 'after', caption) ) )
    
    latex.end <- attr(latex.begin, 'close')
    if(! length(caption))
      latex.end <- paste(latex.end, '\\addtocounter{table}{-1}', sep='\n')
  }
  cat(latex.begin, file=file, append=file != '')
  
  cgroupheader <- NULL
  if(length(cgroup)) {
    cvbar <- paste(cgroup.just, vbar, sep="")
    cvbar[1] <- paste(vbar, cvbar[1], sep="")
    cvbar[-length(cvbar)] <- paste(cvbar[-length(cvbar)], vbar, sep="")
    slmc <- paste(sl, "multicolumn{", sep="")
    if (length(cgroupTexCmd))
      labs <- paste(sl, cgroupTexCmd, " ", cgroup, sep="")
    else
      labs <- cgroup
    
    if(multicol)
      labs <- paste(slmc, n.cgroup, "}{", cvbar, "}{", labs, "}", sep="")

    cgroupheader <- paste(labs, collapse="&")
    
    if (! length(cline)) {
      inr <- as.numeric(length(rowname))
      cline <- paste(sl, "cline{", 1 + inr, "-", nc, "}", sep="")
    }

    cgroupheader <- paste(cgroupheader, eol, cline, "\n", sep="")
    cat(cgroupheader, file=file, append=file != '')
  }


  { # column labels
    cvbar <- paste(collabel.just, vbar, sep="")
    cvbar[1] <- paste(vbar, cvbar[1], sep="")
    if (length(n.cgroup)) {
      vv2 <- cumsum(n.cgroup[-length(n.cgroup)])
      cvbar[vv2] <- paste(cvbar[vv2], vbar, sep="")
    }
    slmc1 <- paste(sl, "multicolumn{1}{", sep="")

    labs <- colheads
    if (length(colnamesTexCmd))
      labs <- paste(sl, colnamesTexCmd, " ", labs, sep="")
    if(nocolheads) colheads <- labs <- NULL
    header <- NULL
    if(length(labs)) {
      if(! length(extracolheads)) {
        heads <- get2rowHeads(labs)
        colheads <- heads[[1]]
        if(any(heads[[2]] != ''))
          extracolheads <- heads[[2]]
      }
      
      if(multicol)
        colheads <- paste(slmc1, cvbar, "}{", colheads, "}", sep="")
      
      header <- if(length(colheads)) paste(colheads, collapse='&')
      if(length(extracolheads)) {
        extracolheads <- ifelse(extracolheads == ''| extracolsize == '',
                                extracolheads,
                                paste('{',sl,extracolsize,' ',
                                      extracolheads,'}',sep=''))
        
        if(multicol)
          extracolheads <- ifelse(extracolheads == '',extracolheads,
                                  paste(slmc1,cvbar,'}{',extracolheads,'}',sep=''))
        else
          extracolheads <- ifelse(extracolheads == '',extracolheads,
                                  paste(extracolheads,sep=''))
        
        header <- if(length(header))
          paste(header, eol, paste(extracolheads, collapse='&'), sep='')
      }
    
      if(length(header)) cat(header, eog, file=file, sep='', append=file != '')

      if(ctable)
        cat(midrule, file=file, append=file != '')
      else
        cat(midrule, file=file, append=file != '')
    }
  }

  if(longtable) {
    if(! length(caption))
      cat(sl,"endhead\n",midrule,sl,"endfoot\n",sep="",
          file=file,append=file != '')
    else {
      cat(sl,"endfirsthead", sep="",file=file, append=file != '')
      cat(sl,"caption[]{\\em (", continued, ")} ", eol,
          sep="",file=file, append=file != '')
      cat(midrule, sep="",file=file, append=file != '')
      if(length(cgroupheader))
        cat(cgroupheader, file=file, append=file != '')
      if(length(header)) cat(header, file=file, sep="&", append=file != '')
      cat(eog, midrule, sl, "endhead", '\n', midrule,
          sep="", file=file, append=file != '')
      if(length(insert.bottom)) {
        if(length(insert.bottom.width) == 0) {
            insert.bottom.width = paste0(sl, "linewidth")
        }
        
        cat(paste(sl, 'multicolumn{', nc, '}{', "p{",insert.bottom.width,'}}{', 
                  insert.bottom, '}', eol, sep='', collapse='\n'),
                  sep="", file=file, append=file != '')
      }
    
      cat(sl,"endfoot\n", sep="",file=file, append=file != '')
      cat(sl,"label{", label, "}\n", sep="", file=file, append=file != '')
    }
  }

  { # individual lines, grouped if appropriate, longtable if appropriate
    if (length(n.rgroup)) {
      rg.end   <- cumsum(n.rgroup)
      rg.start <- rg.end-n.rgroup+1
      if(! length(rgroup)) {
        rgroup <- rep("",length(n.rgroup))
      } else {
        if (length(rgroupTexCmd)) {
          rgroup <- paste("{",sl, rgroupTexCmd, " ", rgroup,"}",sep="") 
        } else rgroup <- paste("{", rgroup,"}",sep="") 
      }
      
      seq.rgroup <- seq(along=n.rgroup)
    } else {
      seq.rgroup <- 1
      rg.end <- nr
      rg.start <- 1
    }

    linecnt <- 0
    for (j in seq.rgroup) {
      if (length(n.rgroup)) {
        if(longtable && linecnt > 0 &&
           (linecnt + n.rgroup[j] + (n.rgroup[j] > 1)) > lines.page) {
          cat(sl, "newpage\n", sep="", file=file, append=file != '')
          linecnt <- 0
        }
        
        cat(rgroup[j], rep("", nc - 1), sep="&", file=file, append=file != '')
        cat(eol, sep="",file=file, append=file != '')
        linecnt <- linecnt + 1
      }

      ## Write the object (and it's formatting instructions)
      ## to the output.
      ## Loop through the rows of the object.
      for(i in rg.start[j] : rg.end[j]) {
        if (! length(n.rgroup)) {
          if(longtable && linecnt > 0 && (linecnt + 1 > lines.page)) {
            cat(sl, "newpage\n", sep="", file=file, append=file != '')
            linecnt <- 0						
          }
        }

        ## Loop through the columns of the object
        ## write each value (and it's format if there
        ## is one)
        
        if (length(rcellTexCmds)) {
          num.cols <- ncol(cx)
          for (colNum in 1 : num.cols) {
            cat(rcellTexCmds[i, colNum], " ", cx[i, colNum],
                file=file, append=file != '')
            if (colNum < num.cols)
              cat("&", file=file, append=file != '')
          }
        } else {
          ## Original code that writes object to output.
          cat(cx[i, ], file=file, sep="&", append=file != '')
        }
        
        cat(if(i == rg.end[j] || (! ctable && ! length(n.rgroup)))
              eog
            else if(i < rg.end[j])
              eol,
            sep="", file=file, append=file != '')
        
        linecnt <- linecnt+1
      }  ## End of for loop that writes the object.

      if(length(n.rgroup) > j)
        cat(midrule, sep = "", file=file, append=file != '')
      else
        cat(bottomrule, sep="",file=file, append=file != '')
    }
  }

  cat(latex.end, file=file, sep='\n', append=file != '')

  sty <- c("longtable"[longtable], "here"[here], "dcolumn"[dcolumn],
           "ctable"[ctable], "booktabs"[booktabs],
           if(landscape && ! ctable) "lscape")
  
  structure(list(file=file, style=sty), class='latex')
}


## Re-written by Daniel Calvelo Aros <dcalvelo@minag.gob.pe> to not use
## S.sty  18Feb04
latex.function <- function(object,
                           title=first.word(deparse(substitute(object))),
                           file=paste(title, ".tex", sep=""),
                           append=FALSE, assignment=TRUE,
                           type=c('example','verbatim','Sinput'),
                           width.cutoff=70, size='', ...)
{
  type <- match.arg(type)
  fctxt <- deparse(object, width.cutoff=width.cutoff)
  if(assignment) fctxt[1] <- paste(title , '<-', fctxt[1]) 
  environment <- ifelse(type == 'example', "alltt", "verbatim")
  environment <- c(example='alltt', verbatim='verbatim',
                   Sinput=paste('Sinput',size,sep=''))[type]
  preamble <- paste("\\begin{",environment,"}\n",sep="")
  cat(preamble, file=file, append=file != "")

  if(type == 'Sinput') cat(fctxt, sep='\n')
  else {
    rxs <-
      if(type == 'example')
        c("\t=>    ",
          "\\\\=>\\\\(\\\\backslash\\\\)",
          "([{}])=>\\\\\\1",
          "<-=>\\\\(\\\\leftarrow\\\\)",
          "#(.*?$)=>{\\\\rm\\\\scriptsize\\\\#\\1}"
          )
      else c("\t=>    ")
    
    substitute <- strsplit( rxs, "=>" )
    for(line in fctxt) {
      for( subst in substitute ) {
        line <- gsub( subst[1], subst[2], line, perl=TRUE )
      }
      
      line <- paste(line,"\n",sep="")
      cat(line, file=file, append=file != "")
    }
  }
  
  postamble <- paste("\\end{",environment,"}\n", sep="")
  cat(postamble, file=file, append=file != '')
  
  structure(list(file=file, style=if(type == 'example')'alltt'), class='latex')
}

latexVerbatim <- function(x,
                          title=first.word(deparse(substitute(x))),
                          file=paste(title, ".tex", sep=""),
                          append=FALSE, size=NULL, hspace=NULL,
                          width=.Options$width,
                          length=.Options$length, ...)
{
  if(! missing(width) || ! missing(length)) {
	old <- options('width', 'length')
    options(width=width, length=length)
    on.exit(options(old))
  }

  if(file != '') sink(file, append=append)
  cat('\\setbox0=\\vbox{\n',
      if(length(size))
        c('\\',size,'\n'),
      '\\begin{verbatim}\n', sep='')
  
  print(x, ...)
  cat('\\end{verbatim}\n}\n',
      if(length(hspace))
        c('\\hspace{',hspace,'}'),
      '{\\makebox[\\textwidth]{\\box0}}\n', sep='')

  if(file == '') return(invisible())
  sink()
  structure(list(file=file, style=NULL), class='latex')
}

latex.list <- function(object,
                       title=first.word(deparse(substitute(object))),
                       file=paste(title, ".tex", sep=""), append=FALSE,
                       label,
                       caption, caption.lot,
                       caption.loc=c('top','bottom'),
                       ...)
{
  caption.loc <- match.arg(caption.loc)
  nx <-	names(object)
  if (! length(nx))
    nx <- paste(title, "[[", seq(along=object), "]]", sep="")
  
  tmp <- latex(object=object[[1]],
               caption=nx[1], label=nx[1], append=append, title=title,
               file=file, caption.lot=NULL,
               caption.loc=caption.loc, ...)
  
  tmp.sty <- tmp$style
  for (i in seq(along=object)[-1]) {
    tmp <- latex(object=object[[i]],
                 caption=nx[i], label=nx[i], append=file != '', title=title, file=file,
                 caption.lot=NULL, caption.loc=caption.loc, ...)
    
    tmp.sty <- c(tmp.sty, tmp$style)
  }
  
  sty <-
    if(length(tmp.sty))
      unique(tmp.sty)
    else
      NULL
  
  structure(list(file=file, style=sty), class='latex')
}


## Function to translate several expressions to LaTeX form, many of
## which require to be put in math mode.
## Arguments inn and out specify additional input and translated
## strings over the usual defaults.
## If pb=T, also translates [()] to math mode using \left, \right
## Assumes that input text always has matches, e.g. [) [] (] (), and
## that surrounding  by $$ is OK
## latexTranslate is used primarily by summary.formula
latexTranslate <- function(object, inn=NULL, out=NULL, pb=FALSE,
                           greek=FALSE, na='', ...)
{
  text <- ifelse(is.na(object), na, as.character(object))
  
  inn <- c("|",  "%",  "#",   "<=",     "<",  ">=",     ">",  "_", "\\243",
           "&", inn, 
           if(pb)
             c("[", "(", "]", ")"))

  out <- c("$|$", "\\%", "\\#", "$\\leq$", "$<$", "$\\geq$", "$>$", "\\_",
           "\\pounds", "\\&", out, 
           if(pb)
             c("$\\left[", "$\\left(", "\\right]$", "\\right)$"))

  text <- sedit(text, '$', 'DOLLARS', wild.literal=TRUE)
  text <- sedit(text, inn, out)

  ##See if string contains an ^ - superscript followed by a number
  ## (number condition added 31aug02)

  dig <- c('0','1','2','3','4','5','6','7','8','9')

  for(i in seq_along(text)) {
    lt <- nchar(text[i])
    x <- substring(text[i], 1 : lt, 1 : lt)
    j <- x == '^'
    if(any(j)) {
      is <- ((1 : lt)[j])[1]  #get first ^
      remain <- x[-(1 : is)]
      k <- remain %in% c(' ',',',')',']','\\','$')
      if(remain[1] %in% dig ||
         (length(remain) > 1 && remain[1] == '-' && remain[2] %in% dig))
        k[-1] <- k[-1] | remain[-1] %nin% dig
      
      ie <- if(any(k)) is + ((1 : length(remain))[k])[1]
        else
          length(x)+1
      
      ##See if math mode already turned on (odd number of $ to left of ^)
      dol <- if(sum(x[1 : is] == '$') %% 2) ''
        else '$'
      
      substring2(text[i],is,ie-1) <-
        paste(dol, '^{', substring(text[i], is + 1, ie - 1), '}', dol,sep='')
    }
    
    if(greek) {
      gl <- c('alpha','beta','gamma','delta','epsilon','varepsilon','zeta',
              'eta','theta','vartheta','iota','kappa','lambda','mu','nu',
              'xi','pi','varpi','rho','varrho','sigma','varsigma','tau',
              'upsilon','phi','carphi','chi','psi','omega','Gamma','Delta',
              'Theta','Lambda','Xi','Pi','Sigma','Upsilon','Phi','Psi','Omega')
      for(w in gl)
        text[i] <- gsub(paste('\\b', w, '\\b', sep=''),
                        paste('$\\\\',w,'$',   sep=''),
                        text[i])
    }
  }
  
  sedit(text, 'DOLLARS', '\\$', wild.literal=TRUE)
}


latex <- function(object, ...)
{
  ## added title= 25May01
  if (! length(class(object)))
    class(object) <- data.class(object)
  
  UseMethod("latex")
}


optionsCmds <- function(pgm)
{
  optionName <- paste(pgm, 'cmd', sep='')
  v <- .Options[[optionName]]
  if(pgm == 'xdvi' && .Platform$OS.type != 'unix' && ! length(v))
    v <- 'yap'  # MikTeX
  if(length(v) && v != '') pgm <- v
  pgm
}


## From Rich Heiberger 2014-12-04:
## The original function in Hmisc_3.14-5 doesn't work on Windows.
## system doesn't handle DOS internal commands such as 'cd'
## I switched it to 'shell' on Windows.

## This revision works on Windows and Macintosh without setting options.
## On Windows yap displays the dvi file and gives a warning I don't understand
## on Mac X displays the dvi file.

## For pdflatex, we need options
## Windows and Macintosh
## options(latexcmd='pdflatex')
## options(dviExtension='pdf')

## Windows with pdflatex
## options(xdvicmd='c:\\progra~1\\Adobe\\Reader~1.0\\Reader\\AcroRd32.exe') ## 32-bit
## options(xdvicmd='c:\\progra~2\\Adobe\\Reader~1.0\\Reader\\AcroRd32.exe') ## 64 bit windows
## Adobe opens correctly and displays the file, but it also gives a warning that
## I don't understand.

## Macintosh with pdflatex
## options(xdvicmd='open')

dvi.latex <- function(object, prlog=FALSE,
                      nomargins=TRUE, width=5.5, height=7, ...)
{
  fi <- object$file;
  sty <- object$style

  if(length(sty))
    sty <- paste('\\usepackage{',sty,'}',sep='')
  
  if(nomargins)
    sty <-  c(sty,
              paste('\\usepackage[paperwidth=',width,
                    'in,paperheight=', height,
                    'in,noheadfoot,margin=0in]{geometry}',sep=''))
  
  ## pre <- tempfile(); post <- tempfile()  # 1dec03
  tmp <- tempfile()
  tmptex <- paste(tmp, 'tex', sep='.')
  infi <- readLines(fi, n=-1)       # Splus 7 doesn't default to read to EOF 3may05
  cat('\\documentclass{report}', sty,
      '\\begin{document}\\pagestyle{empty}', infi,
      '\\end{document}\n', file=tmptex, sep='\n')
  
  if (.Platform$OS.type == "unix")
    sys(paste("cd", shQuote(tempdir()), "&&", optionsCmds("latex"), 
              "-interaction=scrollmode", shQuote(tmp)), output = FALSE)
  else ## MS DOS
    shell(paste("cd", shQuote(tempdir()), "&", optionsCmds("latex"), 
                "-interaction=scrollmode", shQuote(tmp)), shell="CMD",
          intern = FALSE)

  
  if(prlog)
    cat(scan(paste(tmp,'log',sep='.'),list(''),sep='\n')[[1]],
        sep='\n')
  
  fi <- paste(tmp, getOption("dviExtension", "dvi"), sep='.')
  structure(list(file=fi), class='dvi')
}


show.dvi <- function(object, width=5.5, height=7)
{
  viewer <- optionsCmds('xdvi')
  cmd <-
    if(viewer == 'yap') {
      paste(viewer, object$file)
    }
    else if(viewer == 'kdvi') {
      paste(viewer, object$file)
    }
    else if(viewer == 'xdvi') {
      paste(viewer, ' -paper ',
            width, 'x', height, 'in -s 0 ',
            object$file, sep='')
    } else {
      paste(viewer, object$file)
    }

  
  system(cmd, intern = TRUE, wait=TRUE)
  invisible(NULL)
}


## enhanced show.latex 22dec02 - special treatment of file==''
show.latex <- function(object)
{
  if(object$file == '') {
    if(length(object$style)) {
      environment(show.latex)$latexStyles <-
        if(exists("latexStyles", envir=environment(show.latex)))
          unique(c(environment(show.latex)$latexStyles, object$style))
        else object$style

    }

    return(invisible())
  }
  
  show.dvi(dvi.latex(object))
}
environment(show.latex) <- new.env()

print.dvi <- function(x, ...) show.dvi(x)
print.latex <- function(x, ...) show.latex(x)
  
dvi         <- function(object, ...) UseMethod('dvi')
dvips       <- function(object, ...) UseMethod('dvips')
dvigv       <- function(object, ...) UseMethod('dvigv')
dvips.dvi   <- function(object, file, ...)
{
  cmd <-
    if(missing(file))
      paste(optionsCmds('dvips'), shQuote(object$file))
    else
      paste(optionsCmds('dvips'),'-o', file, shQuote(object$file))
  
  ## paste(optionsCmds('dvips'),'-f', object$file,' | lpr') else 5dec03
  ## 2 dQuote 26jan04
  invisible(sys(cmd))
}

dvigv.dvi   <- function(object, ...)
  invisible(sys(paste(optionsCmds('dvips'), '-f', object$file,
                      '| gv - &')))

## added ... to dvixx.dvi calls below 1dec03
dvips.latex <- function(object, ...) invisible(dvips.dvi(dvi.latex(object),...))
dvigv.latex <- function(object, ...) invisible(dvigv.dvi(dvi.latex(object),...))


latexSN <- function(x) {
  x <- format(x)
  x <- sedit(x, c('e+00','e-0*',
                  'e-*',
                  'e+0*',
                  'e+*'),
             c('',
               '\\!\\times\\!10^{-*}','\\!\\times\\!10^{-*}',
               '\\!\\times\\!10^{*}','\\!\\times\\!10^{*}'))
  x
}

htmlSN <- function(x, pretty=TRUE, ...) {
  x <- if(pretty) prettyNum(x, ...) else format(x, ...)
  times <- htmlSpecial('times')
  x <- gsub('e\\+00', '', x)
  x <- gsub('e\\+0([0-9])', '\u00D710<sup>\\1</sup>', x)
  x <- gsub('e\\+(.*)', '\u00D710<sup>\\1</sup>', x)
  x <- gsub('e-0([0-9])', '\u00D710<sup>-\\1</sup>', x)
  gsub('e-(.*)', '\u00D710<sup>-\\1</sup>', x)
}
