##!!WRONG ARG x in !.SV4. def latex generic!
#Changed x to object inside latex() for !.SV4. (Thanks David Lovell)

#Thanks to David R. Lovell <David.Lovell@cmis.csiro.au> CSIRO
#for scientific=    8Feb2000

first.word <- function(x, i=1, expr=substitute(x)) {
  words <- if(!missing(x)) as.character(x)[1] else
    as.character(unlist(expr))[1]
  ## Added !missing(x) as.char(x) 25May01
#	first.letters <- substring(words, 1, 1)
#	word.selector <- (match(first.letters, c(letters,LETTERS,"."), 0) > 0)
#	words <- words[word.selector][i]
#	if(!under.unix) {
#	  words <- sedit(words,'.','')
#	  words <- substring(words,1,8)
#	}
    ##18Nov00 FEH:
    if(i > 1) stop('i > 1 not implemented')
    chars <- substring(words, 1:nchar(words), 1:nchar(words))
    legal.chars <- c(letters,LETTERS,'.',
                     '0','1','2','3','4','5','6','7','8','9')
    non.legal.chars <- (1:length(chars))[chars %nin% legal.chars]
    if(!any(non.legal.chars)) return(words)
    if(non.legal.chars[1]==1) return(character(0))
    substring(words, 1, non.legal.chars[1]-1)
}

#1. if x is a data.frame, then do each component separately.
#2. if x is a matrix, but not a data.frame, make it a data.frame
#   with individual components for the columns.
#3. if a component x$x is a matrix, then do all columns the same.
#4. Use right justify by default for numeric columns.
#5. Use left justify for non-numeric columns.

# The following are made complicated by matrix components of data.frames:
#6. vector cdec must have number of items equal to number of columns
#   of input x.
#7. matrix dec must have number of columns equal to number of columns
#   of input x.
#8. scalar dec is expanded to a vector cdec with number of items equal
#   to number of columns of input x.
#9. vector rdec must have number of items equal to number of rows of input x.
#   rdec is expanded to matrix dec.
#10. col.just must have number of columns equal to number of columns
#    of output cx.

# Value:
# character matrix with character images of properly rounded x.
# matrix components of input x are now just sets of columns of character matrix.
# attr(,col.just) repeats input col.just when provided.
#	Otherwise, recommended justification for columns of output.
#	Default is "l" for characters and factors, "r" for numeric.
#	When dcolumn==T, numerics will have ".".


#FEH 21May96 - changed default for numeric.dollar to cdot
#FEH  5Jun96 - re-written to not rely on as.data.frame,
#              converted data frames to matrices the slow way
#              added matrix.sep 
#    12Aug99 - allowed # decimal places=NA (no rounding, just use format())
#    27May02 - added booktabs FEH
##   13Dec02 - added ctable   FEH
## arguments included check.names=TRUE 23jan03
format.df <- function(x,
	digits, dec=NULL, rdec=NULL, cdec=NULL, numeric.dollar=cdot,
	na.blank=FALSE, na.dot=FALSE, blank.dot=FALSE,
	col.just=NULL,
    cdot=FALSE, dcolumn=FALSE, matrix.sep=' ', scientific=c(-4,4), ...)
{

if(cdot && dcolumn) stop('cannot have both cdot=T and dcolumn=T')

if(missing(digits)) digits <- NULL
if((!length(digits))+(!length(dec))+(!length(rdec))+(!length(cdec)) < 3)
  stop('only one of digits, dec, rdec, cdec may be given')
# if(length(digits)) .Options$digits    6Aug00 what was that?
if(length(digits)) {
  oldopt <- options(digits=digits)
  on.exit(options(oldopt))
}

## For now nsmall and scientific are ignored in R  25May01
formt <- if(!.R.) format.default else
 function(x, decimal.mark='.', nsmall=0, scientific=c(-4,4)) {
   x <- format(x)
   if(decimal.mark!='.') x <- gsub('\\.',decimal.mark,x)
   x
 }
  
dot <- if(cdot) (if(.R.)'\\\\cdotp\\\\!' else '\\cdotp\\!') else '.'

if(is.data.frame(x)) x <- unclass(x)
xtype <- if(is.list(x)) 1 else if(length(dim(x))) 2 else 3  
#Following changed as above 10Mar01
#atx <- attributes(x)
#cl <- atx$class
#if(length(cl) && (idf <- any(cl=='data.frame'))) 
#  attr(x,'class') <- cl[cl!='data.frame']
#xtype <- if(is.list(x))1 else if(length(atx$dim))2 else 3
ncx <- if(xtype==1) length(x) else if(xtype==2)ncol(x) else 1
nams <- if(xtype==1) names(x) else if(xtype==2)dimnames(x)[[2]] else ''
# Added Check to see that if the user passed col.just into format.df
# that the length of col.just if >= ncx 29apr05
if(!missing(col.just) && (length(col.just) < ncx)) {
    stop('col.just needs the same number of elements as number of columns')
}
if(!length(nams)) nams <- rep('', ncx)  ## 19apr03
nrx <- if(xtype==1) {
  if(length(d <- dim(x[[1]]))) d[1] else length(x[[1]])
} else if(xtype==2) nrow(x) else length(x)

rnam <- if(xtype==1) attr(x,'row.names') else
  if(xtype==2)dimnames(x)[[1]] else names(x)

if(length(dec)+length(rdec)+length(cdec)==0) rtype <- 1
if(length(rdec)) {
  rtype <- 2
  dec <- matrix(rdec, nrow=nrx, ncol=ncx)
}
if(length(dec)) {
  rtype <- 3
  if(length(dec)==1) cdec <- rep(dec, ncx)
}
if(length(cdec)) rtype <- 4

cx <- NULL
nam <- NULL
cjust <- NULL

if(blank.dot) sas.char <- function(x) {
	n.x <- nchar(x)
	blanks.x <-
	sapply(n.x, function(n.x.i) paste(rep(" ", n.x.i), collapse=""))
	ifelse(x == blanks.x, ".", x)
}

for(j in 1:ncx) {
  xj <- if(xtype==1) x[[j]] else if(xtype==2) x[,j] else x
  namj <- nams[j]
  num <- is.numeric(xj) || all(is.na(xj)) ## 16sep03
  if(testDateTime(xj)) num <- FALSE            ## 16sep03
 #using xtype avoids things like as.matrix changing special characters 
  ncxj <- max(1,dim(xj)[2], na.rm=TRUE)
  ## Added na.rm=T 5Jan01: SV4 makes dim(xj)=single number if x is data.frame

  for(k in 1:ncxj) {
    xk <- if(ld <- length(dim(xj))==2)xj[,k] else xj
    ## Added ==2 5Jan01
    names(xk) <- NULL   # gets around bug in format.default when 
                        # nsmall is given and there are NAs
    namk <- if(ld) {
      dn <- dimnames(xj)[[2]][k]
      if(length(dn)==0) dn <- as.character(k)
      dn
    } else ''
    namk <- paste(namj, if(namj!='' && namk!='')matrix.sep else '', namk,
                  sep='')
    if(num) {
      cj <- if(length(col.just)) col.just[j] else 'r'
      if(rtype==1) cxk <- formt(xk, decimal.mark=dot, scientific=scientific)
      else if(rtype==3) {
		cxk <- character(nrx)  ## corrected 4Nov97 Eric Bissonette
		for(i in 1:nrx) cxk[i] <-
          if(is.na(dec[i,j])) formt(xk[i], decimal.mark=dot,
                                     scientific=scientific) else
        formt(round(xk[i], dec[i,j]), decimal.mark=dot,
               nsmall=dec[i,j], scientific=scientific)
        ## 12Aug99
	  }
      else if(rtype==4)  # 12Aug99
        cxk <- if(is.na(cdec[j])) formt(xk, decimal.mark=dot,
                                         scientific=scientific) else 
        formt(round(xk, cdec[j]), decimal.mark=dot, nsmall=cdec[j],
               scientific=scientific)
      if(na.blank) cxk[is.na(xk)] <- ''
      if(na.dot) cxk[is.na(xk)] <- '.'  # SAS-specific
      if(blank.dot) cxk <- sas.char(cxk)
      if(numeric.dollar) cxk <- paste("$",cxk,"$",sep="")
# These columns get real minus signs in LaTeX, not hyphens,
# but lose alignment unless their col.just="r"
      if(dcolumn | (length(col.just) && col.just[j]=='c')) {
        cxk <- sedit(cxk, " ", "~")
        if(dcolumn) cj <- "."
      } 
    } else {   #ended if(num)
      cj <- if(length(col.just)) col.just[j] else 'l'
      cxk <- as.character(xk)
    }         
  cx <- cbind(cx, cxk)
  nam <- c(nam, namk)
  cjust <- c(cjust, cj)
  }    #end for k
}              #end for j

dimnames(cx) <- list(rnam, nam)
attr(cx,"col.just") <- cjust
cx
}

#first.hline.double added FEH 11Jun95
#Usage:
#	latex(x) # for x any S object

#Value is a file object of class=c("latex","file") which is
#automatically printed by print.latex(), which constructs a file object
#of class=c("dvi","file"), and automatically prints it using
#print.dvi().  print.latex() returns an invisible file object.


# dcolumn numeric.dollar cdot
#
# dc cd nd  format.df latex.default  # comment
# F  F  T	   $		     # LaTeX usage
# F  T  T   \cdot! $		     # LaTeX usage
# T  F  F   . ~	      .	    dcolumn  # LaTeX usage
# T  T  F   . ~	      \cdot dcolumn  # LaTeX usage
#        
# F  F  F    			     # non-TeX (hyphens in TeX)
#        
# F  T  F   \cdot!		     # TeX errors, hyphens
# T  F  T   . ~	   $  .	    dcolumn  # TeX errors
# T  T  T   . ~	   $  \cdot dcolumn  # TeX errors



latex.default <-
  function(object,
           title=first.word(deparse(substitute(object))),
           file=paste(title, ".tex", sep=""),
           append=FALSE, label=title,
           rowlabel=title, rowlabel.just="l", cgroup=NULL, n.cgroup=NULL,
           rgroup=NULL, n.rgroup=NULL,
           cgroupTexCmd="bfseries",
           rgroupTexCmd="bfseries",
           rownamesTexCmd=NULL, 
           colnamesTexCmd=NULL,
           cellTexCmds=NULL,
           rowname, cgroup.just=rep("c",length(n.cgroup)),
           colheads=dimnames(cx)[[2]],
           extracolheads=NULL, extracolsize='scriptsize',
           dcolumn=FALSE, numeric.dollar=!dcolumn, cdot=FALSE,
           longtable=FALSE, draft.longtable=TRUE, ctable=FALSE, booktabs=FALSE,
           table.env=TRUE, here=FALSE, lines.page=40,
           caption=NULL, caption.lot=NULL, caption.loc=c('top','bottom'),
           double.slash=FALSE,
           vbar=FALSE, collabel.just=rep("c",nc), na.blank=TRUE,
           insert.bottom=NULL, first.hline.double=!(booktabs | ctable),
           where='!tbp', size=NULL,
           center=c('center','centering','none'),
           landscape=FALSE,
           multicol=TRUE, ## to remove multicolumn if no need  SSJ 17nov03
           ...)      ## center MJ 08sep03
{
  center <- match.arg(center)
  caption.loc <- match.arg(caption.loc)
  cx <- format.df(object, dcolumn=dcolumn, na.blank=na.blank,
                  numeric.dollar=numeric.dollar, cdot=cdot, ...)
  # removed check.names=FALSE from above 23jan03
  if (missing(rowname)) rowname <- dimnames(cx)[[1]]
  col.just <- attr(cx,"col.just")
  nc <- ncol(cx)
  nr <- nrow(cx)

if (length(cgroup)) {
  k <- length(cgroup)
  if(!length(n.cgroup)) n.cgroup <- rep(nc/k, k)
  if(sum(n.cgroup)!=nc) stop("sum of n.cgroup must equal number of columns")
  if(length(n.cgroup)!=length(cgroup))
    stop("cgroup and n.cgroup must have same lengths")
}

  if(!length(rowname)) rgroup <- NULL

  if(!length(n.rgroup) && length(rgroup))
	n.rgroup <- rep(nr/length(rgroup), length(rgroup))
  if(length(n.rgroup) && sum(n.rgroup)!=nr)
	stop("sum of n.rgroup must equal number of rows in object")
  if(length(rgroup) && length(n.rgroup) && (length(rgroup)!=length(n.rgroup)))
	stop("lengths of rgroup and n.rgroup must match")
  if (length(rgroup) && rowlabel.just=="l")
	rowname <- paste("~~",rowname,sep="")

  sl <- ifelse(double.slash, "\\\\", "\\")
  eol <- if(ctable) paste(sl, 'NN', sep='') else paste(sl,sl,sep='')
  if(booktabs) {  # 27may02
    toprule    <- paste(sl,"toprule",sep="")
    midrule    <- paste(sl,"midrule",sep="")
    bottomrule <- paste(sl,"bottomrule",sep="")
  } else if(ctable) {   ## 13dec02
    toprule    <- paste(sl, 'FL', sep='')
    midrule    <- paste(sl, 'ML', sep='')
    bottomrule <- paste(sl, 'LL', sep='')
    } else {
    toprule <- if(first.hline.double)
      paste(sl,"hline",sl,"hline",sep="") else paste(sl,"hline",sep="")
    midrule <- bottomrule <- paste(sl,"hline",sep="")
  }




  
  ## ################ CELL AND ROWNAMES FORMATS ###################
  ## If no formats are specified for the rownames and cells there is
  ## nothing to do. If only one is specified then the other must
  ## faked. But rownamesTexCmd should only be faked if rownames is
  ## not NULL.

  ## Check to make sure the dimensions of the cell formats
  ## match the dimensions of the object to be formatted.
  if (!is.null(cellTexCmds) &
      !(all(dim(cx) == dim(cellTexCmds)) &
        length(dim(cx)) == length(dim(cellTexCmds)))) {

    msg <- "The dimensions of cellTexCmds must be:"
    msg1 <- paste(dim(cx), collapse=" x ")
    msg <- paste(msg, msg1)
    msg <- paste(msg, ", but you gave me: ")
    msg1 <- paste(dim(cellTexCmds), collapse=" x ")
    msg <- paste(msg, msg1, sep="")
    stop(msg)
  }
  
  ## If there are column groups, add a blank column
  ## of formats between the groups.
  if (length(cgroup) & !is.null(cellTexCmds)) {
    my.index <- cumsum(n.cgroup)
    new.index <- NULL
    new.col <- dim(cx)[2] + 1
    for (i in seq(along=my.index)) new.index <- c(new.index, my.index[i], new.col)
    new.index <- new.index[-length(new.index)]
    cellTexCmds <- cbind(cellTexCmds, "")[, new.index]
  }

  if (!is.null(cellTexCmds) | !is.null(rownamesTexCmd)) {
    ## LaTeX commands have been specified for either the rownames or
    ## the cells.
    ## Fake rownamesTexCmd if it is NULL and if rowname exists.
    if (is.null(rownamesTexCmd) & !is.null(rowname)) rownamesTexCmd <- rep("", nr)
    ## Fake cellTexCmds if it is NULL.
    if (is.null(cellTexCmds)) {
      cellTexCmds <- rep("", dim(cx)[1] * dim(cx)[2])
      dim(cellTexCmds) <- dim(cx)
    }
    ## Create a combined rowname and cell format object.
    rcellTexCmds <- cbind(rownamesTexCmd, cellTexCmds)
    thisDim <- dim(rcellTexCmds)
    ## Prefix the latex commands with slashes.
    rcellTexCmds <- paste(sl, rcellTexCmds, sep="")
    ## Remove slashes from elements where no format was specified.
    rcellTexCmds[rcellTexCmds == sl] <- ""
    ## Restore the dimensions of the matrix (paste loses them).
    dim(rcellTexCmds) <- thisDim
  } else {
    rcellTexCmds <- NULL
  } ############## END OF CELL AND ROWNAMES FORMATS ###############



  


  
#if (!vbar && length(cgroup)) {
  if (length(cgroup)) {
	last.col <- cumsum(n.cgroup)
	first.col <- c(1, 1+last.col[-length(last.col)])
	cgroup.cols <- cbind(first.col,last.col)
	col.subs <- list()	
    for (i in seq(along=first.col))
      col.subs[[i]] <- first.col[i]:last.col[i]
    
    cxi <- list()
    for (i in seq(along=col.subs)) cxi[[i]] <- cx[,col.subs[[i]],drop=FALSE]
    
    cxx <- cxi[[1]]
    col.justxx <- col.just[col.subs[[1]]]
    collabel.justxx <- collabel.just[col.subs[[1]]]

	cgroupxx <- cgroup[1]
	n.cgroupxx <- n.cgroup[1]
	for (i in seq(along=col.subs)[-1]) {
      cxx <- cbind(cxx, "", cxi[[i]])  # was ""="" 23Feb01 "=" 2Apr02
      col.justxx <- c(col.justxx, "c", col.just[col.subs[[i]]])
      collabel.justxx <-
        c(collabel.justxx, "c", collabel.just[col.subs[[i]]])
      cgroupxx <- c(cgroupxx, "", cgroup[i])
      n.cgroupxx <- c(n.cgroupxx, 1, n.cgroup[i])
	}
	cgroup.colsxx <- cgroup.cols + 0:(nrow(cgroup.cols)-1)
    
	cx <- cxx
	col.just <- col.justxx
	collabel.just <- collabel.justxx
	n.cgroup <- n.cgroupxx
	cgroup.cols <- cgroup.colsxx[cgroup!="",,drop=FALSE]
	cgroup <- cgroupxx
	nc <- ncol(cx)
  }

  cline <- NULL
  if (length(rowname)) {
    cx <- cbind(rowname, cx)
    dimnames(cx)[[2]][1] <- rowlabel
    col.just <- c(rowlabel.just, col.just)
    if(length(extracolheads))
      extracolheads <- c('', extracolheads)  ## 16jun03
    
    collabel.just <- c(rowlabel.just, collabel.just)
    if (!length(cgroup)) n.cgroup <- c(1, nc)
    else {
      cgroup <- c(rowlabel, cgroup)
      dimnames(cx)[[2]][1] <- ""
      rlj <- ifelse(rowlabel.just=="l", "l", "c")
      cgroup.just <- c(rlj, cgroup.just)
      n.cgroup <- c(1, n.cgroup)
      cgroup.cols <- 1+cgroup.cols
      cline <- paste(sl, "cline{", cgroup.cols[,1],"-", cgroup.cols[,2], "}",
                     sep="", collapse=" ")
    }
    nc <- 1 + nc
  }

  vbar <- ifelse(vbar, "|", "")

  if(!append) cat("", file=file)	#start new file
  cat("%",deparse(sys.call()), "\n%\n", file=file, append=file!='')
  ## append= 19apr03 and other places
  ## Was as.character(as.name(match.call()))  15Sep00

  if(dcolumn) {
    decimal.point <- ifelse(cdot, paste(sl,"cdot",sep=""), ".")
    cat(sl,"newcolumntype{.}{D{.}{",decimal.point,"}{-1}}\n",
        sep="", file=file, append=file!='')  # was newcolumn 26Feb02
  }

  { # tabular.cols
    tabular.cols <- paste(vbar, col.just, sep="")
    if (!length(n.cgroup)) tabular.cols <- c(tabular.cols, vbar)
    else {
      vv2 <- cumsum(n.cgroup)
      tabular.cols[vv2] <- paste(tabular.cols[vv2],vbar,sep="")
    }
    tabular.cols <- paste(tabular.cols, collapse="")
  }

  if(length(caption) && !ctable) {
    caption <- paste(
                     sl,"caption",
                     if(length(caption.lot)) paste("[",caption.lot,"]",sep=""),
                     "{", caption,
                     if(!longtable) paste(sl,"label{", label, "}",sep=""),
                     "}", sep="")
    table.env <- TRUE
  }

  if(ctable) {  ## 13dec02
    latex.begin <- c(if(length(size)) paste('{',sl,size,sep=''),
                     paste(sl, "ctable[", sep=''),
                     if(length(caption) && caption.loc=='bottom') 'botcap,',
                     if(length(caption))
                     paste('caption={',caption,'},',sep=''),
                     if(length(caption.lot))
                     paste('cap={',caption.lot,'},',sep=''),
                     paste('label=',label,',',sep=''),
                     if(!landscape) paste('pos=',where,',',sep=''),
                     if(landscape) 'rotate',
                     paste(']{',tabular.cols, '}',sep=''),
                     if(length(insert.bottom))
                     paste('{',sl,'tnote[]{',sedit(insert.bottom,'\\\\',' '),
                           '}}',
                           sep='') else '{}',
                     ## tnote does not allow \\ in its argument
                     paste('{', toprule, sep='')
    )
    latex.end <- c('}',if(length(size)) '}')
  } else if(!longtable) {
    latex.begin <- c(if(landscape) paste(sl, "begin{landscape}",sep=""),
                     if(table.env) paste(
                                         sl, "begin{table}",
                                         if(here)"[H]" else
                                         paste('[',where,']',sep=''),
                                         "\n", sep=""),
                     if(length(size)) paste(sl,size,'\n',sep=''),
                     if(caption.loc=='top' && !missing(caption))
                     paste(caption, "\n"),              ## 3oct03
                     if(center == 'center')             ## MJ: 08sep03
                     paste(sl,"begin{center}\n", sep="")## MJ: 08sep03
                     else {if (center == 'centering')  ## MJ: 08sep03
                     paste(sl,"centering\n", sep="")}, ## MJ: 08sep03
                     paste(sl,"begin{tabular}{", tabular.cols, "}",
                           toprule, "\n", sep="")
                                        #11Jun95   12jan03 "}" was "}{" WHY!
                     )
    latex.end <- c(
                   paste(sl,"end{tabular}\n", sep = ""),
                   if(center == 'center')  ## MJ: 08sep03
                   paste(sl,"end{center}\n", sep=""), ## MJ: 08sep03
                   if(caption.loc=='bottom' && !missing(caption))
                     paste(caption,'\n'),   # 3oct03
                   if(length(insert.bottom)) insert.bottom,
                   if(table.env) paste(sl, "end{table}\n", sep=""),
                   if(landscape) paste(sl, "end{landscape}\n", sep="")
                   )
  }
  else {
    latex.begin <- c(
                     paste(
                           if (!draft.longtable)
                           paste(sl,"let",sl,"LTmulticolumn=",sl,"multicolumn", sep=""),
                           paste(sl,"setlongtables",sep=""),
                           if(landscape) paste(sl, "begin{landscape}",sep=""),
                           if(length(size)) paste('{',sl,size,'\n',sep=''),
                           paste(sl,"begin{longtable}{", tabular.cols, "}",sep=""),
                           sep="\n"),
                     if(caption.loc=='top' && !missing(caption))
                      paste(caption, sl,sl,"\n", sep=""),
                     paste(toprule, "\n", sep="")    #11Jun95
                     )
    latex.end <- paste(if(caption.loc=='bottom' && !missing(caption))
                        paste(caption, sl,sl,"\n",sep=""),  ## 3oct03
                       if(length(insert.bottom)) insert.bottom,
                       paste(sl,"end{longtable}\n", sep=""),
                       if(length(size)) '}',
                       if(landscape) paste(sl,"end{landscape}\n",sep=""))
  }
  
  cat(latex.begin, file=file, append=file!='')

  if(length(cgroup)) {  # was !missing 5Oct00
    cvbar <- paste(cgroup.just, vbar, sep="")
    cvbar[1] <- paste(vbar, cvbar[1], sep="")
    cvbar[-length(cvbar)] <- paste(cvbar[-length(cvbar)], vbar, sep="")
    slmc <- paste(sl,"multicolumn{",sep="")
    ##labs <- paste(sl, "bf ", cgroup, sep="") 
    if (!is.null(cgroupTexCmd)) labs <- paste(sl, cgroupTexCmd, " ", cgroup, sep="") # DRW 12apr05.
    if(multicol) ## SSJ 17nov03
      labs <- paste(slmc, n.cgroup, "}{", cvbar, "}{", labs, "}", sep="")

    cat(labs, file=file, sep="&\n", append=file!='')
    
    if (!length(cline)) {   # was is.length 2Apr02
      inr <- as.numeric(length(rowname))
      cline <- paste(sl,"cline{",1+inr,"-",nc,"}",sep="")
    } 
    cat(eol, " ",cline,"\n", sep="",file=file, append=file!='')
    ## eol was sl, sl  13dec02
  }


  { # column labels
    cvbar <- paste(collabel.just, vbar, sep="")
    cvbar[1] <- paste(vbar, cvbar[1], sep="")
    if (length(n.cgroup)) {
      vv2 <- cumsum(n.cgroup[-length(n.cgroup)])
      cvbar[vv2] <- paste(cvbar[vv2],vbar,sep="")
    }
    slmc1 <- paste(sl, "multicolumn{1}{", sep="")
#    labs <- dimnames(cx)[[2]]   ## 28apr03 and next 5  15jul03 next 2
    labs <- colheads
    if (!is.null(colnamesTexCmd)) labs <- paste(sl, colnamesTexCmd, " ", labs, sep="") # DRW 12apr05.
    if(length(labs)) {
      if(!length(extracolheads)) {
        heads <- get2rowHeads(labs)
        labs <- heads[[1]]
        if(any(heads[[2]] != '')) extracolheads <- heads[[2]]
      }
      if(multicol) ## SSJ 17nov03
        labs <- paste(slmc1, cvbar, "}{", labs, "}", sep="")
      
      cat(labs, file=file, sep="&\n", append=file!='')

      if(length(extracolheads)) {
        extracolheads <- ifelse(extracolheads==''| extracolsize=='',
                                extracolheads,
                                paste('{',sl,extracolsize,' ',
                                      extracolheads,'}',sep=''))
        ## SSJ 17nov03 add | extracolsize=='' to avoid putting {\ } if you don't wont change size in second line title 
        if(multicol) ## SSJ 17nov03
          extracolheads <- ifelse(extracolheads=='',extracolheads,
                                  paste(slmc1,cvbar,'}{',extracolheads,'}',sep=''))
        else  
          extracolheads <- ifelse(extracolheads=='',extracolheads,
                               paste(extracolheads,sep=''))
        
#      cat(eol," ", paste(c(if(length(rowname))'',extracolheads),collapse='&'),
#          file=file, append=file!='') # 21jan03
        cat(eol," ", paste(extracolheads,collapse='&'),
            file=file, append=file!='') # 28apr03
      }
      if(ctable) cat(midrule, '\n', sep='', file=file, append=file!='') else
      cat(eol," ",midrule, "\n",sep="",file=file, append=file!='')
      ## eol was sl, sl  13dec02
    }
  }


  if(longtable) {
    if(missing(caption))
      cat(sl,"endhead\n",midrule,sl,"endfoot\n",sep="",
          file=file,append=file!='')
    else {
      cat(sl,"endfirsthead\n", sep="",file=file, append=file!='')
      cat(sl,"caption[]{\\em (continued)} ",sl,sl,"\n",
          sep="",file=file, append=file!='')
      cat(midrule, "\n", sep="",file=file, append=file!='')
      cat(labs, file=file, sep="&", append=file!='')
      cat(sl,sl," ",midrule, "\n",sl,"endhead",midrule,sl,"endfoot\n",
          sep="",file=file, append=file!='')
      cat(sl,"label{", label, "}\n", sep="", file=file, append=file!='')
    }
  }

  { # individual lines, grouped if appropriate, longtable if appropriate
    if (length(n.rgroup)) {
      rg.end   <- cumsum(n.rgroup)
      rg.start <- rg.end-n.rgroup+1
      if(!length(rgroup)) {
        rgroup <- rep("",length(n.rgroup))
      } else {
        if (!is.null(rgroupTexCmd)) { # DRW 12apr05. This if block.
          rgroup <- paste("{",sl, rgroupTexCmd, " ", rgroup,"}",sep="") 
        } else {
          rgroup <- paste("{", rgroup,"}",sep="") 
        }
      }
      ##else rgroup <- paste("{",sl,"bf ",rgroup,"}",sep="") 
      seq.rgroup <- seq(along=n.rgroup)
    }
    else {
      seq.rgroup <- 1
      rg.end <- nr
      rg.start <- 1
    }

    linecnt <- 0
    for (j in seq.rgroup) {
      if (length(n.rgroup)) {
        if(longtable && linecnt>0 &&
           (linecnt+n.rgroup[j]+(n.rgroup[j]>1)) > lines.page) {
          cat(sl,"newpage\n", sep="",file=file, append=file!='')
          linecnt <- 0
        }
        cat(rgroup[j], rep("",nc-1), sep="&", file=file, append=file!='')
        cat(eol,"\n", sep="",file=file, append=file!='')
        ## eol was sl,sl 13dec02
        linecnt <- linecnt+1
      }

      ## Write the object (and it's formatting instructions)
      ## to the output.
      ## Loop through the rows of the object.
      for(i in rg.start[j]:rg.end[j]) {
        if (!length(n.rgroup)) {
          if(longtable && linecnt>0 && (linecnt+1 > lines.page)) {
            cat(sl,"newpage\n",sep="",file=file, append=file!='')
            linecnt <- 0						
          }
        }

        ## Loop through the columns of the object
        ## write each value (and it's format if there
        ## is one). 
        ## DRW 12apr05. This if/else block.
        if (!is.null(rcellTexCmds)) {
          num.cols <- ncol(cx)
          for (colNum in 1:num.cols) {
            cat(rcellTexCmds[i, colNum], " ", cx[i, colNum], file=file, append=file!='')
            if (colNum < num.cols) cat(" & ", file=file, append=file!='')
          }
        } else {
          ## Original code that writes object to output.
          cat(cx[i,], file=file, sep="&", append=file!='')
        } 
        
        cat(if(!ctable || i < rg.end[j]) eol,
            "\n", sep="",file=file, append=file!='')
        ## eol was sl,sl  added if( ) 13dec02
        linecnt <- linecnt+1
      }  ## End of for loop that writes the object.

      
      cat(bottomrule, "\n", sep="",file=file, append=file!='')
    }
  }

  cat(latex.end, file=file, sep="\n", append=file!='')
  sty <- c("longtable"[longtable], "here"[here], "dcolumn"[dcolumn],
           "ctable"[ctable], "booktabs"[booktabs],
           if(landscape && !ctable) "lscape")
  
  structure(list(file=file, style=sty), class='latex')
}


# Re-written by Daniel Calvelo Aros <dcalvelo@minag.gob.pe> to not use
# S.sty  18Feb04
latex.function <- function(
	object,
	title=first.word(deparse(substitute(object))),
	file=paste(title, ".tex", sep=""),
	append=FALSE,
	assignment=TRUE,  type=c('example','verbatim'), ...)
{
  type <- match.arg(type)
  type <- match.arg(type)
  fctxt <- format(object)
  if(assignment) fctxt[1] <- paste(title , '<-', fctxt[1]) 
  environment <- ifelse(type=='example', "alltt", "verbatim")
  preamble <- paste("\\begin{",environment,"}\n",sep="")
  cat(preamble, file=file, append=file!="")
  rxs <- if(type=='example') c(
              "\t=>    ",
              "\\\\=>\\\\(\\\\backslash\\\\)",
              "([{}])=>\\\\\\1",
              "<-=>\\\\(\\\\leftarrow\\\\)",
              "#(.*?$)=>{\\\\rm\\\\scriptsize\\\\#\\1}" 
              ) else c(
                       "\t=>    "
                       )                       
  substitute <- strsplit( rxs, "=>" )
  for(line in fctxt){
    for( subst in substitute ){
      line <- gsub( subst[1], subst[2], line, perl=TRUE )
    }
    line <- paste(line,"\n",sep="")
    cat(line, file=file, append=file!="")
  }
  postamble <- paste("\\end{",environment,"}\n", sep="")
  cat(postamble, file=file, append=file!='')
  
  structure(list(file=file, style=if(type=='example')'alltt'), class='latex')
}

latexVerbatim <- function(x,
                          title=first.word(deparse(substitute(x))),
                          file=paste(title, ".tex", sep=""),
                          append=FALSE, size=NULL, hspace=NULL,
                          width=.Options$width, length=.Options$length, ...) {

  if(!missing(width) || !missing(length)) {
    old <- options(width=width, length=length)
    on.exit(options(old))
  }
  sink(file, append=append)
  cat('\\setbox0=\\vbox{\n',if(length(size))c('\\',size,'\n'),
      '\\begin{verbatim}\n', sep='')
  print(x, ...)
  cat('\\end{verbatim}\n}\n',if(length(hspace))c('\\hspace{',hspace,'}'),
      '{\\makebox[\\textwidth]{\\box0}}\n', sep='')
  sink()
 
  structure(list(file=file, style=NULL), class='latex')
}

latex.list <- function( object,
	title=first.word(deparse(substitute(object))),
                       file=paste(title, ".tex", sep=""), append=FALSE,
                       label,
                       caption, caption.lot,
                       caption.loc=c('top','bottom'),
                       ...) {
  caption.loc <- match.arg(caption.loc)
  nx <-	names(object)
  if (!length(nx)) nx <- paste(title, "[[",
	seq(along=object), "]]", sep="")
  tmp <- latex(object=object[[1]],
	caption=nx[1], label=nx[1], append=append, title=title, file=file,
	caption.lot=NULL, caption.loc=caption.loc, ...)
  tmp.sty <- tmp$style
  for (i in	seq(along=object)[-1]) {
    tmp <- latex(object=object[[i]],
                 caption=nx[i], label=nx[i], append=file!='', title=title, file=file,
                 caption.lot=NULL, caption.loc=caption.loc, ...)
    tmp.sty <- c(tmp.sty, tmp$style)
  }
  sty <- if(length(tmp.sty)) unique(tmp.sty) else NULL
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
                           greek=FALSE, ...) {

  text <- object
  
  inn <- c("|",  "%",  "<=",     "<",  ">=",     ">",  "_", "\\243",
         inn, 
         if(pb) c("[","(","]",")"))

out <- c("$|$","\\%","$\\leq$","$<$","$\\geq$","$>$","\\_", "\\pounds",
         out, 
         if(pb) c("$\\left[","$\\left(","\\right]$","\\right)$"))

text <- sedit(text, '$', 'DOLLARS', wild.literal=TRUE)   ##17Nov00
text <- sedit(text, inn, out)

  ##See if string contains an ^ - superscript followed by a number
  ## (number condition added 31aug02)

  dig <- c('0','1','2','3','4','5','6','7','8','9')

for(i in 1:length(text)) {
  lt <- nchar(text[i])
  x <- substring(text[i],1:lt,1:lt)
  j <- x=='^'
  if(any(j)) {
    is <- ((1:lt)[j])[1]  #get first ^
    remain <- x[-(1:is)]
    k <- remain %in% c(' ',',',')',']','\\','$')
    ## Following 3 lines 31aug02
    if(remain[1] %in% dig ||
       (length(remain) > 1 && remain[1]=='-' && remain[2] %in% dig))
       k[-1] <- k[-1] | remain[-1] %nin% dig
    ie <- if(any(k)) is + ((1:length(remain))[k])[1] else length(x)+1
#See if math mode already turned on (odd number of $ to left of ^)
    dol <- if(sum(x[1:is]=='$') %% 2) '' else '$'
    substring2(text[i],is,ie-1) <- paste(dol,'^{',
                                        substring(text[i],is+1,ie-1),'}',
       dol,sep='')  # 25May01
  }
  if(greek) {
    gl <- Cs(alpha,beta,gamma,delta,epsilon,varepsilon,zeta,eta,theta,
             vartheta,iota,kappa,lambda,mu,nu,xi,pi,varpi,rho,varrho,
             sigma,varsigma,tau,upsilon,phi,carphi,chi,psi,omega,Gamma,
             Delta,Theta,Lambda,Xi,Pi,Sigma,Upsilon,Phi,Psi,Omega)
    for(w in gl)
      text[i] <- gsub(paste('\\b', w, '\\b', sep=''),
                      paste('$\\\\',w,'$',   sep=''),
                      text[i])
  }
}
sedit(text, 'DOLLARS', '\\$', wild.literal=TRUE)  ## 17Nov00
}


latex <- function(object,
                  title=first.word(deparse(substitute(object))),...)
  {
    ## added title= 25May01
    if (!length(oldClass(object))) oldClass(object) <- data.class(object)
    UseMethod("latex")
  }


optionsCmds <- function(pgm) {
  optionName <- paste(pgm,'cmd',sep='')
  v <- .Options[[optionName]]
  if(pgm=='xdvi' && !under.unix && !length(v))
    v <- 'yap'  # MikTeX  7Feb03
  if(length(v) && v!='') pgm <- v
  pgm
}

dvi.latex <- function(object, prlog=FALSE,
                      nomargins=TRUE, width=5.5, height=7, ...) {
  fi <- object$file; sty <- object$style

  if(length(sty))sty <- paste('\\usepackage{',sty,'}',sep='')
  if(nomargins) sty <-  c(sty,
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
  
  sc <- if(under.unix)';' else '&'   # DOS command separator
  sys(paste('cd',dQuote(tempdir()),sc,optionsCmds('latex'),
            '-interaction=scrollmode', dQuote(tmp)), output=FALSE)
  if(prlog) cat(scan(paste(tmp,'log',sep='.'),list(''),sep='\n')[[1]],
                sep='\n')
  fi <- paste(tmp,'dvi',sep='.')
  structure(list(file=fi), class='dvi')
}

if(.R. && FALSE) show <- function(object) UseMethod('show')

show.dvi <- function(object, width=5.5, height=7) {
  viewer <- optionsCmds('xdvi')
  cmd <- if(viewer=='yap') paste(viewer,object$file) else
   if(viewer=='kdvi') paste(viewer,object$file,'&') else
   paste(viewer, ' -paper ',
         width,'x',height,'in -s 0 ',
         object$file,' &',sep='')
  sys(cmd)
  invisible()
}

## enhanced show.latex 22dec02 - special treatment of file==''
show.latex <- function(object) {
  if(object$file=='') {
    if(length(object$style)) {
      latexStyles <- if(exists('latexStyles'))
        unique(c(latexStyles, object$style)) else object$style
      storeTemp(latexStyles,'latexStyles')
    }
    return(invisible())
  }
  show.dvi(dvi.latex(object))
}

print.dvi <- function(x, ...) show.dvi(x)
print.latex <- function(x, ...) show.latex(x)
  
dvi         <- function(object, ...) UseMethod('dvi')
dvips       <- function(object, ...) UseMethod('dvips')
dvigv       <- function(object, ...) UseMethod('dvigv')
dvips.dvi   <- function(object, file, ...) {
  cmd <- if(missing(file))
    paste(optionsCmds('dvips'), dQuote(object$file)) else
  paste(optionsCmds('dvips'),'-o', file, dQuote(object$file))
  ## paste(optionsCmds('dvips'),'-f', object$file,' | lpr') else 5dec03
  ## 2 dQuote 26jan04
  invisible(sys(cmd))
}
dvigv.dvi   <- function(object, ...)
  invisible(sys(paste(optionsCmds('dvips'),'-f',object$file,
                      '| gv - &')))
## added ... to dvixx.dvi calls below 1dec03
dvips.latex <- function(object, ...) invisible(dvips.dvi(dvi.latex(object),...))
dvigv.latex <- function(object, ...) invisible(dvigv.dvi(dvi.latex(object),...))

html <- function(object, ...) UseMethod('html')
                 
html.latex <- function(object, ...) {
  fi  <- object$file
  sty <- object$style
  
  if(length(sty))sty <- paste('\\usepackage{',sty,'}',sep='')
  ## pre <- tempfile(); post <- tempfile()  1dec03
  tmp <- tempfile()
  tmptex <- paste(tmp,'tex',sep='.')  # 5dec03
  infi <- readLines(fi)
  cat('\\documentclass{report}', sty, '\\begin{document}', infi,
      '\\end{document}\n', file=tmptex, sep='\n')
  ##  if(under.unix)
  ##    sys(paste('cat',pre,fi,post,'>',paste(tmp,'tex',sep='.')))
  ##  else sys(paste('copy',pre,'+',fi,'+',post,paste(tmp,'tex',sep='.')))
  ## 17dec02
  ##  unlink(c(pre,post))
  sc <- if(under.unix)';' else '&'  # 7feb03
  sys(paste('cd ',dQuote(tempdir()),sc,
            ' hevea ',dQuote(tmptex), sep=''))
  ## 24nov03 dQuote
  fi <- paste(tmp,'html',sep='.')
  structure(list(file=fi), class='html')
}

html.data.frame <-
  function(object,
           file=paste(first.word(deparse(substitute(object))),
             'html',sep='.'),
           append=FALSE,
           link=NULL, linkCol=1, linkType=c('href','name'),
           ...) {

  linkType <- match.arg(linkType)
  
  x   <- format.df(object, ...)
  adj <- attr(x,'col.just')

  if(any(adj=='r')) for(i in seq(along=adj)[adj=='r'])
    x[,i] <- paste('<div align=right>',x[,i],'</div>',sep='')

  if(length(r <- dimnames(x)[[1]])) x <- cbind('Name'=r, x)
  cat('<TABLE BORDER>\n', file=file, append=append)
  cat('<tr>', paste('<td>', dimnames(x)[[2]], '</td>',sep=''), '</tr>\n',
      sep='', file=file, append=file!='')
  if(length(link)) x[,linkCol] <-
    ifelse(link=='',x[,linkCol],
           paste('<a ',linkType,'="',link,'">',x[,linkCol],'</a>',sep=''))

  for(i in 1:nrow(x))
    cat('<tr>',paste('<td>',x[i,],'</td>',sep=''),'</tr>\n',
        sep='', file=file, append=file!='')

  cat('</TABLE>\n', file=file, append=file!='')
  structure(list(file=file), class='html')
}

html.default <- function(object,
                         file=paste(first.word(deparse(substitute(object))),
                           'html',sep='.'),
                         append=FALSE,
                         link=NULL, linkCol=1, linkType=c('href','name'),
                         ...)
html.data.frame(object, file=file, append=append, link=link,
                linkCol=linkCol, linkType=linkType, ...)

show.html <- function(object) {
  browser <- .Options$help.browser
  if(!length(browser)) browser <- .Options$browser
  if(!length(browser)) browser <- 'netscape'
  sys(paste(browser, object, if(under.unix) '&'))
  invisible()
}

print.html <- function(x, ...) show.html(x)

latexSN <- function(x) {
  x <- format(x)
  x <- sedit(x, c('e+00','e-0*',                'e-*',
                    'e+0*',               'e+*'),
                  c('',    '\\\!\\times\\\!10^{-*}','\\\!\\times\\\!10^{-*}',
                    '\\\!\\times\\\!10^{*}','\\\!\\times\\\!10^{*}'))
  x
}
