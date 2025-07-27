if(!exists("NROW", mode='function')) {
  NROW <- function(x)
    if (is.array(x) || is.data.frame(x)) nrow(x) else length(x)
}

if(!exists("NCOL", mode='function')) {
  NCOL <- function(x)
    if (is.array(x) && length(dim(x)) > 1 || is.data.frame(x)) ncol(x) else as.integer(1)
}

prn <- function(x, txt, file='', head=deparse(substitute(x), width.cutoff=500)[1])
{
  if(file != '') sink(file, append=TRUE)
  
  if(!missing(txt)) {
    if(nchar(txt) + nchar(head) +3 > .Options$width)
      head <- paste('\n\n  ', head, sep='')
    else
      txt <- paste(txt, '   ', sep='')
    cat('\n', txt, head, '\n\n', sep='') 
  }
  else cat('\n', head, '\n\n',sep='')
  print(x)
  if(file != '') sink()
  invisible()
}

formatSep <- function(x, digits, ...)
{
  y <- character(length(x))
  for(i in 1:length(x))
    y[i] <- if(missing(digits)) format(x[i], ...)
            else format(x[i],digits=digits, ...)

  names(y) <- names(x)
  y
}

nomiss <- function(x)
{
  if(is.data.frame(x)) na.exclude(x)
  else if(is.matrix(x))
    x[!is.na(x %*% rep(1,ncol(x))),]
  else x[!is.na(x)]
}

fillin <- function(v, p)
{
  v.f <- ifelse(is.na(v),p,v)
  if(length(p)==1)
    label(v.f) <- paste(label(v),"with",sum(is.na(v)),
                        "NAs replaced with",format(p))
  else
    label(v.f) <- paste(label(v),"with",sum(is.na(v)),"NAs replaced")
  v.f
}

spearman <- function(x, y)
{
  x <- as.numeric(x)
  y <- as.numeric(y)  ## 17Jul97
  
  notna <- !is.na(x+y)	##exclude NAs
  if(sum(notna) < 3)
    c(rho=NA)
  else
    c(rho=cor(rank(x[notna]), rank(y[notna])))
}

plotCorrPrecision <- function(rho=c(0,0.5), n=seq(10,400,length.out=100),
                              conf.int=0.95, offset=.025, ...)
{
  ## Thanks to Xin Wang for computations
  curves <- vector('list', length(rho))
  names(curves) <- paste('r',format(rho),sep='=')
  zcrit <- qnorm(1-(1-conf.int)/2)
  for(i in 1:length(rho)) {
    r <- rho[i]
    z <- .5*log((1+r)/(1-r))
    lo <- z - zcrit/sqrt(n-3)
    hi <- z + zcrit/sqrt(n-3)
    rlo <- (exp(2*lo)-1)/(exp(2*lo)+1)
    rhi <- (exp(2*hi)-1)/(exp(2*hi)+1)
    precision <- pmax(rhi-r, r-rlo)
    curves[[i]] <- list(N=n, Precision=precision)
  }
  labcurve(curves, pl=TRUE, xrestrict=quantile(n,c(.25,1)), offset=offset, ...)
  invisible()
}

trap.rule <- function(x,y) sum(diff(x)*(y[-1]+y[-length(y)]))/2

uncbind <- function(x, prefix="", suffix="")
{
  nn <- dimnames(x)[[2]]
  warning("You are using uncbind.  That was a really bad idea. If you had any variables in the global environment named ", paste(prefix, nn, suffix, sep="", collapse=", "), " they are now over writen.\n\nYou are now warned.", immediate. = TRUE, )
  for(i in 1:ncol(x))
    assign(paste(prefix,nn[i],suffix,sep=""), x[,i], pos=parent.env())
  invisible()
}

## Function to pick off ordinates of a step-function at user-chosen abscissas

stepfun.eval <- function(x, y, xout, type=c("left","right"))
{
  s <- !is.na(x+y)
  type <- match.arg(type)
  approx(x[s], y[s], xout=xout, method="constant", f=if(type=="left")0 else 1)$y
}


km.quick <- function(S, times, q, type=c('kaplan-meier', 'fleming-harrington', 'fh2'),
                     interval=c('>', '>='), method=c('constant', 'linear'), fapprox=0, n.risk=FALSE)
{
  sRequire('survival')
  type     <- match.arg(type)
  interval <- match.arg(interval)
  method   <- match.arg(method)

  S <- S[! is.na(S), ]
  stratvar <- factor(rep(1, nrow(S)))
  f <- if(attr(S, 'type') == 'right')
    survival::survfitKM(stratvar, S, se.fit=FALSE, conf.type='none', type=type)
    else survival::survfit(S ~ stratvar, se.fit=FALSE, conf.type='none')
  t0 <- f$t0
  if(! length(t0)) t0 <- 0e0
  nr <- if(n.risk) list(time=f$time, n.risk=f$n.risk)
  if(missing(times) & missing(q)) {
    time <- f$time[f$n.event > 1e-10]    # survfit.formula for left censoring
    surv <- f$surv[f$n.event > 1e-10]    # creates a tiny nonzero value for n.event
    if(interval == '>=') surv <- c(1e0, surv[-length(surv)])
    res <- list(time=time, surv=surv)
  } else {
    tt <- c(t0, f$time)
    ss <- c(1, f$surv)
    if(missing(times)) res <- min(tt[ss <= q])
    else {
      if(interval == '>=') {tt <- f$time; ss <- ss[-length(ss)]}
      res <- approxExtrap(tt, ss, xout=times, method=method, f=fapprox)$y
    }
  }
if(n.risk) attr(res, 'n.risk') <- nr
res
}

oPar <- function()
{
  ## Saves existing state of par() and makes changes suitable
  ## for restoring at the end of a high-level graphics functions
  oldpar <- par()
  oldpar$fin <- NULL
  oldpar$new <- FALSE
  invisible(oldpar)
}

setParNro <- function(pars)
{
  ## Sets non-read-only par parameters from the input list
  i <- names(pars) %nin%
    c('cin','cra','csi','cxy','din','xlog','ylog','gamma','page')
  invisible(par(pars[i]))
}

mgp.axis.labels <- function(value, type=c('xy','x','y','x and y'))
{
  type <- match.arg(type)
  if(missing(value)) {
    value <- .Options$mgp.axis.labels
    pr <- par(c('mgp', 'las'))
    mgp <- pr$mgp
    if(! length(value)) value <- c(.7, .7)
    return(switch(type, 
                  xy = value, 
                  x = c(mgp[1], value[1], mgp[3]),
                  y = c(mgp[1], value[2], mgp[3]),
                  'x and y' = list(x = c(mgp[1], value[1], mgp[3]),
                                   y = c(mgp[1], value[2], mgp[3]))))
  }
  
  if(value[1]=='default')
    value <- c(.7,.7)
  
  ##c(.6, if(par('las')==1) 1.3 else .6)
  options(mgp.axis.labels=value, TEMPORARY=FALSE)
  invisible()
}

mgp.axis <-
  function(side, at=NULL, ...,
           mgp=mgp.axis.labels(type=if(side==1 | side==3)'x' else 'y'),
           axistitle=NULL, cex.axis=par('cex.axis'), cex.lab=par('cex.lab'))
{
  ## Version of axis() that uses appropriate mgp from mgp.axis.labels and
  ## gets around bug in axis(2, ...) that causes it to assume las=1
  mfrow <- par('mfrow')
  tcl   <- max(par('tcl'), -0.4)
  nr <- mfrow[1]; nc <- mfrow[2]
  w <- list(side=side)
  w <- c(w, list(...))
  w$cex.axis <- cex.axis
  if(length(at)) w$at <- at
  if(side == 1 || side == 3) {
    w$mgp <- mgp / nr
    w$tcl <- tcl / nr
    if(side==1 && length(axistitle))
      title(xlab=axistitle, mgp = mgp / min(2.25, nr), cex.lab=cex.lab)
  } else {
    w$mgp <- mgp / nc
    w$tcl <- tcl / nc
    las <- par('las')
    w$srt <- 90 * (las == 0)
    w$adj <- if(las == 0) 0.5
    else 1
    if(side == 2 && length(axistitle))
      title(ylab=axistitle, mgp=mgp / min(2.25, nc), cex.lab=cex.lab)
  }
  do.call('axis', w)
  invisible()
}

trellis.strip.blank <- function()
{
  s.b <- lattice::trellis.par.get("strip.background")
  s.b$col <- 0
  lattice::trellis.par.set("strip.background", s.b)
  s.s <- lattice::trellis.par.get("strip.shingle")
  s.s$col <- 0
  lattice::trellis.par.set("strip.shingle", s.s)
  invisible()
}

lm.fit.qr.bare <- function(x, y, 
                           tolerance = NULL,
                           intercept=TRUE, xpxi=FALSE,
                           singzero=FALSE)
{
  if(!length(tolerance)) tolerance <- 1e-7
  if(intercept)
    x <- cbind(Intercept=1, x)
  else x <- as.matrix(x)
  z    <- lm.fit(x, y, tol=tolerance)
  coef <- z$coefficients
  if(singzero && any(isna <- is.na(coef))) coef[isna] <- 0.
    
  res <- z$residuals
  sse <- sum(res^2)
  sst <- sum((y - mean(y))^2)

  res <- list(coefficients = coef,    residuals = res, 
              rsquared     = 1 - sse / sst,
              fitted.values = z$fitted.values)
  if(xpxi) {
    p <- 1L : z$rank
    res$xpxi <- chol2inv(z$qr$qr[p, p, drop=FALSE])
  }
  res
}

all.is.numeric <- function(x, what=c('test','vector','nonnum'),
                           extras=c('.','NA'))
{
  what <- match.arg(what)
  x <- sub('[[:space:]]+$', '', x)
  x <- sub('^[[:space:]]+', '', x)
  xs <- x[x %nin% c('', extras)]
  if(! length(xs) || all(is.na(x)))
    return(switch(what, test = FALSE, vector=x, nonnum=x[0]))
  isnon <- suppressWarnings(! is.na(xs) & is.na(as.numeric(xs)))
  isnum <- ! any(isnon)
  # suppressWarnings below handles extras present in x
  switch(what,
         test   = isnum,
         vector = if(isnum) suppressWarnings(as.numeric(x)) else x,
         nonnum = xs[isnon])
}

Lag <- function(x, shift=1)
{
  ## Lags vector x shift observations, padding with NAs or blank strings
  ## preserving attributes of x

  xLen <- length(x)
  if(shift == 0) return(x)
  
  # Create base vector use character to generate "" for mode "character"
  # Coerce base vector to be type of x
  ret <- as.vector(character(xLen), mode=storage.mode(x))
  
  # set resp attributes equal to x attributes
  attrib <- attributes(x)

  if(length(attrib$label))
    attrib$label <- paste(attrib$label, 'lagged', shift, 'observations')

  if(abs(shift) < xLen)
    {
      if(shift > 0) ret[-(1:shift)] <- x[1:(xLen - shift)]
      else ret[1:(xLen+shift)] <- x[(1-shift):xLen]
    }
  
  attributes(ret) <- attrib
  return(ret)
}

xySortNoDupNoNA <- function(x, y)
{
  if(is.list(x)) {
    y <- x[[2]]; x <- x[[1]]
  }
  
  s <- !is.na(x + y)
  if(any(s)) {
    x <- x[s]; y <- y[s]
  }
  
  i <- order(x)
  x <- x[i]
  y <- y[i]
  i <- !duplicated(x)
  list(x=x[i], y=y[i])
}

outerText <-
  function(string, y, cex=par('cex'), ...) {
    usr <- par('usr'); plt <- par('plt')
    pos <- usr[2] + (usr[2] - usr[1])/(plt[2] - plt[1]) * (1 - plt[2])
    axis(2, at=y, labels=string, tick=FALSE, las=1,
         pos=pos, cex.axis=cex, xpd=NA)
  }

##    if(missing(space)) space <- max(nchar(string))*.5
##    mtext(string, side=side, las=1, at=y, adj=adj, cex=cex, line=space)


# This method does not survive shrinking the graphics window
# Right justifies (if adj=1) a vector of strings against the right margin
# (side=4) or against the y-axis (side=2)
#outerText <-
#  function(string, y, side=4, cex=par('cex'), adj=1, ...) {
#    if(side %nin% c(2,4)) stop('only works for side=2 or 4')
#    x <- if(side==4) grconvertX(1, from='nfc', to='user') else
#     par('usr')[1]
#    text(x, y, paste(string,''), cex=cex, adj=adj, xpd=NA)
#}    

## Old method [dropped because does not scale upon resizing device]
  ## Use text() to put test strings in left or right margins
  ## Temporarily sets par(xpd=NA) if using R
  ## For adj=1 side=4, setAside is a character string used to determine
  ## the space to set aside for all strings
  ## space is the number of extra characters to leave to the left of
  ## the string(s) (adj=0) or to the right (adj=1)
  
if(FALSE) outerText <- function(string, y, setAside=string[1], side=4, space=1,
                      adj=1, cex=par('cex'))
{
  usr <- par('usr')
  xpd <- par('xpd')
  if(!is.na(xpd)) {
    on.exit(par(xpd=xpd))
    par(xpd=NA)
  }
  
  ie <- is.expression(string)  ## 1sep02
  if(ie)
    adj <- 0  ## adj=1 not work well for expressions in R
  
  if(side!=4)
    stop('only side=4 implemented')
  if(adj==0)
    text(usr[2], y,
         if(ie)
           string
         else
           paste(space,string,sep=''),
         adj=0)
  else {
    usr.space.needed <- strwidth(setAside, units='user', cex=cex)
    text(usr[2]+0.5*strwidth(space, units='user', cex=cex)+usr.space.needed,
         y, string, adj=1, cex=cex) # was usr[2]- 18jul02;added 0* 25jul02
    ## was 0*strwidth(space,...) 31jan03
  }
  invisible()
}

if(FALSE) {
  expandUsrCoord <- function()
  {
    ## Expands usr coordinates of current plot to entire figure region
    ## so that out of range plots may be plotted
    pr <- par()
    usr <- pr$usr
    p <- pr$plt
    invisible(pr)
  }
}


## Author: Patrick Connolly <P.Connolly@hortresearch.co.nz>
## HortResearch
## Mt Albert
## Auckland, New Zealand

print.char.matrix <-
  function (x, file = "",
            col.name.align = "cen", col.txt.align = "right", 
            cell.align = "cen", hsep = "|", vsep = "-", csep = "+",
            row.names = TRUE, col.names = FALSE,
            append = FALSE, top.border = TRUE, left.border = TRUE, ...) 
{
### To print a data frame or matrix to a text file or screen
###   and having names line up with stacked cells
###
### First, add row names as first column (might be removed later)
  
  ndimn <- names(dimnames(x))  ## FEH
  rownames <- dimnames(x)[[1]]
  x <- cbind(rownames, x)
  names(dimnames(x)) <- ndimn  ## FEH
  cnam <- dimnames(x)[[2]]     ## FEH
  if(length(ndimn))
    cnam[1] <- ndimn[1]  ## FEH
  ##dimnames(x)[[1]] <- seq(nrow(x))  25Mar02 for R  FEH
  dimnames(x) <- list(as.character(seq(nrow(x))), cnam)
  names(dimnames(x)) <- ndimn  ## 26Mar02 FEH
###  Set up some padding functions:
###
  pad.left <- function(z, pads)
  {
    ## Pads spaces to left of text
    padding <- paste(rep(" ", pads), collapse = "")
    paste(padding, z, sep = "")
  }
  
  pad.mid <- function(z, pads)
  {
    ## Centres text in available space
    padding.right <- paste(rep(" ", pads%/%2), collapse = "")
    padding.left <- paste(rep(" ", pads - pads%/%2), collapse = "")
    paste(padding.left, z, padding.right, sep = "")
  }
  
  pad.right <- function(z, pads) {
    ## Pads spaces to right of text
    padding <- paste(rep(" ", pads), collapse = "")
    paste(z, padding, sep = "")
  }
  
  ##  (Padding happens on the opposite side to alignment)
  pad.types <- c("left", "mid", "right")
  names(pad.types) <- c("right", "cen", "left")
  pad.name <- pad.types[col.name.align]
  pad.txt <- pad.types[col.txt.align]
  pad.cell <- pad.types[cell.align]
  
  ## Padding character columns
  ##    Need columns with uniform number of characters
  pad.char.col.right <- function(y)
  {
    ## For aligning text to LHS of column
    col.width <- nchar(y)
    biggest <- max(col.width)
    smallest <- min(col.width)
    padding <- biggest - col.width
    out <- NULL
    for (i in seq(y))
      out[i] <- pad.right(y[i], pads = padding[i])
    out
  }
  
  pad.char.col.left <- function(y)
  {
    ## For aligning text to RHS of column
    col.width <- nchar(y)
    biggest <- max(col.width)
    smallest <- min(col.width)
    padding <- biggest - col.width
    out <- NULL
    for (i in seq(y))
      out[i] <- pad.left(y[i], pads = padding[i])
    out
  }
  
  pad.char.col.mid <- function(y) {
    ## For aligning text to centre of column
    col.width <- nchar(y)
    biggest <- max(col.width)
    smallest <- min(col.width)
    padding <- biggest - col.width
    out <- NULL
    for (i in seq(y))
      out[i] <- pad.mid(y[i], pads = padding[i])
    out
  }
  
  ## which functions to use this time.
  pad.name.fn <- get(paste("pad.", pad.name, sep = ""))
  pad.txt.fn <- get(paste("pad.char.col.", pad.txt, sep = ""))
  pad.cell.fn <- get(paste("pad.", pad.cell, sep = ""))
  
  ## Remove troublesome factors
  x <- as.data.frame(x)
  fac.col <- names(x)[sapply(x, is.factor)]
  for (i in fac.col)
    x[, i] <- I(as.character(x[, i]))
  ## ARE ANY LINE BREAKS IN ANY COLUMNS?
  break.list <- list()
  for (i in seq(nrow(x))) {
    x.i <- unlist(x[i, ])
    rows.i <- sapply(strsplit(unlist(x[i, ]), "\n"), length)
    rows.i[rows.i < 1] <- 1
    break.list[[i]] <- rows.i
  }
  break.row <- sapply(break.list, function(x) any(x > 1))
  names(break.row) <- seq(nrow(x))
  xx <- x
  if (any(break.row)) {
    ## add in extra row/s
    xx <- NULL
    reprow <- lapply(break.list, unique)
    for (k in seq(nrow(x))) {
      x.k <- unlist(x[k, ])
      x.k[x.k == ""] <- " "
      if (break.row[k]) {
        l.k <- strsplit(x.k, "\n")
        add.blanks <- max(break.list[[k]]) - break.list[[k]]
        names(l.k) <- names(add.blanks) <- seq(length(l.k))
        if (any(add.blanks > 0)) {
          for (kk in names(add.blanks[add.blanks > 0]))
            l.k[[kk]] <- c(l.k[[kk]], rep(" ", add.blanks[kk]))
        }
        l.k.df <- as.data.frame(l.k)
        names(l.k.df) <- names(x)
        xx <- rbind(xx, as.matrix(l.k.df))
      }
      else xx <- rbind(xx, x.k)
    }
    row.names(xx) <- paste(rep(row.names(x), sapply(reprow, 
                                                    max)),
                           unlist(reprow), sep = ".")
    
    ## Make an index for the rows to be printed
    rn <- row.names(xx)
    rnb <- strsplit(rn, "\\.")
    rpref <- as.numeric(factor(sapply(rnb, function(z) z[1])))
    ## was codes( ) 10oct03
  }
  else
    rpref <- seq(nrow(x))
  x <- as.data.frame(xx)
  
  ## Character columns need different treatment from numeric columns
  char.cols <- sapply(x, is.character)
  if (any(char.cols)) 
    x[char.cols] <- sapply(x[char.cols], pad.txt.fn)
  
  ## Change numeric columns into character
  if (any(!char.cols)) 
    x[!char.cols] <- sapply(x[!char.cols], format)
  
  ## now all character columns each of which is uniform element width
  ##
  ## Lining up names with their columns
  ## Sometimes the names of columns are wider than the columns they name, 
  ##  sometimes vice versa.

  names.width <- nchar(names(x))
  if (!col.names) 
    names.width <- rep(0, length(names.width))
  cell.width <- sapply(x, function(y) max(nchar(as.character(y))))

  ## (the width of the characters in the cells as distinct
  ##  from their names)  
  name.pads <- cell.width - names.width
  cell.pads <- -name.pads
  name.pads[name.pads < 0] <- 0
  cell.pads[cell.pads < 0] <- 0
  pad.names <- name.pads > 0
  pad.cells <- cell.pads > 0
  
  ## Pad out the column names if necessary:
  if (any(pad.names)) {
    stretch.names <- names(x)[pad.names]
    for (i in stretch.names) {
      names(x)[names(x) == i] <- pad.name.fn(i, name.pads[i])
    }
  }
  
  ## likewise for the cells and columns
  if (any(pad.cells)) {
    stretch.cells <- names(x)[pad.cells]
    for (j in stretch.cells) x[, j] <- pad.cell.fn(x[, j], 
                                                   cell.pads[j])
  }
  
  ## Remove row names if not required
  if (!row.names) 
    x <- x[-1]
  ## Put the column names on top of matrix
  if (col.names) 
    mat2 <- rbind(names(x), as.matrix(x))
  else
    mat2 <- as.matrix(x)
  
  mat.names.width <- nchar(mat2[1, ])
  ## character string to separate rows
  space.h <- ""
  for (k in seq(along=mat.names.width)) {  ## added along= FEH 26Mar02
    space.h <- c(space.h, rep(vsep, mat.names.width[k]), csep)
  }
  
  line.sep <- paste(c(ifelse(left.border, csep, ""), space.h), 
                    collapse = "")
  if (col.names) 
    rpref <- c(0, rpref, 0)
  else
    rpref <- c(rpref, 0)
  
  ## print to screen or file
  if(top.border && line.sep !='') {
    write(line.sep, file = file, append = append)
    append <- TRUE
  }
  for (i in 1:nrow(mat2)) {
    if (left.border) 
      write(paste(paste(c("", mat2[i, ]), collapse = hsep), 
                  hsep, sep = ""), file = file, append = append)
    else
      write(paste(paste(mat2[i, ], collapse = hsep), hsep, 
                  sep = ""), file = file, append = append)
    append <- TRUE

    ## print separator if row prefix is not same as next one
    if (rpref[i] != rpref[i + 1] && line.sep != '') 
      write(line.sep, file = file, append = TRUE)
  }
}

unPaste <- function(str, sep='/')
{
  w <- strsplit(str, sep)
  w <- matrix(unlist(w), ncol=length(str))
  nr <- nrow(w)
  ans <- vector('list', nr)
  for(j in 1:nr)
    ans[[j]] <- w[j,]
  ans
}

get2rowHeads <- function(str) {
  w <- strsplit(str, '\n')
  ## strsplit returns character(0) when element=""  23may03
  list(sapply(w, function(x)if(length(x))    x[[1]] else ''),
       sapply(w, function(x)if(length(x) > 1)x[[2]] else ''))
}


## Note: can't say f[vector of names] <- list(...) to update args
## In R you have to put ALL arguments in list(...) so sometimes we set
## unneeded ones to NULL.  Ignore this assignment in S<

## Two lists of functions, one for primitives for S+ or R (either Trellis
## or low-level), one for R grid
## Note: rect is only defined in R, not S+
ordGridFun <- function(grid)
{
  if(!grid)
    list(lines    = function(...) lines(...),
         points   = function(..., size=NULL)
                    {
                      if(length(size))
                        warning('size not implemented yet')
                      points(...)
                    },
         text     = function(...) text(...),
         segments = function(...) segments(...),
         arrows   = function(..., open, size) arrows(..., length=size*.8),
         rect     = function(...) rect(...),
         polygon  = function(x, y=NULL, ..., type=c('l','s'))
         {
           type <- match.arg(type)
           if(!length(y))
             {
               y <- x$y
               x <- x$x
             }
           j <- !is.na(x+y)
           x <- x[j]
           y <- y[j]
           if(type=='s') polygon(makeSteps(x, y), ..., border=NA)
           else polygon(x, y, ..., border=NA)
         },
         abline   = function(...) abline(...),
         unit     = function(x, units='native')
                    {
                      if(units!='native')
                        stop('units="native" is only units implemented outside of grid')
                      x
                    },
         axis     = function(...) axis(...))
  else {
    sRequire('lattice')
    list(lines = function(x, y, ...)
         {
           if(is.list(x)) {
             y <- x[[2]]; x <- x[[1]]
           }
           lattice::llines(if(is.unit(x))
                    convertX(x, 'native', valueOnly=TRUE)
                  else x,
                  if(is.unit(y))
                    convertY(y, 'native', valueOnly=TRUE)
                  else y,
                  ...)
         },

         points = function(x, y, ...)
         {
           if(is.list(x)) {
             y <- x[[2]]; x <- x[[1]]
           }
           lattice::lpoints(if(is.unit(x))
                     convertX(x, 'native', valueOnly=TRUE)
                   else x,
                   if(is.unit(y))
                   convertY(y, 'native', valueOnly=TRUE)
                   else y,
                   ...)
         },

         text = function(x, y, ...)
         {
           if(is.list(x)) {
             y <- x[[2]]; x <- x[[1]]
           }
           lattice::ltext(if(is.unit(x))
                   convertX(x, 'native', valueOnly=TRUE)
                 else x,
                 if(is.unit(y))
                   convertY(y, 'native', valueOnly=TRUE)
                 else y,
                 ...)
         },

         segments = function(x0, y0, x1, y1, ...)
         {
           grid.segments(x0, y0, x1, y1, default.units='native',
                         gp=gpar(...))
         },
       
         arrows = function(...) lattice::larrows(...),

         rect = function(xleft, ybottom, xright, ytop, density, angle,
                         border, xpd, ...)
         {
           grid.rect(xleft, ybottom, width=xright-xleft,
                     height=ytop-ybottom, just='left',
                     default.units='native', gp=gpar(...))
         },
         polygon  = function(x, y=NULL, col=par('col'), type=c('l','s'), ...)
         {
           type <- match.arg(type)
           if(!length(y))
             {
               y <- x$y
               x <- x$x
             }
           j <- !is.na(x+y)
           x <- x[j]
           y <- y[j]
           if(type=='s') grid.polygon(makeSteps(x, y),
                default.units='native',
                gp=gpar(fill=col, col='transparent', ...))
           else grid.polygon(x, y, default.units='native',
                      gp=gpar(fill=col,col='transparent',...))
              },
         abline=function(...) lattice::panel.abline(...),
         unit = function(x, units='native', ...) unit(x, units=units, ...),
       
         axis = function(side=1, at=NULL, labels, ticks=TRUE,
                         distn, line, pos, outer, ...)
         {
           if(!length(at))stop('not implemented for at= unspecified')
           if(side > 2) stop('not implemented for side=3 or 4')
           ## ticks=ticks removed from grid.?axis FEH 30Aug09
           if(side==1) grid.xaxis(at=at, label=labels, gp=gpar(...))
           if(side==2) grid.yaxis(at=at, label=labels, gp=gpar(...))
         })
  }
}

parGrid <- function(grid=FALSE)
{
  pr <- par()
  cin <- pr$cin
  cex <- pr$cex
  lwd <- pr$lwd
  if(grid) {
    ## cvp <- current.viewport()
    ## usr <- c(cvp$xscale, cvp$yscale)
    usr <- c(convertX(unit(0:1, "npc"), "native", valueOnly=TRUE),
             convertY(unit(0:1, "npc"), "native", valueOnly=TRUE))

    pin <- 
      c(convertWidth(unit(1, "npc"), "inches", valueOnly=TRUE),
        convertHeight(unit(1, "npc"), "inches", valueOnly=TRUE))

    uin <- 
      c(convertWidth(unit(1, "native"), "inches", valueOnly=TRUE),
        convertHeight(unit(1, "native"), "inches", valueOnly=TRUE))
    
  }
  else {
    usr <- pr$usr
    pin <- pr$pin
    uin <- c(pin[1]/(usr[2]-usr[1]), pin[2]/(usr[4]-usr[3]))
    ## 22Mar01 - R does not have par(uin)
  }
  list(usr=usr, pin=pin, uin=uin, cin=cin, cex=cex, lwd=lwd)
}

## Replaces R's xinch, yinch, extending them to grid
## These convert inches to data units
xInch <- function(x=1, warn.log=!grid, grid=FALSE)
{
  if (warn.log && par("xlog"))
    warning("x log scale:  xInch() is nonsense")
  pr <- parGrid(grid)
  x * diff(pr$usr[1:2])/pr$pin[1]
}

yInch <- function (y = 1, warn.log=!grid, grid=FALSE)
{
  if (warn.log && par("ylog"))
    warning("y log scale:  yInch is nonsense")
  pr <- parGrid(grid)
  y * diff(pr$usr[3:4])/pr$pin[2]
}

  na.include <- function(obj) {
    if(inherits(obj,'data.frame'))
      for(i in seq(along=obj))
        obj[[i]] <- na.include(obj[[i]])
    else {
      if(length(levels(obj)) && any(is.na(obj)))
        obj <- factor(obj,exclude=NULL)
    }
    obj
  }


if(FALSE) {
  whichClosest <- function(x, w)
  {
    ## x: vector of reference values
    ## w: vector of values to find closest matches in x
    ## Returns: subscripts in x corresponding to w
    i <- order(x)
    x <- x[i]
    n <- length(x)
    br <- c(-1e30, x[-n]+diff(x)/2,1e30)
    m <- length(w)
    i[.C("bincode", as.double(w), m, as.double(br),
         length(br), code = integer(m), right = TRUE, 
         include = FALSE, NAOK = TRUE, DUP = FALSE, 
         PACKAGE = "base")$code]
  }
  NULL
}

## Just as good, ties shuffled to end
## function(x, w) round(approx(x,1:length(x),xout=w,rule=2,ties='ordered')$y)
## Remove ties= for S-Plus.  Note: does not work when 2nd arg to
## approx is not uniformly spaced
## NO! ties='ordered' bombs in x not ordered
## Try
## approx(c(1,3,5,2,4,2,4),1:7,xout=c(1,3,5,2,4,2,4),rule=2,ties=function(x)x[1])
## NO: only works in general if both x and y are already ordered


## The following runs the same speed as the previous S version (in R anyway)
whichClosest <- function(x, w)
{
  ## x: vector of reference values
  ## w: vector of values for which to lookup closest matches in x
  ## Returns: subscripts in x corresponding to w
  ## Assumes no NAs in x or w
  .Fortran(F_wclosest,as.double(w),as.double(x),
           length(w),length(x),
           j=integer(length(w)))$j
}

whichClosePW <- function(x, w, f=0.2) {
  lx <- length(x)
  lw <- length(w)
  .Fortran(F_wclosepw,as.double(w),as.double(x),
           as.double(runif(lw)),as.double(f),
           lw, lx, double(lx), j=integer(lw))$j
}              

whichClosek <- function(x, w, k) {
  ## x: vector of reference values
  ## w: vector of values for which to lookup close matches in x
  ## Returns: subscripts in x corresponding to w
  ## Assumes no NAs in x or w
  ## First jitters x so there are no ties
  ## Finds the k closest matches and takes a single random pick of these k
  y <- diff(sort(x))
  mindif <- if(all(y == 0)) 1 else min(y[y > 0])
  x <- x + runif(length(x), -mindif/100, mindif/100)
  z <- abs(outer(w, x, "-"))
  s <- apply(z, 1, function(u) order(u)[1:k])
  if(k == 1) return(s)
  apply(s, 2, function(u) sample(u, 1))
}
                        
if(FALSE) {
  sampWtdDist <- function(x, w)
  {
    ## x: vector of reference values
    ## w: vector of values to find closest matches in x
    ## Returns: subscripts in x corresponding to w

    ## 25% slower but simpler method:
    ## z <- abs(outer(w, x, "-"))
    ## s <- apply(z, 1, max)
    ## z <- (1 - sweep(z, 1, s, FUN='/')^3)^3
    ## sums <- apply(z, 1, sum)
    ## z <- sweep(z, 1, sums, FUN='/')

    lx <- length(x)
    lw <- length(w)
    z <- matrix(abs( rep( x , lw ) - rep( w, each = lx ) ),
                nrow=lw, ncol=lx, byrow=TRUE) ## Thanks: Chuck Berry
    ## s <- pmax( abs( w - min(x) ), abs( w - max(x) ) )  # to use max dist
    s <- rowSums(z)/lx/3   # use 1/3 mean dist for each row
    tricube <- function(u) (1 - pmin(u,1)^3)^3
    ## z <- (1 - (z/rep(s,length.out=lx*lw))^3)^3   # Thanks: Tim Hesterberg
    z <- tricube(z/s)   # Thanks: Tim Hesterberg
    sums <- rowSums(z)
    z <- z/sums 
    as.vector(rMultinom(z, 1))
  }
  NULL
}

approxExtrap <- function(x, y, xout, method='linear', n=50, rule=2,
                         f=0, ties='ordered', na.rm=FALSE)
{
  ## Linear interpolation using approx, with linear extrapolation
  ## beyond the data
  if(is.list(x)) {
    y <- x[[2]]; x <- x[[1]]
  }

  ## remove duplicates and order so can do linear extrapolation
  if(na.rm) {
    d <- ! is.na(x + y)
    x <- x[d]; y <- y[d]
  }

  x <- as.numeric(x)  # handles dates etc.
  y <- as.numeric(y)
  
  d <- ! duplicated(x)
  x <- x[d]
  y <- y[d]
  d <- order(x)
  x <- x[d]
  y <- y[d]
  
  w <- approx(x, y, xout=xout, method=method, n=n,
              rule=2, f=f, ties=ties)$y
  
  r <- range(x)
  d <- xout < r[1]
  if(any(is.na(d)))
    stop('NAs not allowed in xout')
  
  if(any(d))
    w[d] <- (y[2]-y[1])/(x[2]-x[1])*(xout[d]-x[1])+y[1]
  
  d <- xout > r[2]
  n <- length(y)
  if(any(d))
    w[d] <- (y[n]-y[n-1])/(x[n]-x[n-1])*(xout[d]-x[n-1])+y[n-1]
  
  list(x=xout, y=w)
}


inverseFunction <- function(x, y) {
  d <- diff(y)
  xd <- x[-1]
  dl <- c(NA, d[-length(d)])
  ic <- which(d>=0 & dl<0 | d>0 & dl<=0 | d<=0 & dl>0 | d<0 & dl>=0)
  nt <- length(ic)
  k <- nt + 1
  if(k==1) {
    h <- function(y, xx, yy, turns, what, coef)
      approx(yy, xx, xout=y, rule=2)$y
    formals(h) <- list(y=numeric(0), xx=x, yy=y, turns=numeric(0),
                       what=character(0), coef=numeric(0))
  return(h)
  }
  turns <- x[ic]
  turnse <- c(-Inf, turns, Inf)
  xrange <- yrange <- matrix(NA, nrow=k, ncol=2)
  for(j in 1:k) {
    l <- which(x >= turnse[j] & x <= turnse[j+1])
    xrange[j,] <- x[l[c(1,length(l))]]
    yrange[j,] <- y[l[c(1,length(l))]]
  }

  for(j in 1:length(ic)) {
    l <- (ic[j]-1):(ic[j]+1)
    turns[j] <- approxExtrap(d[l], xd[l], xout=0, na.rm=TRUE)$y
  }

  hh <- function(y, xx, yy, turns, xrange, yrange, what, coef) {
    what <- match.arg(what)
    ## Find number of monotonic intervals containing a given y value
    ylo <- pmin(yrange[,1],yrange[,2])
    yhi <- pmax(yrange[,1],yrange[,2])
    n <- outer(y, ylo, function(a,b) a >= b) &
         outer(y, yhi, function(a,b) a <= b)
    ## Columns of n indicate whether or not y interval applies
    ni <- nrow(yrange)
    fi <- matrix(NA, nrow=length(y), ncol=ni)
    turnse <- c(-Inf, turns, Inf)
    for(i in 1:ni) {
      w <- n[,i]
      if(any(w)) {
        l <- xx >= turnse[i] & xx <= turnse[i+1]
        fi[w,i] <- approx(yy[l], xx[l], xout=y[w])$y
      }
    }
    noint <- !apply(n, 1, any)
    if(any(noint)) {
      ## Determine if y is closer to yy at extreme left or extreme right
      ## of an interval
      m <- length(yy)
      yl <- as.vector(yrange); xl <- as.vector(xrange)
      fi[noint,1] <- xl[whichClosest(yl, y[noint])]
    }
    if(what=='sample')
      apply(fi, 1, function(x) {
       z <- x[!is.na(x)]
       if(length(z)==1) z else if(length(z)==0) NA else sample(z, size=1)
       }) else fi
  }
  formals(hh) <- list(y=numeric(0), xx=x, yy=y, turns=turns,
                      xrange=xrange, yrange=yrange,
                      what=c('all', 'sample'), coef=numeric(0))
  ## coef is there for compatibility with areg use
  hh
}

Names2names <- function(x)
{
  if(is.list(x)) {
  }
  else {
    n <- names(attributes(x))
    if(any(n=='.Names'))
      names(attributes(x)) <- ifelse(n=='.Names','names',n)
  }
  x
}

##xedit <- function(file, header, title, delete.file=FALSE) {
## In R, use e.g. options(pager=xedit); page(x,'p')
##  sys(paste('xedit -title "', title, '" ', file, ' &',
##            sep=''))
##  invisible()
##}

if(FALSE) {
  gless <- function(x, ...)
  {
    ## Usage: gless(x) - uses print method for x, puts in window with
    ## gless using name of x as file name prefixed by ~, leaves window open
    nam <- substring(deparse(substitute(x)), 1, 40)
    file <- paste('/tmp/',nam,sep='~')  #tempfile('Rpage.')
    sink(file)
    ##  cat(nam,'\n' )
    ##  if(length(attr(x,'label')) && !inherits(x,'labelled'))
    ##    cat(attr(x,'label'),'\n')
    ##  cat('\n')
    print(x, ...)
    sink()
    sys(paste('gless --geometry=600x400 "',file,'" &',sep=''))
    ## gless does not have a title option
    invisible()
  }
  NULL
}

xless <-
  function(x, ..., title=substring(deparse(substitute(x)), 1, 40))
{
  ## Usage: xless(x) - uses print method for x, puts in persistent window with
  ## xless using name of x as title (unless title= is specified)
  ## If running under MacOS, use the system open command instead of xless
  mac <- Sys.info()['sysname'] == 'Darwin'
	file <- if(mac) paste(tempdir(), makeNames(title), sep='/') else tempfile()
  capture.output(x, ..., file=file)
  cmd <- if(mac)
           paste('open -a TextEdit', file) else
           paste('xless -title "', title, '" -geometry "90x40" "',
                  file, '" &', sep='')
  system(cmd)
invisible()
}


pasteFit <- function(x, sep=',', width=.Options$width)
{
  ## pastes as many elements of character vector x as will fit in a line
  ## of width 'width', starting new lines when needed
  ## result is the lines of pasted text
  m <- nchar(x)
  out <- character(0)
  cur <- ''
  n   <- 0
  for(i in 1:length(x)) {
    if(cur=='' | (m[i] + nchar(cur) <= width))
      cur <- paste(cur, x[i],
                   sep=if(cur=='')''
                       else sep)
    else {
      out <- c(out, cur)
      cur <- x[i]
    }
  }
  if(cur != '') out <- c(out, cur)
  out
}

## Determine if variable is a date, time, or date/time variable in R.
## The following 2 functions are used by describe.vector
## timeUsed assumes is date/time combination variable and has no NAs
testDateTime <- function(x, what=c('either','both','timeVaries'))
{
  what <- match.arg(what)
  cl <- class(x)
  if(!length(cl))
    return(FALSE)

  dc <- c('Date', 'POSIXt','POSIXct','dates','times','chron')
  
  dtc <- c('POSIXt','POSIXct','chron')
  
  switch(what,
         either = any(cl %in% dc),
         both   = any(cl %in% dtc),
         timeVaries = {
           if('chron' %in% cl || 'Date' %in% cl) { 
             ## chron or S+ timeDate
             y <- as.numeric(x)
             length(unique(round(y - floor(y),13))) > 1
           }
           else length(unique(format(x,'%H%M%S'))) > 1
         })
}

## Format date/time variable from either R or S+
## x = a numeric summary of the original variable (e.g., mean)
## at = attributes of original variable
formatDateTime <- function(x, at, roundDay=FALSE)
{
  cl <- at$class
  w <- if(any(cl %in% c('chron','dates','times'))){
         attributes(x) <- at
         fmt <- at$format
         if(roundDay) {
           if (!requireNamespace("chron", quietly = TRUE))
             stop("'roundDay = TRUE' requires the 'chron' package.")
           if(length(fmt)==2 && is.character(fmt))
             format(chron::dates(x), fmt[1])
           else
             format(chron::dates(x))
         }
         else x
       } else {
         attributes(x) <- at
         if(roundDay && 'Date' %nin% at$class) 
           as.POSIXct(round(x, 'days'))
         else x
       }
  format(w)
}

## Try to guess whether a factor or character variable is a date, and
## allow for partial dates of the form YYYY and mm/YYYY, the former
## only used if at least one observation has a month in it.  If a minority
## fraction of observations fracnn or less is not convertable to a date,
## set those observations to NA
## Allows for mixing of common date forms across observations
##
## Example:
##
## x <- convertPdate(c(rep('2019-03-04',7), '2018', '03/2018', 'junk', '3/11/17','2017-01-01','2017-01-01', NA, ''))
## x
## describe(x)


convertPdate <- function(x, fracnn=0.3, considerNA=NULL) {
  xo <- x
  if(is.factor(x)) x <- as.character(x)
  if(! is.character(x)) return(xo)
  x <- trimws(x)
  if(all(is.na(x)) || all(x =='')) return(xo)

  ymd <- grepl('^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$', x)
  mdy <- grepl('^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$', x)
  y   <- grepl('^[0-9]{4}$', x)
  my  <- grepl('^[0-9]{1,2}/[0-9]{4}$', x)
  m   <- sum(x %nin% c('', considerNA) & ! is.na(x)) * (1 - fracnn)
  dny <- ymd | mdy | my   # date other than just YYYY
  d   <- dny | (y & any(dny))
  ## The variable is a date if at least m values are dates
  if(sum(d) < m) return(xo)
  special <- obs <- NULL
  ndnm <- ! d & ! is.na(x)   # not date and not missing
  if(any(ndnm)) {
    special <- setdiff(x[ndnm], c('', ' '))
    obs     <- x %in% special
    x[ndnm] <- NA    # values such as text comments
    }

  x <- ifelse(y, paste0(x, '-07-03'),
        ifelse(my, gsub('^([0-9]{1,2})/([0-9]{4})$', '\\2-\\1-15', x),
         ifelse(ymd, x,
          ifelse(mdy, gsub('^([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})$',
                               '\\3-\\1-\\2', x), NA))))

  x <- as.Date(x)
  if(length(special))
    attr(x, 'special.miss') <- list(codes=special, obs=obs)
  if(any(y | my)) attr(x, 'imputed') <- which(y | my)
  x
}

getHdata <-
  function(file, what=c('data','contents','description','all'),
           where='https://hbiostat.org/data/repo') {
    what <- match.arg(what)
    fn <- as.character(substitute(file))
    localrepo <- .Options$localHfiles
    localrepo <- length(localrepo) && is.logical(localrepo) && localrepo
    if(localrepo) where <- '~/web/data/repo'
    
    ads <- readLines(paste0(where, '/Rcontents.txt'))
    a <- unlist(strsplit(ads,'.sav|.rda'))
    if(missing(file)) return(a)
    
    wds <- paste(substitute(file), c('rda','sav'), sep='.')
    if(!any(wds %in% ads))
      stop(paste(paste(wds, collapse=','),
                 'are not on the web site.\nAvailable datasets:\n',
                 paste(a, collapse=' ')))
    wds <- wds[wds %in% ads]
    if(what %in% c('contents','all')) {
      w <- paste(if(fn=='nhgh')'' else 'C',fn,'.html',sep='')
      browseURL(paste(where, w, sep='/'))
    }
    
    if(what %in% c('description','all')) {
      ades <- scan(paste(where,'Dcontents.txt',sep='/'),list(''),
                   quiet=TRUE)[[1]]
      i <- grep(paste(fn,'\\.',sep=''),ades)
      if(!length(i))
        warning(paste('No description file available for',fn))
      else {
        w <- ades[i[1]]
        browseURL(paste(where, w, sep='/'))
      }
    }
    
    if(what %nin% c('data','all'))
      return(invisible())
    
    f <- paste(where, wds, sep='/')
    if(length(f) > 1)
      warning(paste('More than one file matched; using the first:',
                    paste(f, collapse=', ')))
    if(localrepo) return(invisible(load(f, .GlobalEnv)))
    tf <- tempfile()
    download.file(f, tf, mode='wb', quiet=TRUE)
    load(tf, .GlobalEnv)
    invisible()
  }

hdquantile <- function(x, probs=seq(0, 1, 0.25), se=FALSE,
                       na.rm=FALSE, names=TRUE, weights=FALSE)
{
  if(na.rm) {
    na <- is.na(x)
    if(any(na))
      x <- x[!na]
  }
  
  x <- sort(x, na.last=TRUE)
  n <- length(x)
  if(n < 2)
    return(rep(NA, length(probs)))
  
  m  <- n + 1

  ps <- probs[probs > 0 & probs < 1]
  qs <- 1 - ps

  a <- outer((0:n)/n, ps,
             function(x,p,m) pbeta(x, p*m, (1-p)*m), m=m)
  w <- a[-1,,drop=FALSE] - a[-m,,drop=FALSE]

  r <- drop(x %*% w)
  rp <- range(probs)
  pp <- ps
  if(rp[1]==0) {
    r <- c(x[1], r); pp <- c(0,pp)
  }

  if(rp[2]==1) {
    r <- c(r, x[n]); pp <- c(pp,1)
  }
  
  r <- r[match(pp, probs)]

  if(names) names(r) <- format(probs)

if(weights)
  attr(r,'weights') <- structure(w, dimnames=list(NULL,format(ps)))

  if(!se)
    return(r)
  if(n < 3)
    stop('must have n >= 3 to get standard errors')

  l <- n - 1
  a <- outer((0:l)/l, ps,
             function(x,p,m) pbeta(x, p*m, (1-p)*m), m=m)
  w <- a[-1,,drop=FALSE] - a[-n,,drop=FALSE]

  storage.mode(x) <- 'double'
  storage.mode(w) <- 'double'

  nq <- length(ps)
  ## Get all n leave-out-one quantile estimates
  S <- matrix(.Fortran(F_jacklins, x, w, as.integer(n), as.integer(nq),
                       res=double(n*nq))$res, ncol=nq)

  se <- l * sqrt(diag(var(S))/n)

  if(rp[1]==0)
    se <- c(NA, se)
  
  if(rp[2]==1)
    se <- c(se, NA)
  
  se <- se[match(pp,probs)]
  if(names)
    names(se) <- names(r)
  
  attr(r, 'se') <- se
  r
}

sepUnitsTrans <- function(x, 
                          conversion=c(day=1, month=365.25/12, year=365.25, week=7),
                          round=FALSE, digits=0)
{
  if(!any(is.present(x)))
    return(x)
  
  target <- names(conversion[conversion==1])
  if(!length(target))
    stop('must specify a target unit with conversion factor=1')
  
  lab <- attr(x,'label')
  x <- ifelse(is.present(x),casefold(as.character(x)),'')

  for(w in names(conversion)) {
    i <- grep(w, x)
    if(length(i)) x[i] <-
      as.character(as.numeric(gsub(paste(w,'s*',sep=''), '', x[i]))*
                   conversion[w])
  }

  i <- grep('[a-z]', x)
  if(any(i))
    warning(paste('variable contains units of measurement not in',
                  paste(names(conversion), collapse=','),':',
                  paste(unique(x[i]),collapse=' ')))
  
  x <- as.numeric(x)
  if(round)
    x <- round(x, digits)
  
  units(x) <- target
  if(length(lab))
    label(x) <- lab
  x
}

makeNames <- function(names, unique=FALSE, allow=NULL)
{
  ## Runs make.names with exceptions in vector allow
  ## By default, R 1.9 make.names is overridden to convert _ to . as
  ## with S-Plus and previous versions of R.  Specify allow='_' otherwise.
  n <- make.names(names, unique)
  if(!length(allow))
    n <- gsub('_', '.', n)
  n
}

Load <- function(object)
{
  nam <- deparse(substitute(object))
  path <- .Options$LoadPath
  if(length(path))
    path <- paste(path,'/',sep='')
  file <- paste(path, nam, '.rda', sep='')
  load(file, .GlobalEnv)
}

Save <- function(object, name=deparse(substitute(object)), compress=TRUE)
{
  path <- .Options$LoadPath
  if(length(path))
    path <- paste(path, '/', sep='')
  
  .FileName <- paste(path, name, '.rda', sep='')
  assign(name, object)
  if(is.logical(compress) && compress) compress <- 'gzip'
  eval(parse(text=paste('save(', name, ', file="',
                        .FileName, '", compress="', compress, '")', sep='')))
}

getZip <- function(url, password=NULL) {
  ## Allows downloading and reading a .zip file containing one file
  ## File may be password protected.  Password will be requested unless given.
  ## Password is 'foo'
  ## url may also be a local file
  ## Note: to make password-protected zip file z.zip, do zip -e z myfile
  if(grepl("^https?://", tolower(url))) {
    f <- tempfile()
    download.file(url, f)
  } else f <- url
  cmd <- if(length(password))
           paste('unzip -p -P', password) else 'unzip -p'
  pipe(paste(cmd, f))
}

getLatestSource <- function(x=NULL, package='Hmisc',
                            recent=NULL, avail=FALSE) {
  urlf  <- paste0('https://hbiostat.org/R/', package, '/dir.txt')
  fs    <- scan(urlf, what=list('', ''), sep=' ', quiet=TRUE)
  dates <- fs[[1]]
  files <- fs[[2]]
  
  url <- if(length(recent))
           paste0('https://github.com/harrelfe/', package, '/commits/master/R')
           else
             paste0('https://github.com/harrelfe/', package, '/tree/master/R/')

  if(avail) return(data.frame(file=files, date=as.Date(dates)))

  if(length(recent)) x <- files[1:recent]
  if(length(x)==1 && x=='all') x <- files
  for(fun in x) {
    i <- which(files==fun)
    if(!length(i)) stop(paste('no file ', fun,' in ',package, sep=''))
    cat('Fetching', fun, dates[i],'\n')
    url <- paste0('https://raw.githubusercontent.com/harrelfe/', package,
                  '/master/R/', fun)
    source(url)
  }
}
  
clowess <- function(x, y=NULL, iter=3, ...) {
  ## to get around bug in lowess with occasional wild values with iter>0
  r <- range(if(length(y)) y else x$y)
  f <- lowess(x, y, iter=iter, ...)
  if(iter != 0 && any(f$y < r[1] | f$y > r[2]))
    f <- lowess(x, y, iter=0)
  f
}

prselect <- function(x, start=NULL, stop=NULL, i=0, j=0, pr=TRUE)
  {
    f <- function(pattern, x)
      {
        y <- grep(pattern, x)
        if(length(y) > 1) y <- y[1]
        y
      }
    lx <- length(x)
    k <- if(length(start)) f(start, x) else 1
    if(length(k))
      {
        k <- k + i
        m <- if(length(stop))
          {
            w <- f(stop, x[k:lx])
            if(length(w)) w + k - 1 + j else -1
          }
        else lx
        if(m > 0) x <- if(k==1) (if(m==lx) '...' else c('...', x[-(k:m)]))
        else
          {
            if(m==lx) c(x[-(k:m)], '...')
            else c(x[1:(k-1)], '...', x[(m+1):lx])
          }
      }
    else # no start specified; keep lines after stop
      {
        m <- f(stop, x)
        if(length(m) > 0)
          {
            m <- if(length(m)) m + j - 1 else lx
            x <- if(m==lx) '...' else c('...', x[-(1:m)])
          }
      }
    if(pr) cat(x, sep='\n')
    invisible(x)
  }

## The following is taken from survival:::plot.survfit internal dostep function
## Remove code to remove duplicates in y

makeSteps <- function(x, y)
{
  if (is.na(x[1] + y[1]))
    {
      x <- x[-1]
      y <- y[-1]
    }
  n <- length(x)
  if (n > 2)
    {
      xrep <- rep(x, c(1, rep(2, n - 1)))
      yrep <- rep(y, c(rep(2, n - 1), 1))
      list(x = xrep, y = yrep)
    }
  else if (n == 1)
    list(x = x, y = y)
  else list(x = x[c(1, 2, 2)], y = y[c(1, 1, 2)])
}

latexBuild <- function(..., insert=NULL, sep='') {
  w <- list(...)
  l <- length(w)
  if(l %% 2 != 0) stop('# arguments must be multiple of 2')
  k <- l / 2
  j <- 1
  txt <- op <- character(0)
  for(i in 1 : k) {
    a <- w[[j]]
    if(length(a)) {
      txt <- c(txt, a)
      if(w[[j + 1]] != '') op <- c(op,  w[[j + 1]])
    }
    j <- j + 2
  }
  txt <- paste(txt, collapse=sep)
  w <- character(0)
  close <- if(length(op)) {
    for(y in rev(op)) {
      if(length(insert))
        for(ins in insert)
          if(length(ins) &&
             ins[[1]] == y && ins[[2]] == 'before')
            w <- c(w, '\n', ins[[3]])
      w <- c(w,
             if(y == '(') ')'
             else if(y == '{') '}'
             else if(y == '[') ']'
             else sprintf('\\end{%s}', y))
      if(length(insert))
        for(ins in insert)
          if(length(ins) &&
             ins[[1]] == y && ins[[2]] == 'after')
            w <- c(w, '\n', ins[[3]])
    }
    paste(w, collapse=sep)
  }
  structure(txt, close=close)
}

getRs <- function(file=NULL,
                  guser='harrelfe', grepo='rscripts',
                  gdir='raw/master', dir=NULL,
                  browse=c('local', 'browser'), cats=FALSE,
                  put=c('source', 'rstudio')) {
  
  browse <- match.arg(browse)
  put    <- match.arg(put)

  localrepo <- .Options$localHfiles
  localrepo <- length(localrepo) && is.logical(localrepo) && localrepo
  if(localrepo) where <- '~/R/rscripts'
  else {
    where  <- paste('https://github.com', guser, grepo, gdir, sep='/')
    if(length(dir)) where <- paste(where, dir, sep='/')
    }
  
  trim <- function(x) sub('^[[:space:]]+','',sub('[[:space:]]+$','', x))

  pc <- function(s) {
    wr <- function(x) {
      n <- length(x)
      z <- character(n)
      for(i in 1 : n) z[i] <- paste(strwrap(x[i], width=15), collapse='\n')
      z
    }
    s <- with(s, cbind(Major = wr(Major),
                       Minor = wr(Minor),
                       File  = wr(File),
                       Type  = wr(Type),
                       Description = wr(Description)))
    print.char.matrix(s, col.names=TRUE)
  }

  read.table.HTTPS <- function(url) {
    res <- tryCatch(read.table(url,
                               sep='|', quote='', header=TRUE, as.is=TRUE), 
                    error=function(e) e)
    if(inherits(res, "simpleError")) {
      if(res$message == "https:// URLs are not supported") {
        res$message <- paste(res$message, "Try installing R version >= 3.2.0", sep="\n\n")
      }
      stop(res)
    }
    res
  }

  download.file.HTTPS <- function(url, file, method='libcurl', 
                                  quiet=TRUE, extra='--no-check-certificate') {
    res <- tryCatch(download.file(url, file, method, quiet=quiet, extra=extra), 
                    error=function(e) e)
    if(inherits(res, "simpleError")) {
      if(res$message == "download.file(method = \"libcurl\") is not supported on this platform") {
        warning(paste(res$message, "Try installing R version >= 3.2.0", "Attempting method=\"wget\"", sep="\n\n"))
        return(download.file.HTTPS(url, file, method='wget'))
      }
      if(res$message == "https:// URLs are not supported") {
        res$message <- paste(res$message, "Try installing R version >= 3.2.0", sep="\n\n")
      }
      stop(res)
    }
    invisible(res)
  }
  
  if(! length(file)) {
    s <- read.table.HTTPS(paste(where, 'contents.md', sep='/'))
    s <- s[-1,]
    names(s) <- c('Major', 'Minor', 'File', 'Type', 'Description')
    sd <- s; n <- nrow(s)   # sd = s with dittoed items duplicated
    for(x in c('Major', 'Minor')) {
      u <- v <- gsub('\\*\\*', '', trim(s[[x]]))
      for(i in 2 : n) if(u[i] == '"') u[i] <- u[i - 1]
      v <- gsub('"', '', v)
      s[[x]] <- v; sd[[x]] <- u
    }
    s$File        <- trim(gsub('\\[(.*)\\].*', '\\1', s$File))
    d <- trim(gsub('\\[.*\\]\\(.*\\)', '', s$Description))
    s$Description <- gsub('\\[report\\].*', '', d)

    if(is.logical(cats)) {
      if(cats) {
        ## List all major and minor categories
        maj <- sort(unique(sd$Major))
        min <- setdiff(sort(unique(sd$Minor)), '')
        cat('\nMajor categories:\n', maj,
            '\nMinor categories:\n', min, '', sep='\n')
        return(invisible(list(Major=maj, Minor=min)))
      }
    } else {  ## list all scripts whose "first hit" major category contains cats
        i <- grepl(tolower(cats), tolower(sd$Major))
        if(! any(i)) cat('No scripts with', cats, 'in major category\n')
        else pc(s[i, ])
        return(invisible(s[i, ]))
      }
    if(browse == 'local') pc(s)
    else
      browseURL(if(localrepo) '~/R/rscripts/contents.md'
                else
                  'https://github.com/harrelfe/rscripts/blob/master/contents.md')
    return(invisible(s))
  }

  if(put == 'source')
    return(invisible(source(paste(where, file, sep='/'))))

  if(localrepo) file.copy(paste(where, file, sel='/'), file)
  else download.file.HTTPS(paste(where, file, sep='/'), file)
  if(requireNamespace('rstudioapi', quietly=TRUE) &&
     rstudioapi::isAvailable()) rstudioapi::navigateToFile(file)
  else file.edit(file)
  invisible()
}

knitrSet <-
  function(basename  = NULL,
           w=if(! bd) 4,
           h=if(! bd) 3,
           wo=NULL, ho=NULL,
           fig.path  = if(length(basename)) basename else '',
           fig.align = if(! bd) 'center',
           fig.show  = 'hold',
           fig.pos   = if(! bd) 'htbp',
           fig.lp    = if(! bd) paste('fig', basename, sep=':'),
           dev       = switch(lang,
                              latex='pdf', markdown='png',
                              blogdown=NULL, quarto=NULL),
           tidy=FALSE, error=FALSE,
           messages=c('messages.txt', 'console'),
           width=61, decinline=5, size=NULL, cache=FALSE,
           echo=TRUE, results='markup', capfile=NULL,
           lang=c('latex','markdown','blogdown','quarto')) {

  if(! requireNamespace('knitr')) stop('knitr package not available')
  
  messages <- match.arg(messages)
  lang     <- match.arg(lang)
  options(knitrSet.lang = lang)
  bd       <- lang %in% c('blogdown', 'quarto')
  
  ## Specify e.g. dev=c('pdf','png') or dev=c('pdf','postscript')
  ## to produce two graphics files for each plot
  ## But: dev='CairoPNG' is preferred for png
  if(length(basename)) basename <- paste(basename, '-', sep='')

  ## Default width fills Sweavel boxes when font size is \small and svmono.cls
  ## is in effect (use 65 without svmono)

  if(lang == 'latex') knitr::render_listings()
  
  if(messages != 'console') {
    unlink(messages) # Start fresh with each run
    hook_log = function(x, options) cat(x, file=messages, append=TRUE)
    knitr::knit_hooks$set(warning = hook_log, message = hook_log)
  }
  else
    knitr::opts_chunk$set(message=FALSE, warning=FALSE)
  
  if(length(size)) knitr::opts_chunk$set(size = size)
  ## For htmlcap see http://stackoverflow.com/questions/15010732
  ## Causes collisions in html and plotly output; Original (no better)
  ## enclosed in <p class="caption"> ... </p>
#  if(lang == 'markdown')
#    knitr::knit_hooks$set(htmlcap = function(before, options, envir) {
#      if(! before) options$htmlcap
#        htmltools::HTML(paste0('<br><div style="font-size: 75%;">',
#                               options$htmlcap, "</div><br>"))
#    })
  
  if(length(decinline)) {
    rnd <- function(x, dec) if(!is.numeric(x)) x else round(x, dec)
    formals(rnd) <- list(x=NULL, dec=decinline)
    knitr::knit_hooks$set(inline = rnd)
  }
  

  knitr::knit_hooks$set(par=function(before, options, envir)
    if(before && options$fig.show != 'none') {
      p <- c('bty','mfrow','ps','bot','top','left','rt','lwd',
             'mgp','las','tcl','axes','xpd')
      pars <- knitr::opts_current$get(p)
      pars <- pars[! is.na(names(pars))]
      ## knitr 1.6 started returning NULLs for unspecified pars
      i <- sapply(pars, function(x) length(x) > 0)
      .spar. <-
        function(mar=if(!axes)
                       c(2.25+bot-.45*multi,2*(las==1)+2+left,.5+top+.25*multi,
                         .5+rt) else
                                  c(3.25+bot-.45*multi,2*(las==1)+3.5+left,.5+top+.25*multi,
                                    .5+rt),
                 lwd = if(multi)1 else 1.75,
                 mgp = if(!axes) mgp=c(.75, .1, 0) else
                       if(multi) c(1.5, .365, 0) else c(2.4-.4, 0.475, 0),
                 tcl = if(multi)-0.25 else -0.4, xpd=FALSE, las=1,
                 bot=0, left=0, top=0, rt=0, ps=if(multi) 14 else 12,
                 mfrow=NULL, axes=TRUE, cex.lab=1.15, cex.axis=1,
                 ...) {
          multi <- length(mfrow) > 0
          par(mar=mar, lwd=lwd, mgp=mgp, tcl=tcl, ps=ps, xpd=xpd,
              cex.lab=cex.lab, cex.axis=cex.axis, las=las, ...)
          if(multi) par(mfrow=mfrow)
        }

      if(any(i)) do.call(.spar., pars[i]) else .spar.()
    })
  
    knitr::opts_knit$set(width=width)

    if(length(capfile)) {
      options(FigCapFile=capfile)
      
      cf <- function(before, options, envir) {
        if(before) return()
        lang <- getOption('knitrSet.lang')
        label   <- knitr::opts_current$get('label')
        prefx   <- if(lang == 'quarto') '' else options$fig.lp
        figname <- paste0(prefx, label)
        ## Quarto uses a chunk figure label convention fig-...
        ## and figures are referenced by @fig-...
        figref  <- if(grepl('^fig-', figname))
                     paste0('@', figname) else paste0('\\@ref(', figname, ')')
        cap     <- options$fig.cap
        scap    <- options$fig.scap
        if(length(cap) && is.call(cap))   cap <- eval(cap)
        if(length(scap) && is.call(scap)) scap <- eval(scap)
        if( ! length(scap) || scap == '') scap <- cap
        if(length(scap) && scap != '')
          cat(label, figref, paste0('"', scap, '"\n'), sep=',',
              append=TRUE, file=getOption('FigCapFile'))
      }
      knitr::knit_hooks$set(capfileFun=cf)
    }
    ## May want to see https://stackoverflow.com/questions/37116632/r-markdown-html-number-figures

  
  ## aliases=c(h='fig.height', w='fig.width', cap='fig.cap', scap='fig.scap'))
  ## eval.after = c('fig.cap','fig.scap'),
  ## error=error)  #, keep.source=keep.source (TRUE))

  ## See if need to remove dev=dev from below because of plotly graphics
  w <- list(fig.path=fig.path, fig.align=fig.align,
            fig.width=w, fig.height=h,
            out.width=wo,out.height=ho,
            fig.show=fig.show, fig.lp=fig.lp, fig.pos=fig.pos,
            dev=dev, par=TRUE, capfileFun=length(capfile) > 0,
            tidy=tidy, cache=cache,
            echo=echo, error=error, comment='', results=results)
  if(bd) w$fig.path <- NULL
  w <- w[sapply(w, function(x) length(x) > 0)]
  ## knitr doesn't like null fig.align etc.
    do.call(knitr::opts_chunk$set, w)

  if(lang %in% c('markdown', 'blogdown'))
      knitr::knit_hooks$set(uncover=markupSpecs$html$uncover)

  hook_chunk = knitr::knit_hooks$get('chunk')

  ## centering will not allow too-wide figures to go into left margin
  if(lang == 'latex') knitr::knit_hooks$set(chunk = function(x, options) { 
    res = hook_chunk(x, options) 
    if (options$fig.align != 'center') return(res) 
    gsub('\\{\\\\centering (\\\\includegraphics.+)\n\n\\}', 
         '\\\\centerline{\\1}', res) 
  }) 
  knitr::set_alias(w   = 'fig.width', h    = 'fig.height',
                   wo  = 'out.width', ho   = 'out.height',
                   cap = 'fig.cap',   scap ='fig.scap')
}
## see http://yihui.name/knitr/options#package_options

## Use caption package options to control caption font size


grType <- function() {
  if(! length(find.package('plotly', quiet=TRUE))) return('base')
	if(length(g <- .Options$grType) && g == 'plotly') 'plotly' else 'base'
}

prType <- function() {
  g <- .Options$prType
  if(! length(g)) 'plain' else g
}

htmlSpecialType <- function() {
	if(length(g <- .Options$htmlSpecialType) && g == '&') '&' else 'unicode'
}



## Save a plotly graphic with name foo.png where foo is the name of the
## current chunk
## http://stackoverflow.com/questions/33959635/exporting-png-files-from-plotly-in-r

plotlySave <- function(x, ...) {
  
  if (!requireNamespace("plotly"))
    stop("This function requires the 'plotly' package.")
  
  chunkname <- knitr::opts_current$get("label")
  path      <- knitr::opts_chunk$get('fig.path')
  if(is.list(x) & ! inherits(x, 'plotly_hash')) {
    for(w in names(x)) {
      file <- paste0(path, chunkname, '-', w, '.png')
      plotly::plotly_IMAGE(x[[w]], format='png', out_file=file, ...)
    }
  }
  else {
    file <- paste0(path, chunkname, '.png')
    plotly::plotly_IMAGE(x, format='png', out_file=file, ...)
    }
  invisible()
}

## Miscellaneous functions helpful for plotly specifications

plotlyParm = list(
  ## Needed height in pixels for a plotly dot chart given the number of
  ## rows in the chart
  heightDotchart = function(rows, per=25, low=200, high=800)
    min(high, max(low, per * rows)),

  ## Given a vector of row labels that appear to the left on a dot chart,
  ## compute the needed chart height taking label line breaks into account
  ## Since plotly devotes the same vertical space to each category,
  ## just need to find the maximum number of breaks present
  heightDotchartb = function(x, per=40,
      low=c(200, 200, 250, 300, 375)[min(nx, 5)],
      high=1700) {
    x  <- if(is.factor(x)) levels(x) else sort(as.character(x))
    nx <- length(x)
    m <- sapply(strsplit(x, '<br>'), length)
    # If no two categories in a row are at the max # lines,
    # reduce max by 1
    mx   <- max(m)
    lm   <- length(m)
    mlag <- if(lm == 1) 0 else c(0, m[1:(lm - 1)])
    if(! any(m == mx & mlag == mx)) mx <- mx - 1
    z <- 1 + (if(mx > 1) 0.5 * (mx - 1) else 0)
    min(high, max(low, per * length(x) * z))
  },

  ## Colors for unordered categories
  colUnorder = function(n=5, col=colorspace::rainbow_hcl) {
    if(! is.function(col)) rep(col, length.out=n)
    else col(n)
  },

  ## Colors for ordered levels
  colOrdered = function(n=5, col=viridisLite::viridis) {
    if(! is.function(col)) rep(col, length.out=n)
    else col(n)
  },

  ## Margin to leave enough room for long labels on left or right as
  ## in dotcharts
  lrmargin = function(x, wmax=190, mult=7) {
    if(is.character(x)) x <- max(nchar(x))
    min(wmax, max(70, x * mult))
    }
 
  )

## Function written by Dirk Eddelbuettel:
tobase64image <- function (file, Rd = FALSE, alt = "image") {
  input <- normalizePath(file, mustWork = TRUE)
  buf <- readBin(input, raw(), file.info(input)$size)
  base64 <- base64enc::base64encode(buf)
  sprintf("%s<img src=\"data:image/png;base64,\n%s\" alt=\"%s\" />%s",
          if (Rd)
            "\\out{"
          else "", base64, alt, if (Rd)
                                  "}"
                                else "")
}

plotp <- function(data, ...) UseMethod("plotp")

keepHattrib <- function(obj) {
  g <- function(x) {
    a <- attributes(x)
    i <- intersect(names(a), c('label', 'units'))
    if(length(i)) a[i]
  }
  if(! is.list(obj)) list(.single.variable.=g(obj)) else sapply(obj, g)
}

restoreHattrib <- function(obj, attribs) {
  nam <- names(obj)
  for(n in names(attribs)) {
    a <- attribs[[n]]
    if(length(a)) {
      sv <- n == '.single.variable.'
      if(sv || n %in% nam) {
        x <- if(sv) obj else obj[[n]]
        if(length(a$label)) label(x) <- a$label
        if(length(a$units)) units(x) <- a$units
        if(sv) return(x)
        obj[[n]] <- x
      }
    }
  }
  obj
}

if(FALSE) {
Hglossary <-
  list(Gmd=list('Gini\'s mean difference', 'a measure of dispersion defined as the mean absolute difference over all possible pairs of different observations.  It is more robust than the standard deviation.', 'https://www.researchgate.net/publication/5182211_Gini\'s_Mean_Difference_A_Superior_Measure_of_Variability_for_Non-Normal_Distributions'),
       Info=list('Information index', 'a measure of the information content in a numeric variable relative to the information in a continuous numeric variable with no ties.  The lowest value of Info occurs in a very imbalanced binary variable.  Info comes from the approximate formula for the variance of a log odds ratio for a proportional odds model/Wilcoxon test, due to Whitehead (1993). Info is the ratio of the variance if there no ties in the data to the variance for the frequency distribution of observed values.', 'https://hbiostat.org/bib/r2.html')
       )

rHglossary <- function(x, html=TRUE, collapse=TRUE) {
  nams <- names(Hglossary)
  i <- which(tolower(nams) == tolower(x))
  if(! length(i))
    stop(paste(x, 'is not defined in Hglossary'))
  w     <- Hglossary[[i]]
  sname <- nams[i]
  lname <- w[[1]]
  def   <- w[[2]]
  href  <- w[[3]]
  if(html) {
    lname <- paste0('<a href="', href[1], '">', lname, '</a>')
    if(length(href) > 1)
      def <- paste0(def, ' <a href="', href[2], '">More information</a>')
  }
  if(collapse) paste0('<details><summary>', sname, '</summary>',
                             lname, ': ', def, '</details>')
  else paste0(sname, ': ', lname, ', ', def)
}
}


## Function to render HTML
## Converts argument to one character string with \n delimiters
## If knitr is currently running, runs this string through
## knitr::asis_output
## Otherwise, makes it browsable HTML using htmltools so that
## an RStudio Viewer or a new browser window will display the result
## See https://github.com/quarto-dev/quarto-cli/discussions/4248 which
## explains that you meed to enclose the text to keep from fooling
## Pandoc's reader

rendHTML <- function(x, html=TRUE) {
  x <- paste(x, collapse='\n')

  raw <- getOption('rawmarkup', FALSE)
  if(raw) {
    cat(x, '\n')
    return(invisible())
    }
   if(length(getOption('knitr.in.progress'))) {
     if(html)
       return(htmltools::knit_print.html(x))  # includes htmlPreserve
    ## if(html) x <- paste0('```{=html}\n\n', x, '\n```\n')
    return(knitr::asis_output(x))
  }
  if(! html) {  # Convert from RMarkdown to html
    tf <- tempfile(fileext='.Rmd')
    o  <- tempfile(fileext='.html')
    cat('---\ntitle: ""\npagetitle: x\noutput: html_document\n---\n',
        x, '\n', sep='', file=tf)
    rmarkdown::render(tf, output_file=o, quiet=TRUE)
    x <- readLines(o)
  }
   ## The following has prettier output for model fits than the kableExtra method
   print(htmltools::browsable(htmltools::HTML(x)))
}

## See ~/r/rmarkdown/html/render.qmd

## kableExtra print method did not render regression fit output
## as nicely as print(htmltools::browsable(...)) method above

## See kableExtra:::print.kableExtra
#   class(x) <- 'kableExtra'
#   dep <- list(rmarkdown::html_dependency_jquery(),
#               rmarkdown::html_dependency_bootstrap(theme = "cosmo"), 
#               kableExtra::html_dependency_kePrint(),
#               kableExtra::html_dependency_lightable())
#   ht <- htmltools::browsable(htmltools::HTML(as.character(x), 
#         "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({tex2jax: {inlineMath: [[\"$\",\"$\"]]}})</script><script async src=\"https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"))
#  htmltools::htmlDependencies(ht) <- dep
#  htmltools::html_print(ht)

sRequire <- function(package) {
  if(! requireNamespace(package, quietly=TRUE))
    stop(paste('package', package, 'is required but not installed'))
  invisible()
}
