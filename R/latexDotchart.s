latexDotchart <- 
  function(data, labels, groups = NULL, gdata = NA,
           xlab = "", auxdata, auxgdata=NULL, auxtitle,
           w=4, h=4, margin, lines = TRUE, dotsize = .075, size='small',
           size.labels = 'small', size.group.labels = 'normalsize',
           ttlabels = FALSE, sort.=TRUE, xaxis=TRUE, lcolor='gray',
           ...)
{
  txt <- function(x, y, s, size=NULL, just=c('c','l','r'), tt=FALSE) {
    just <- match.arg(just)
    s <- latexTranslate(s)
    n <- max(length(x), length(y), length(s))
    x <- rep(x, length.out=n)
    y <- rep(y, length.out=n)
    s <- rep(s, length.out=n)
    z <- character(n)
    if(tt) s <- paste('\\texttt{', s, '}', sep='')
    if(length(size)) s <- paste('\\', size, ' ', s, sep='')
    for(i in 1:n)
      z[i] <- sprintf('\\put(%g,%g){\\makebox(.001,.001)[%s]{%s}}',
                      x[i], y[i], just, s[i])
    z
  }
  ln <- function(x1, y1, x2, y2, color='black') {
    n <- max(length(x1), length(x2), length(y1), length(y2))
    x1 <- rep(x1, length.out=n)
    y1 <- rep(y1, length.out=n)
    x2 <- rep(x2, length.out=n)
    y2 <- rep(y2, length.out=n)
    z <- character(n)
    for(i in 1:n)
      z[i] <- if(x1[i] == x2[i])
        sprintf('\\put(%g,%g){\\line(0,%g){%g}}', x1[i], y1[i],
                1*(y2[i] >= y1[i]) - 1*(y2[i] < y1[i]), abs(y2[i]-y1[i]))
    else if(y1[i] == y2[i])
      sprintf('\\put(%g,%g){\\line(%g,0){%g}}', x1[i], y1[i],
              1*(x2[i] >= x1[i]) - 1*(x2[i] < x1[i]), abs(x2[i]-x1[i]))
    else
        sprintf('\\drawline(%g,%g)(%g,%g)',
                      x1[i], y1[i], x2[i], y2[i])
    if(color != 'black')
      z <- c(if(color == 'gray') '\\color[gray]{0.8}' else
             sprintf('\\color{%s}', color),
             z, '\\color{black}')
    z
  }
  ## Approximate length in inches of longest char. string
  acl <- function(s) 0.09 * max(nchar(s))
  f <- sprintf

  if(size.labels == size) size.labels <- NULL
  if(size.group.labels == size) size.group.labels <- NULL

  z <- c('\\setlength{\\unitlength}{1in}',
         f('\\begin{picture}(%g,%g)', w, h),
         f('\\%s', size))
  
  ndata <- length(data)
  if(missing(labels)) {
    if(length(names(data)))
      labels <- names(data)
    else labels <- paste("#", seq(along = ndata))
  }
  else labels <- rep(as.character(labels), length = ndata)

  if(missing(groups)) {
    glabels <- NULL
    gdata <- NULL
    if(sort.) {
      ord <- order(-data)
      data <- data[ord]
      labels  <- labels[ord]
      if(! missing(auxdata)) auxdata <- auxdata[ord]
    }
  } else {
    if(! sort.) {
      ##assume data sorted in groups, but re-number groups
      ##to be as if groups given in order 1,2,3,...
      ug <- unique(as.character(groups))
      groups <- factor(as.character(groups), levels=ug)
    }
    
    groups  <- unclass(groups)
    glabels <- levels(groups)
    gdata   <- rep(gdata, length = length(glabels))	
    ord     <- if(sort.) order(groups, -data) else
                         order(groups, seq(along = groups))
    groups  <- groups[ord]
    data    <- data[ord]
    labels  <- labels[ord]
    if(! missing(auxdata)) auxdata <- auxdata[ord]
  }

  alldat <- c(data, gdata)
  if(! missing(auxdata)) auxdata <- format(c(auxdata, auxgdata))
  
  alllab <- c(labels, glabels)
  ## set up margins and user coordinates, draw box

  xl <- range(p <- pretty(alldat))
  yl <- c(1, length(alldat))

  if(missing(margin))
    margin <- c(acl(alllab),
                ifelse(xlab == '', .2, .4),
                ifelse(missing(auxdata), 0, acl(auxdata)),
                ifelse(missing(auxtitle), 0, .1))
                                  

  xt <- function(x) round((w - sum(margin[c(1,3)]))*(x - xl[1])/diff(xl) +
                          margin[1], 5)
  yt <- function(y) round((h - sum(margin[c(2,4)]))*(y - yl[1])/diff(yl) +
                          margin[2], 5)

  ## \color screws up line and circle placement if first multiputlist
  ## and put occur after \color
  if(xaxis) {
    z <- c(z, paste(f('\\multiputlist(%g,%g)(%g,%g){',
                      xt(xl[1]), yt(yl[1]) - .15, diff(xt(p[1:2])), 0),
                    paste(p, collapse=','), '}', sep=''))
    z <- c(z, ln(xt(p), yt(yl[1]) - 0.05, xt(p), yt(yl[1])))
    if(xlab != '')
      z <- c(z, txt(xt(xl[1] + diff(xl)/2), .1, xlab))
  }

  z <- c(z, ln(xt(xl), yt(yl[1]), xt(xl), yt(yl[2])),
            ln(xt(xl[1]), yt(yl), xt(xl[2]), yt(yl)))
 
  den <- ndata + 2 * length(glabels) + 1

  delt <- ( - (yl[2] - yl[1]))/den
  ypos <- seq(yl[2], by = delt, length = ndata)

  if(! missing(groups)) {
    ypos1 <- ypos + 2 * delt * (if(length(groups)>1)
                                  cumsum(c(1, diff(groups) > 0))
                                else 1)
    diff2 <- c(3 * delt, diff(ypos1))
    ypos2 <- ypos1[abs(diff2 - 3 * delt) < abs(0.001 * delt)] - 
      delt
    ypos <- c(ypos1, ypos2) - delt
  }

  ##put on labels and data
  ypos <- ypos + delt
  nongrp <- 1:ndata

  if(lines)
    z <- c(z, ln(xt(xl[1]), yt(ypos[nongrp]), xt(xl[2]), yt(ypos[nongrp]),
           color=lcolor))
  
  for(i in seq(along = alldat))
    if(! is.na(alldat[i] + ypos[i]))
      z <- c(z, f('\\put(%g,%g){\\circle*{%g}}',
                  xt(alldat[i]), yt(ypos[i]), dotsize))
                
  if(! missing(auxdata)) {
    z <- c(z, txt(w - 0.02, yt(ypos[nongrp]), auxdata,
                  size=size.labels, just='r'))
    if(! missing(auxtitle))
      z <- c(z, txt(w - 0.02, yt(yl[2]) + 0.1, auxtitle,
                    size=size.labels, just='r'))
  }
  labng <- alllab[nongrp]
  yposng <- ypos[nongrp]
  
  z <- c(z, txt(margin[1] - 0.05, yt(yposng), labng,
                size=size.labels, just='r', tt=ttlabels))
  if(! missing(groups))
    z <- c(z, txt(margin[1] - 0.05, yt(ypos[-nongrp]), alllab[-nongrp],
                  size=size.group.labels, just='r'))
  
  z <- c(z, '\\end{picture}')
  z
}
