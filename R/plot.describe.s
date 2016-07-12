plot.describe <- function(x, which=c('continuous', 'binary', 'categorical'),
                          what=NULL, sort=c('ascending', 'descending', 'none'),
                          n.unique=10, digits=5, ...) {
  if(grType() != 'plotly')
    stop('must have plotly package installed and specify options(grType="plotly")')
 	
  which <- match.arg(which)
  if(length(what)) x <- x[what]

  sort <- match.arg(sort)

  format_counts <- function(s) {
    bl <- '                                     '
    na  <- substring(paste(names(s), bl, sep=''), 1,
                     max(nchar(names(s))))
    va  <- paste(substring(bl, 1,
                           max(nchar(s)) - nchar(s) + 1), s, sep='')
    zz  <- paste(na, va, sep=':')
    gsub(' ', '&nbsp;', zz)
  }

  fmtlab <- function(x) {
    lab <- sub('^.*:', '', x$descript)
    if(length(x$units))
      lab <- paste(lab, ' &emsp;<span style="font-size:0.8em">',
                   x$units, '</span>', sep='')
    lab
  }

  ge <- function(which) unlist(lapply(w, function(x) x[[which]]))

  
  if(which == 'binary') {
    f <- function(x) {
      s <- x$counts
      if('Sum' %nin% names(s))
        return(data.frame(prop=numeric(0), text=character(0)))
      n    <- as.numeric(s['n'])
      freq <- as.numeric(s['Sum'])
      text <- paste(c(fmtlab(x),
                      paste(round(freq / n, 3),
                            '&emsp;<span style="font-size:0.7em">',
                            freq, '/', n, '</span>', sep=''),
                      format_counts(s)), collapse='<br>')
      data.frame(prop=freq / n, text=I(text), missing=as.numeric(s['missing']))
    }

    w <- lapply(x, f)
    nam <- names(w)
    for(n in nam) {
      l <- length(w[[n]]$prop)
      w[[n]]$xname <- if(l) rep(n, l) else character(0)
    }

    if(length(ge('xname')) == 0) {
      warning('no binary variables found')
      return(invisible())
    }
 
    z <- data.frame(xname = I(ge('xname')), Proportion= ge('prop'),
                    text  = I(ge('text')),  missing   = ge('missing'))

    if(sort != 'none') {
      i <- order(ifelse(sort == 'ascending', 1, -1) * z$Proportion)
      z <- z[i, ]
    }
    
    p <- if(any(z$missing > 0))
           plotly::plot_ly(z, x=Proportion, y=xname, text=text,
                           color=missing, mode='markers',
                           type='scatter', hoverinfo='text')
         else
           plotly::plot_ly(z, x=Proportion, y=xname, text=text,
                           mode='markers', type='scatter', hoverinfo='text')

    tl <- seq(0, 1, by=0.05)
    tt <- ifelse(tl %% 0.1 == 0, as.character(tl), '')
    
    return(plotly::layout(xaxis=list(range=c(0,1), zeroline=FALSE,
                                     tickvals=tl, ticktext=tt),
                          yaxis=list(title=''),
                          autosize=FALSE, height=150 + 15 * nrow(z)))
           
  }

  if(which == 'categorical') {
    stop('type="categorical" not yet implemented')
  }

  ## which == 'continuous'
  f <- function(x) {
    s <- x$counts
    v <- x$values
    if(! (as.numeric(s['unique']) >= n.unique &&
          length(v) && is.list(v) &&
          all(names(v) == c('value', 'frequency')) &&
          is.numeric(v$value)) )
      return(data.frame(X=numeric(0), count=numeric(0), text=character(0)))
    X <- v$value
    Y <- v$frequency

    text <- paste(format(X, digits=digits), ' (n=', Y, ')', sep='')
    X    <- (X - X[1]) / diff(range(X))
    zz   <- format_counts(s)
    lab  <- fmtlab(x)

    text[1] <- paste(c(lab, text[1], zz), collapse='<br>')
    ## Note: plotly does not allow html tables as hover text
    m <- rep(as.numeric(s['missing']), length(X))
    list(X=X, prop=Y / sum(Y), text=I(text), missing=m)
  }
  
  w <- lapply(x, f)
  nam <- names(w)
  for(n in nam) {
    l <- length(w[[n]]$X)
    w[[n]]$xname <- if(l) rep(n, l) else character(0)
  }

  if(length(ge('xname')) == 0) {
    warning('no continuous variables found')
    return(invisible())
    }
    
  z <- data.frame(xname      = ge('xname'), X=ge('X'),
                  Proportion = round(ge('prop'), 4),
                  text       = ge('text'),
                  missing    = ge('missing'))
  
  ## Note: plotly does not allow font, color, size changes for hover text
  p <- if(any(z$missing > 0))
         plotly::plot_ly(z, x=X, y=xname, size=Proportion, text=text,
                         color=missing, mode='markers',
                         marker=list(symbol='line-ns-open'),
                         type='scatter', hoverinfo='text') else
         plotly::plot_ly(z, x=X, y=xname, size=Proportion, text=text,
                         mode='markers',
                         marker=list(symbol='line-ns-open'),
                         type='scatter', hoverinfo='text')
  plotly::layout(xaxis=list(title='', showticklabels=FALSE, zeroline=FALSE,
                            showgrid=FALSE),
                 yaxis=list(title=''), autosize=TRUE)
}

utils::globalVariables(c('X', 'Proportion', 'xname'))
