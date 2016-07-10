plot.describe <- function(x, which=c('continuous', 'categorical'),
                          n.unique=8, digits=5, ...) {
  if(grType() != 'plotly')
    stop('must have plotly package installed and specify options(grType="plotly")')
 	
  which <- match.arg(which)
	
  f <- function(x) {
      s <- x$counts
      if((as.numeric(s['unique']) < n.unique) ||
          'Sum' %in% names(s) || 'Info' %nin% names(s))
          return(data.frame(X=numeric(0), count=numeric(0), text=character(0)))
  i <- x$intervalFreq
  if(length(i)) {
      X <- seq(i$range[1], i$range[2], length=length(i$count))
      Y <- i$count
      j <- Y > 0
      X <- X[j]
      Y <- Y[j]
  } else {
      X <- as.numeric(colnames(x$values))
      Y <- x$values['Frequency', ]
  }
    
  text <- format(X, digits=digits)
  X   <- (X - X[1]) / diff(range(X))
  na  <- substring(paste(names(s), '                          ', sep=''), 1, max(nchar(names(s))))
  va  <- paste(substring('                      ', 1, max(nchar(s)) - nchar(s) + 1), s, sep='')
  zz  <- paste(na, va, sep=':')
  zz  <- gsub(' ', '&nbsp;', zz)
  lab <- sub('^.*:', '', x$descript)
  text[1] <- paste(c(lab, text[1], zz), collapse='<br>')
  # Note: plotly does not allow html tables as hover text

  list(X=X, count=Y, text=text)
  }
  
  w <- lapply(x, f)
  nam <- names(w)
  for(n in nam) {
      l <- length(w[[n]]$X)
      w[[n]]$xname <- if(l) rep(n, l) else character(0)
  }
  
  g <- function(which) unlist(lapply(w, function(x) x[[which]]))
  z <- data.frame(xname=g('xname'), X=g('X'), count=g('count'),
                  text=g('text'))
  
  # Note: plotly does not allow font, color, size changes for hover text
  plot_ly(z, x=X, y=xname, size=count, text=text, mode='markers',
          marker=list(symbol='line-ns-open'),
          type='scatter', hoverinfo='text') %>%
      layout(xaxis=list(title='', showticklabels=FALSE, zeroline=FALSE, showgrid=FALSE),
             yaxis=list(title=''))
}
