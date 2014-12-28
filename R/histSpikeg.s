histSpikeg <- function(formula=NULL, predictions=NULL, data,
                       xlim=NULL, ylim=NULL,
                       side=1, nint=100, frac=.02) {
  ## Raw data in data, predicted curves are in predictions
  ## If predictions is not given, side (1 or 3) is used
  v  <- all.vars(formula)
  yv <- v[ 1]
  xv <- v[-1]
  X  <- xv[1]

  if(length(xv) > 1 && ! length(predictions))
    stop('predictions must be given if formula has two variables on the right')
  x1 <- data[[X]]
  if(length(xlim)) {
    data <- data[x1 >= xlim[1] & x1 <= xlim[2], ]
    x1 <- data[[X]]
  }
  if(length(unique(x1)) > nint) {
    eps <- diff(range(x1, na.rm=TRUE)) / nint
    x1  <- round(x1 / eps) * eps
    data[[X]] <- x1
  }
  p  <- predictions
  if(length(p)) xr <- range(p[[X]], na.rm=TRUE)
  else frac <- frac * 2

  if(length(xv) == 1) {
    tab    <- as.data.frame(do.call(table, data[X]))
    tab[[X]] <- as.numeric(as.character(tab[[X]]))
    tab    <- tab[tab[[X]] >= xr[1] & tab[[X]] <= xr[2], ]
    tab$.rf. <- tab$Freq / max(tab$Freq)
    if(length(p)) tab$.yy. <- approx(p[[X]], p[[yv]], xout=tab[[X]])$y
  } else {
    tab <- as.data.frame(do.call(table, data[xv]))
    tab <- subset(tab, Freq > 0)
    tab[[X]] <- as.numeric(as.character(tab[[X]]))
    tab    <- tab[tab[[X]] >= xr[1] & tab[[X]] <= xr[2], ]
    tab$.rf. <- tab$Freq / max(tab$Freq)
    tab$.yy. <- rep(NA, nrow(tab))
    gv <- do.call(paste, tab[xv[-1]])
    gvd <- do.call(paste, p[xv[-1]])
    for(s in unique(gv)) {
      i <- gv  == s
      j <- gvd == s
      tab$.yy.[i] <- approx(p[[X]][j], p[[yv]][j], xout=tab[[X]][i])$y
    }
  }
  tab$.rf. <- tab$.rf. * diff(ylim) * frac / max(tab$.rf.)
  n <- nrow(tab)
  tab$.ylo. <- if(length(p)) tab$.yy. - tab$.rf.
  else if(side == 1) rep(ylim[1], n) else rep(ylim[2], n)

  tab$.yhi. <- if(length(p)) tab$.yy. + tab$.rf.
  else if(side == 1) ylim[1] + tab$.rf. else ylim[2] - tab$.rf.

  eval(parse(text=sprintf(
  'ggplot2::geom_segment(data=tab,
                        aes(x=%s, xend=%s,
                            y=.ylo., yend=.yhi.))', X, X)))
}

utils::globalVariables(c('aes', 'Freq', '.ylo.', '.yhi.'))
