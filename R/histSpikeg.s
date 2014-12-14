histSpikeg <- function(formula=NULL, predictions=NULL, data,
                       xlim=NULL, ylim=NULL,
                       side=1, nint=100, frac=.02) {
  ## Raw data in data, predicted curves are in predictions
  ## If predictions is not given, side (1 or 3) is used
  v  <- all.vars(formula)
  yv <- v[ 1]
  xv <- v[-1]
  if(length(xv) > 2) stop('may not specify more than two predictors')
  if(length(xv) == 2 && ! length(predictions))
    stop('predictions must be given if formula has two variables on the right')
  x1 <- data[[xv[1]]]
  if(length(xlim)) {
    data <- data[x1 >= xlim[1] & x1 <= xlim[2], ]
    x1 <- data[[xv[1]]]
  }
  if(length(unique(x1)) > nint) {
    eps <- diff(range(x1, na.rm=TRUE)) / nint
    x1  <- round(x1 / eps) * eps
  }
  x2 <- if(length(xv) == 2) data[[xv[2]]]
  p  <- predictions
  if(length(p)) xr <- range(p[[xv[1]]], na.rm=TRUE)
  else frac <- frac * 2
  
  if(length(xv) == 1) {
    tab    <- as.data.frame(table(x1))
    tab$x1 <- as.numeric(as.character(tab$x1))
    tab    <- tab[tab$x1 >= xr[1] & tab$x1 <= xr[2], ]
    tab$rf <- tab$Freq / sum(tab$Freq)
    if(length(p)) tab$yy <- approx(p[[xv]], p[[yv]], xout=tab$x1)$y
  } else {
    tab <- as.data.frame(table(x1, x2))
    tab <- subset(tab, Freq > 0)
    tab$x1 <- as.numeric(as.character(tab$x1))
    tab    <- tab[tab$x1 >= xr[1] & tab$x1 <= xr[2], ]
    tab$rf <- tab$Freq / sum(tab$Freq)
    tab$yy <- rep(NA, nrow(tab))
    for(s in levels(tab$x2)) {
      i <- tab$x2 == s
      j <- p[[xv[2]]] == s
      tab$yy[i] <- approx(p[[xv[1]]][j], p[[yv]][j], xout=tab$x1[i])$y
    }
  }
  tab$rf <- tab$rf * diff(ylim) * frac / max(tab$rf)
  n <- nrow(tab)
  tab$ylo <- if(length(p)) tab$yy - tab$rf
  else if(side == 1) rep(ylim[1], n) else rep(ylim[2], n)

  tab$yhi <- if(length(p)) tab$yy + tab$rf
  else if(side == 1) ylim[1] + tab$rf else ylim[2] - tab$rf
  if(length(xv) == 2) names(tab)[names(tab) == 'x2'] <- xv[2]
  
  ggplot2::geom_segment(data=tab,
                        aes(x=x1, xend=x1,
                            y=ylo, yend=yhi))
}

utils::globalVariables(c('Freq', 'ylo', 'yhi'))
