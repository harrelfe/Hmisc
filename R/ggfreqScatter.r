ggfreqScatter <- function(x, y, bins=50, g=10,
                          xtrans  = function(x) x,
                          ytrans  = function(y) y,
                          xbreaks = pretty(x, 10),
                          ybreaks = pretty(y, 10),
                          xminor  = NULL,
                          yminor  = NULL,
                          xlab=as.character(substitute(x)),
                          ylab=as.character(substitute(y)),
                          fcolors=rainbow(10), ...) {

  xlab <- if(! missing(xlab)) xlab else if(label(x) != '') label(x, plot=TRUE) else xlab
  ylab <- if(! missing(ylab)) ylab else if(label(y) != '') label(y, plot=TRUE) else ylab

  xbreaks <- xbreaks; ybreaks <- ybreaks
  bins <- rep(bins, length=2)
  
  i <-  ! (is.na(x) | is.na(y))
  x <- xtrans(x[i]); y <- ytrans(y[i])
  
  if(is.numeric(x)) {
    rx <- range(x)
    sx <- diff(rx) / bins[1]
    x  <- rx[1] + sx * round((x - rx[1]) / sx)
  }
  if(is.numeric(y)) {
    ry <- range(y)
    sy <- diff(ry) / bins[2]
    y  <- ry[1] + sy * round((y - ry[1]) / sy)
  }
  
  k <- subset(as.data.frame(table(x, y)), Freq > 0)
  if(is.numeric(x)) k$x <- as.numeric(as.character(k$x))
  if(is.numeric(y)) k$y <- as.numeric(as.character(k$y))
  k$fg <- cut2(k$Freq, g=g)
  br   <- 1 : length(levels(k$fg))
  ggplot(k, aes(x=x, y=y, alpha=fg, color=as.integer(fg))) + geom_point(...) +
    scale_x_continuous(breaks=xtrans(xbreaks), labels=xbreaks,
                       minor_breaks=if(length(xminor)) xtrans(xminor)) +
    scale_y_continuous(breaks=ytrans(ybreaks), labels=ybreaks,
                       minor_breaks=if(length(yminor)) ytrans(yminor)) +
    scale_color_gradientn(colors=fcolors, breaks=1 : length(levels(k$fg)),
                          labels=levels(k$fg)) +
    guides(alpha = FALSE, 
           color = guide_legend(title='Frequency')) +
    xlab(xlab) + ylab(ylab)
}

utils::globalVariables('fg')

