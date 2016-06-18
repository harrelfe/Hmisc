ggfreqScatter <- function(x, y, bins=50, g=10,
                         xlab=as.character(substitute(x)),
                         ylab=as.character(substitute(y))) {
    xlab <- xlab; ylab <- ylab
    i <-  ! (is.na(x) | is.na(y))
    x <- x[i]; y <- y[i]
    if(is.numeric(x)) {
      rx <- range(x)
      sx <- diff(rx) / bins
      x  <- rx[1] + sx * round((x - rx[1]) / sx)
    }
    if(is.numeric(y)) {
      ry <- range(y)
      sy <- diff(ry) / bins
      y  <- ry[1] + sy * round((y - ry[1]) / sy)
      }
    
    k <- subset(as.data.frame(table(x, y)), Freq > 0)
    if(is.numeric(x)) k$x <- as.numeric(as.character(k$x))
    if(is.numeric(y)) k$y <- as.numeric(as.character(k$y))
    k$fg <- cut2(k$Freq, g=g)
    ggplot(k, aes(x=x, y=y, alpha=fg)) + geom_point() +
      guides(alpha=guide_legend(title='Frequency')) +
      xlab(xlab) + ylab(ylab)
}



