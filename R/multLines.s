multLines <- function(x, y, pos=c('left', 'right'),
                      col='gray', lwd=1, lty=1,
                      lwd.vert=.85, lty.vert=1, alpha=0.4,
                      grid=FALSE) {
  pos <- match.arg(pos)
  p <- ncol(y)
  n <- nrow(y)
  if(! is.matrix(y) || p == 1 || p %% 2 != 1)
    stop('y must have 3, 5, 7, ... columns')
  if(length(x) != n)
    stop('length of x must match rows of y')

  vcol  <- adjustcolor(col, alpha.f=alpha)

  if(grid) {
    llines(x, y[, 1], col=col, lwd=lwd, lty=lty)
    xdel <- unit(0.75, 'mm')
    x    <- unit(x, 'native')
    gp   <- gpar(col=vcol, lwd=lwd.vert, lty=lty.vert)
  }
  else {
    lines(x, y[, 1], col=col, lwd=lwd, lty=lty)
    xdel <- 0.005 * diff(par('usr')[1 : 2])
  }


  half <- (p - 1) / 2
  x0   <- if(grid) unit(x, 'native') else x
  for(i in 1 : half) {
    i1 <- i + 1
    i2 <- p - i + 1
    x0 <- switch(pos, left =  x0 - xdel, right = x0 + xdel)
    if(grid) grid.segments(x0, y[, i1], x0, y[, i2],
                           gp=gp, default.units='native')
    else segments(x0, y[, i1], x0, y[, i2], col=vcol,
                  lty=lty.vert, lwd=lwd.vert)
  }
}

