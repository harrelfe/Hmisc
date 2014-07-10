subplot <- function (fun, x, y = NULL, size = c(1, 1), vadj = 0.5,
                     hadj = 0.5, pars = NULL) {
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  if (missing(x))
    x <- locator(2)

  xy <- xy.coords(x, y)
  if (length(xy$x) != 2) {
    pin <- par("pin")
    tmp <- cnvrt.coords(xy$x[1], xy$y[1], "usr")$plt
    x <- c(tmp$x - hadj * size[1]/pin[1], tmp$x + (1 - hadj) *
           size[1]/pin[1])
    y <- c(tmp$y - vadj * size[2]/pin[2], tmp$y + (1 - vadj) *
           size[2]/pin[2])
    xy <- cnvrt.coords(x, y, "plt")$fig
  }
  else {
    xy <- cnvrt.coords(xy, , "usr")$fig
  }

  if(length(pars)) par(pars)
  par(plt = c(xy$x, xy$y), new = TRUE)
  if(is.function(fun))fun() else fun
  tmp.par <- par(no.readonly = TRUE)

  return(invisible(tmp.par))
}
