## Function to use the mouse to zoom in on plots.
## Author: Bill Dunlap <bill@STAT.WASHINGTON.EDU>
zoom <- function(fun, ...) {
  on.exit(par(oldpar))
  oldpar <- par('err')
  par(err = -1)
  fun(...)
  while(TRUE) {
    cat("Click mouse over corners of zoom area: ")
    p <- locator(n = 2)
    if(length(p$x) != 2) break

    xlim <- range(p$x)
    ylim <- range(p$y)
    cat("xlim=", xlim, "ylim=", ylim, "\n")
    fun(..., xlim=xlim, ylim=ylim)
  }

  cat("Bye! \n")
}
