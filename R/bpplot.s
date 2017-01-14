##Modified FEH 30Jun97 - delete missing data, names default to T,
## auto names for list argument, ylab default to "" instead of Percentiles
## names -> name, added srtx
bpplot <- function(..., name = TRUE,
                   main = "Box-Percentile Plot", 
                   xlab = "", ylab = "", srtx=0, plotopts=NULL)
{
  all.x <- list(...)  ## FH 30Jun97
  nam <- character(0)   ## FH
  ## if(is.list(...)) {  ## FH
  if(is.list(all.x[[1]])) {
    all.x <- all.x[[1]]
    if(is.logical(name) && name) name <- names(...)   ## FH
  }
  
  n <- length(all.x)
  centers <- seq(from = 0, by = 1.2, length = n)
  ymax <- max(sapply(all.x, max, na.rm=TRUE))  ## na.rm=T FEH
  ymin <- min(sapply(all.x, min, na.rm=TRUE))
  xmax <- max(centers) + 0.5
  xmin <- -0.5
  pargs <- c(list(c(xmin, xmax), c(ymin, ymax), type = "n", main = main,
                  xlab = '', ylab = ylab, xaxt = "n"), plotopts)
  do.call("plot", pargs)
  for(i in 1 : n) {
    plot.values <- bpx(all.x[[i]], centers[i])
    lines(plot.values$x1,    plot.values$y1)
    lines(plot.values$x2,    plot.values$y2)
    lines(plot.values$q1.x,  plot.values$q1.y)
    lines(plot.values$q3.x,  plot.values$q3.y)
    lines(plot.values$med.x, plot.values$med.y)
  }

  if(is.logical(name)) {
    if(name)
      mgp.axis(1, centers, 
               sapply(substitute(list(...)), deparse)[2:(n + 1)],
               srt=srtx,
               adj=if(srtx==0).5
                   else 1,
               axistitle=xlab)
  }
  else mgp.axis(1, centers, name, srt=srtx,
                adj=if(srtx==0).5
                    else 1,
                axistitle=xlab)
  
  invisible(centers)
}

bpx <- function(y, offset)
{
  y <- y[!is.na(y)]   ## FEH 30Jun97
  n <- length(y)
  delta <- 1/(n + 1)
  prob <- seq(delta, 1 - delta, delta)
  quan <- sort(y)
  med <- median(y)
  q1 <- median(y[y < med])
  q3 <- median(y[y > med])
  first.half.p <- prob[quan <= med]
  second.half.p <- 1 - prob[quan > med]
  plotx <- c(first.half.p, second.half.p)
  
  ## calculating the ends of the first quartile line

  qx <- approx(quan, plotx, xout = q1)$y
  q1.x <- c( - qx, qx) + offset

  ## calculating the ends of the third quartile line

  qx <- approx(quan, plotx, xout = q3)$y
  q3.x <- c( - qx, qx) + offset
  q1.y <- c(q1, q1)
  q3.y <- c(q3, q3)
  med.x <- c( - max(first.half.p), max(first.half.p)) + offset
  med.y <- c(med, med)
  return(list(x1 = ( - plotx) + offset, y1 = quan, x2 = plotx + offset,
              y2 = quan, q1.y = q1.y, q1.x = q1.x, q3.y = q3.y, q3.x = q3.x,
              med.y = med.y, med.x = med.x))
}
