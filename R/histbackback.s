"histbackback"<-
  function(x, y, brks = NULL, xlab = NULL, axes = TRUE, probability = FALSE, 
           xlim = NULL, ylab='',...)
{
  if(length(xlab))
    xlab <- rep(xlab, length = 2)
  
  if(is.list(x)) {
    namx <- names(x)  # FEH 5Jan99
    y <- x[[2]]   # was x$y  FEH
    if(!length(xlab)) {
      if(length(namx))
        xlab <- namx[1:2]
      else {   #FEH
        xlab <- deparse(substitute(x))
        xlab <- paste(xlab, c("x", "y"), sep = "$")
      }
    }

    x <- x[[1]]   # was x$x FEJ
  } else if(!length(xlab))
    xlab <- c(deparse(substitute(x)), deparse(substitute(y)))
  
  if(!length(brks))
    brks <- hist(c(x, y), plot = FALSE)$breaks

  if(.R.) {
    ll <- hist(x, breaks = brks, plot = FALSE)
    rr <- hist(y, breaks = brks, plot = FALSE)

    if(probability) {
      ll$counts <- ll$density
      rr$counts <- rr$density
    }
  } else {
    ll <- hist(x, breaks = brks, plot = FALSE, probability = probability)
    rr <- hist(y, breaks = brks, plot = FALSE, probability = probability)
  }

  if(length(xlim) == 2)
    xl <- xlim
  else {
    xl <- pretty(range(c( - ll$counts, rr$counts)))  ## 1Dec01
    xl <- c(xl[1],xl[length(xl)])
  }
      
  if(length(ll$counts) > 0) {
    if(.R.)
      barplot(-ll$counts, xlim=xl, space=0,
              horiz=TRUE, axes=FALSE, col=0, ...)
    else
      barplot( - ll$counts, brks, xlim = xl, histo = TRUE, horiz = TRUE, 
              axes = FALSE, ...)
    
    par(new = TRUE)
  }

  if(length(rr$counts) > 0) {
    if(.R.)
      barplot(rr$counts, xlim=xl, space=0,
              horiz=TRUE, axes=FALSE, col=0, ...)
    else
      barplot(rr$counts, brks, xlim = xl, histo = TRUE, horiz = TRUE, axes
              = FALSE, ...)
  }

  if(axes) {
    mgp.axis(1, at=pretty(xl), labels=format(abs(pretty(xl))))  ##FEH
    if(.R.) {
      del <- (brks[2]-brks[1] - (brks[3]-brks[2]))/2
      brks[1] <- brks[1] + del
      brks[-1] <- brks[-1] - del
      mgp.axis(2, at=0:(length(brks)-1),
               labels=formatC(brks, format='f', digits=.Options$digits))
    } else
      mgp.axis(2)
    
    title(xlab = xlab[1], adj = (-0.5 * xl[1])/( - xl[1] + xl[2]))
    title(xlab = xlab[2], adj = (-xl[1] + 0.5 * xl[2])/(-xl[1] + xl[2]))
    if(ylab!='')
      title(ylab=ylab)   # FEH
  }
  
  abline(v = 0)
  box()
  invisible(list(left = ll$counts, right = rr$counts, breaks = brks))
}
