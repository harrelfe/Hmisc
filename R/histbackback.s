histbackback <-
  function(x, y, brks = NULL, xlab = NULL, axes = TRUE, probability = FALSE, 
           xlim = NULL, ylab='', ...)
{
  if(length(xlab))
    xlab <- rep(xlab, length = 2)
  
  if(is.list(x))
    {
      namx <- names(x)
      y <- x[[2]]
      if(!length(xlab))
        {
          if(length(namx))
            xlab <- namx[1:2]
          else
            {
              xlab <- deparse(substitute(x))
              xlab <- paste(xlab, c("x", "y"), sep = "$")
            }
        }
      
      x <- x[[1]]
    }
  else if(!length(xlab))
    xlab <- c(deparse(substitute(x)), deparse(substitute(y)))
  
  if(!length(brks))
    brks <- hist(c(x, y), plot = FALSE)$breaks

  ll <- hist(x, breaks = brks, plot = FALSE)
  rr <- hist(y, breaks = brks, plot = FALSE)
  
  if(probability)
    {
      ll$counts <- ll$density
      rr$counts <- rr$density
    }
    
  if(length(xlim) == 2)
    xl <- xlim
  else
    {
      xl <- pretty(range(c( - ll$counts, rr$counts)))
      xl <- c(xl[1], xl[length(xl)])
    }
      
  if(length(ll$counts) > 0)
    {
      barplot(-ll$counts, xlim=xl, space=0,
              horiz=TRUE, axes=FALSE, col=0, ...)
      par(new = TRUE)
    }

  if(length(rr$counts) > 0)
        barplot(rr$counts, xlim=xl, space=0,
                horiz=TRUE, axes=FALSE, col=0, ...)
  
  if(axes)
    {
      mgp.axis(1, at=pretty(xl), labels=format(abs(pretty(xl))))
      del <- (brks[2]-brks[1] - (brks[3]-brks[2]))/2
      brks[1] <- brks[1] + del
      brks[-1] <- brks[-1] - del
      at <- 0 : (length(brks) - 1)
      pb <- pretty(brks)
      atpb <- approxExtrap(brks, at, xout=pb)$y
      mgp.axis(2, at=atpb, labels=format(pb))
    
      title(xlab = xlab[1], adj = (-0.5 * xl[1])/( - xl[1] + xl[2]))
      title(xlab = xlab[2], adj = (-xl[1] + 0.5 * xl[2])/(-xl[1] + xl[2]))
      if(ylab!='') title(ylab=ylab)
    }
  
  abline(v = 0)
  box()
  invisible(list(left = ll$counts, right = rr$counts, breaks = brks))
}
