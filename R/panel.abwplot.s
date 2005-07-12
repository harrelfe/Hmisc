if(FALSE) {
  panel.abwplot <- function(x, y, box.ratio = 1, means=TRUE,
                            font = box.dot$font, pch = box.dot$pch, 
                            cex = box.dot$cex, 
                            col = box.dot$col, ...)
  {
    ok <- !is.na(x) & !is.na(y)
    x <- x[ok]
    y <- y[ok]
    y.unique <- sort(unique(y))
    width <- box.ratio/(1 + box.ratio)
    w <- width/2
    lineopts <- trellis.par.get("box.rectangle")
    for(Y in y.unique) {
      X <- x[y == Y]
      q <- quantile(X, c(.01,.05,.1,.25,.75,.9,.95,.99,.5))
      median.value <- list(x = q[9], y = Y)
      z <- c(1, .01,
             2, .01,
             2, .05,
             3, .05,
             3, .10,
             4, .10,
             4, .25,
             5, .25,
             5, .10,
             6, .10,
             6, .05,
             7, .05,
             7, .01,
             8, .01,
             8,-.01,
             7,-.01,
             7,-.05,
             6,-.05,
             6,-.10,
             5,-.10,
             5,-.25,
             4,-.25,
             4,-.10,
             3,-.10,
             3,-.05,
             2,-.05,
             2,-.01,
             1,-.01,
             1, .01)
      box.dot <- trellis.par.get("box.dot")
      box.dot.par <- c(list(pch = pch, cex = cex, col = col, font = font), ...)
      do.call('lines',c(list(x=q[z[seq(1,length(z),by=2)]],
                             y=Y + 4*w*z[seq(2,length(z),by=2)]),lineopts))
      ##do.call('segments',c(list(x1=q[c(2:7)],y1=Y+rep(-w,6),
      ##                     x2=q[c(2:7)],y2=Y+rep(w,6)),
      ##                     lineopts))
      
      do.call("points", c(median.value, box.dot.par))
      if(means)
        do.call('lines',c(list(x=rep(mean(X),2),y=Y+c(-w,w)),
                          lineopts, lty=2))
    }
  }
  
  NULL
}
