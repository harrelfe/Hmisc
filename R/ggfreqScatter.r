ggfreqScatter <- function(x, y, bins=50, g=10, cuts=NULL,
                          xtrans  = function(x) x,
                          ytrans  = function(y) y,
                          xbreaks = pretty(x, 10),
                          ybreaks = pretty(y, 10),
                          xminor  = NULL,
                          yminor  = NULL,
                          xlab=as.character(substitute(x)),
                          ylab=as.character(substitute(y)),
                          fcolors=viridis::viridis(10),
                          nsize=FALSE, html=FALSE, prfreq=FALSE, ...) {

  xlab <- if(! missing(xlab)) xlab
          else if(label(x) != '') label(x, plot=TRUE, html=html) else xlab
  ylab <- if(! missing(ylab)) ylab
          else if(label(y) != '') label(y, plot=TRUE, html=html) else ylab

  nx <- is.numeric(x); ny <- is.numeric(y)
  xbreaks <- if(nx) xbreaks; ybreaks <- if(ny) ybreaks
  bins <- rep(bins, length=2)
  
  i <-  ! (is.na(x) | is.na(y))
  x <- xtrans(x[i]); y <- ytrans(y[i])
  
  if(nx) {
    rx <- range(x)
    sx <- diff(rx) / bins[1]
    x  <- rx[1] + sx * round((x - rx[1]) / sx)
  }
  if(ny) {
    ry <- range(y)
    sy <- diff(ry) / bins[2]
    y  <- ry[1] + sy * round((y - ry[1]) / sy)
  }
  
  k <- subset(as.data.frame(table(x, y)), Freq > 0)
  if(nx) k$x <- as.numeric(as.character(k$x))
  if(ny) k$y <- as.numeric(as.character(k$y))
  if(prfreq) print(table(k$Freq))

  if(g == 0) {
    w <-  if(nsize)
            ggplot(k, aes(x=x, y=y, size=Freq ^ 0.25, label=Freq)) +
              geom_point(...) +
              scale_size_continuous() +
              xlab(xlab) + ylab(ylab) +
              guides(size = guide_legend(title='Frequency'))
       else
         ggplot(k, aes(x=x, y=y, alpha=Freq ^ 0.25, label=Freq,
                       color=Freq ^ 0.25)) +
                   geom_point(...) +
                   scale_color_gradientn(colors=fcolors) +
                   guides(alpha = FALSE, 
                          color = guide_legend(title='Frequency')) +
           xlab(xlab) + ylab(ylab)
    return(w)
  }
  
  k$fg <- if(length(cuts)) cut2(k$Freq, cuts=cuts) else cut2(k$Freq, g=g)

  ufreq <- sort(unique(k$Freq))
  few <- length(ufreq) <= 15
  brn <- if(few) ufreq else unique(quantile(k$Freq, seq(0, g) / g))
  w <- if(nsize)
         ggplot(k, aes(x=x, y=y, size=Freq ^ 0.25, label=Freq)) +
           geom_point(...) +
           scale_size_continuous(breaks=brn ^ 0.25, labels=round(brn)) +
           xlab(xlab) + ylab(ylab) +
           guides(size = guide_legend(title='Frequency'))
       else
         ggplot(k, aes(x=x, y=y, alpha=fg, label=Freq,
                       color=if(few) k$Freq else as.integer(fg))) +
           geom_point(...) +
           scale_color_gradientn(colors=fcolors,
                                 breaks=if(few) ufreq else 1 : length(levels(k$fg)),
                                 labels=if(few) ufreq else levels(k$fg)) +
           guides(alpha = FALSE, 
                  color = guide_legend(title='Frequency')) +
           xlab(xlab) + ylab(ylab)

  if(nx) w <- w + scale_x_continuous(breaks=xtrans(xbreaks),
                                     labels=format(xbreaks),
                                     minor_breaks=if(length(xminor))
                                                    xtrans(xminor))
  if(ny) w <- w + scale_y_continuous(breaks=ytrans(ybreaks),
                                     labels=format(ybreaks),
                                     minor_breaks=if(length(yminor))
                                                    ytrans(yminor))
  w
}

utils::globalVariables('fg')

