## Werner, Martin and Tim have added several useful
## things. At the end of this e-mail there is our final result.
##
## As an example we reproduced a similar figure as Fig. 4.23 of Chambers et
## al. (1983) "Graphical Methods For Data Analysis":
##
## ii_3:4
## x <- matrix(aperm(iris[,ii,], perm =c(1,3,2)), ncol=2,
##             dimnames=list(dimnames(iris)[[1]],dimnames(iris)[[2]][ii]))
## xr <- round(2*x,1)/2
## nam <- dimnames(xr)[[2]]
## p.sunflowers(xr[,1],xr[,2], xlab=nam[1], ylab=nam[2], size= 1/16,
##              main="Iris data")
##
##
## Andreas Ruckstuhl <ruckstuhl@stat.math.ethz.ch>			
## Seminar fuer Statistik, SOL G5, ETH (Federal Institute of Technology)
## 8092 Zurich	SWITZERLAND  	phone: x-41-1-256-5319  fax: x-41-1-252-3410
##
##
##================================ S function ========================
##

if(!.R.) {
  p.sunflowers <- function(x, y, number, size = 0.125, add = FALSE,
                           pch = 16, ...)
  {
    ## Purpose: Produce a 'sunflower'-Plot
    ## -------------------------------------------------------------------------
    ## Arguments: x,y: coordinates;
    ##    number[i] = number of times for (x[i],y[i])  [may be 0]
    ##    size: in inches;  1 in := 2.54 cm
    ##    add : (logical) Should I add to a previous plot ?
    ##    further args: as for plot(..)
    ## -------------------------------------------------------------------------
    ## Authors: Andreas Ruckstuhl, Werner Stahel, Martin Maechler, Tim Hesterberg
    ## Date   : Aug 89 / Jan 93,   March 92,      Jan 93,          Jan 93
    ## Examples: p.sunflowers(x=sort(round(rnorm(100))), y= round(2*rnorm(100),0))
    ## ~~~~~~~~  p.sunflowers(rnorm(100),rnorm(100), number=rpois(n=100,lambda=2), 
    ##                        main="Sunflower plot")
    
    n <- length(x)
    if(length(y) != n)
      stop("x & y must have same length !")
    
    if(missing(number)) {
      orderxy <- order(x, y)
      x <- x[orderxy]
      y <- y[orderxy]
      first <- c(TRUE, (x[-1] != x[ - n]) | (y[-1] != y[ - n]))
      x <- x[first]
      y <- y[first]
      number <- diff(c((1:n)[first], n + 1))
    } else {
      if(length(number) != n)
        stop("number must have same length as x & y !")
      
      x <- x[number > 0]
      y <- y[number > 0]
      number <- number[number > 0]
    }

    n <- length(x)
    if(!add) {
      axislabels <- match(c("xlab", "ylab"), names(list(...)))
      if(!is.na(axislabels[1]))
        xlab <- list(...)[[axislabels[1]]]
      else xlab <- deparse(substitute(x))
      
      if(!is.na(axislabels[2]))
        ylab <- list(...)[[axislabels[2]]]
      else ylab <- deparse(substitute(y))

      plot(x, y, xlab = xlab, ylab = ylab, type = "n", ...)
    }

    nequ1 <- number == 1
    if(any(nequ1))
      points(x[nequ1], y[nequ1], pch = pch, csi = size * 1.25)

    if(any(!nequ1))
      points(x[!nequ1], y[!nequ1], pch = pch, csi = size * 0.8)

    i.multi <- (1:n)[number > 1]
    if(length(i.multi)) {
      ppin <- par()$pin
      pusr <- par()$usr
      xr <- (size * abs(pusr[2] - pusr[1]))/ppin[1]
      yr <- (size * abs(pusr[4] - pusr[3]))/ppin[2]
      i.rep <- rep(i.multi, number[number > 1])
      z <- NULL
      for(i in i.multi)
        z <- c(z, 1:number[i])

      deg <- (2 * pi * z)/number[i.rep]
      segments(x[i.rep], y[i.rep], x[i.rep] + xr * sin(deg), y[i.rep] +
               yr * cos(deg))
    }
    
    invisible()
  }
  
  NULL
}
