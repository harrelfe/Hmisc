##rcspline.eval - function to create design matrix for restricted cubic
##	spline function of Stone & Koo, given an input vector and optionally
##	a vector of knots.  If knots are not given, knots are set using
##	default algorithm.  If the number of knots is not given, 5 are used.
##	Terms are normalized by (outer-inner knot)^2.
##	Can optionally return antiderivative of spline functions if
##	type="integral".
##	norm=0 : no normalization of constructed variables
##	norm=1 : divide by cube of difference in last 2 knots
##		 makes all variables unitless
##	norm=2 : (default) divide by square of difference in outer knots
##		 makes all variables in original units of x
##
##	Returns:
##		x - design matrix for derived spline variables
##		(includes original x in first column if inclx=T or 
##		 type="integral")
##		attribute knots - input or derived vector of knots
##	If knots.only=T, returns instead the vector of estimated or given
##	knots.
##	If rpm is not null, replaces missing x with rpm before evaluating
##	but after estimating knots.
##
##	F. Harrell 13 Feb 90
##       Modified   28 Mar 90 - improved default knot computation
##		   22 Aug 90 - put knots as attribute, return matrix
##		   20 Sep 90 - added knots.only argument
##		   16 Oct 90 - added rpm argument
##		   11 Dec 91 - added type argument
##		   27 Dec 91 - added norm argument
##		   26 Jun 93 - added evasive action if <3 knots

rcspline.eval <- function(x,knots=NULL,nk=5,inclx=FALSE,knots.only=FALSE,
                          type="ordinary",norm=2, rpm=NULL)
{
  if(!length(knots)) {
    xx <- x[!is.na(x)]
    n <- length(xx)
    if(n<6)
      stop('fewer than 6 non-missing observations with knots omitted')
    
    if(nk<3)
      stop('nk must be >= 3')
    
    outer <- .1
    if(nk>3)
      outer <- .05
    
    if(nk>6)
      outer <- .025
    
    knots <- quantile(xx,seq(outer,1.0-outer,length=nk))
    if(length(unique(knots))<3) {
      knots <- quantile(xx,seq(outer,1.0-outer,length=2*nk))
      if((nu <- length(unique(knots)))<3) {
        cat("Fewer than 3 unique knots.  Frequency table of variable:\n")
        print(table(xx))
        stop()
      }
      
      warning(paste("could not obtain",nk,"knots with default algorithm.\n",
                    "Used alternate algorithm to obtain",
                    nu,"knots"))

    }
    
    if(n<100) {
      xx <- sort(xx)
      knots[1]<-xx[5]
      knots[nk]<-xx[n-4]
    }
  }
  
  knots <- sort(unique(knots))
  nk <- length(knots)
  if(nk<3) {
    cat("fewer than 3 unique knots.  Frequency table of variable:\n")
    print(table(x))
    stop()
  }

  if(knots.only)
    return(knots)

  ##x <- as.matrix(x)     10Mar01
  ##storage.mode(x) <- "single"
  if(!is.null(rpm))
    x[is.na(x)] <- rpm
  
  xx <- matrix(1.1,length(x),nk-2)  # 10Mar01
  knot1 <- knots[1]
  knotnk <- knots[nk]
  knotnk1 <- knots[nk-1]
  if(norm==0)
    kd <- 1
  else if(norm==1)
    kd <- knotnk-knotnk1
  else
    kd <- (knotnk-knot1)^.66666666666666666666666

  if(type=="integral")
    power <- 4
  else power <- 3

  for(j in 1:(nk-2)) {
    xx[,j]<-pmax((x-knots[j])/kd,0)^power + 
      ((knotnk1-knots[j])*pmax((x-knotnk)/kd,0)^power -
       (knotnk-knots[j])*(pmax((x-knotnk1)/kd,0)^power))/
         (knotnk-knotnk1)
  }

  if(power==4)
    xx <- cbind(x, x*x/2, xx*kd/4)
  else if(inclx)
    xx <- cbind(x, xx)
  
  if(!.R.)
    storage.mode(xx) <- 'single'  # 10Mar01
  
  attr(xx,"knots") <- knots
  xx
}
