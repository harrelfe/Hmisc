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
##        1 Oct 13 - added logic to handle excessive ties at start or end x
##        8 Mar 14 - refined that logic, added logic for low # uniques

rcspline.eval <- function(x, knots=NULL, nk=5, inclx=FALSE, knots.only=FALSE,
                          type="ordinary", norm=2, rpm=NULL, pc=FALSE,
                          fractied=0.05)
{
  if(! length(knots)) {   ## knot locations unspecified
    xx <- x[!is.na(x)]
    n <- length(xx)
    if(n < 6)
      stop('knots not specified, and < 6 non-missing observations')
    
    if(nk < 3)
      stop('nk must be >= 3')

    xu  <- sort(unique(xx))
    nxu <- length(xu)
    
    if((nxu - 2) <= nk) {
      warning(sprintf('%s knots requested with %s unique values of x.  knots set to %s interior values.', nk, nxu, nxu - 2))
      knots <- xu[- c(1, length(xu))]
    }
    else {
      outer <- if(nk > 3) .05 else .1
      if(nk > 6) outer <- .025
      
      knots <- numeric(nk)
      overrideFirst <- overrideLast <- FALSE
      nke <- nk
      firstknot <- lastknot <- numeric(0)
      
      if(fractied > 0 && fractied < 1) {
        f <- table(xx) / n
        if(max(f[- c(1, length(f))]) < fractied) {
          if(f[1] >= fractied) {
            firstknot <- min(xx[xx > min(xx)])
            xx <- xx[xx > firstknot]
            nke <- nke - 1
            overrideFirst <- TRUE
          }
          if(f[length(f)] >= fractied) {
            lastknot <- max(xx[xx < max(xx)])
            xx <- xx[xx < lastknot]
            nke <- nke - 1
            overrideLast <- TRUE
          }
        }
      }
      if(nke == 1) knots <- median(xx)
      else {
        if(nxu <= nke) knots <- xu
        else {
          p <- if(nke == 2) seq(.5, 1.0 - outer, length=nke)
          else
            seq(outer, 1.0 - outer, length=nke)
          knots <- quantile(xx, p)
          if(length(unique(knots)) < min(nke, 3)) {
            knots <- quantile(xx, seq(outer, 1.0 - outer, length=2 * nke))
            if(length(firstknot) && length(unique(knots)) < 3) {
              midval <- if(length(firstknot) && length(lastknot))
                (firstknot + lastknot) / 2. else median(xx)
              knots <- sort(c(firstknot, midval,
                              if(length(lastknot)) lastknot
                              else quantile(xx, 1.0 - outer) ))
            }
            if((nu <- length(unique(knots))) < 3) {
              cat("Fewer than 3 unique knots.  Frequency table of variable:\n")
              print(table(x))
              stop()
            }
            
            warning(paste("could not obtain", nke,
                          "interior knots with default algorithm.\n",
                          "Used alternate algorithm to obtain",
                          nu, "knots"))
          }
        }
        
        if(length(xx) < 100) {
          xx <- sort(xx)
          if(! overrideFirst) knots[1]   <- xx[5]
          if(! overrideLast)  knots[nke] <- xx[length(xx) - 4]
        }
      }
      knots <- c(firstknot, knots, lastknot)
    }
  }   ## end knot locations not specified
      
  knots <- sort(unique(knots))
  nk <- length(knots)

  if(nk < 3) {
    cat("fewer than 3 unique knots.  Frequency table of variable:\n")
    print(table(x))
    stop()
  }
  
  if(knots.only) return(knots)
  
  if(length(rpm)) x[is.na(x)] <- rpm
  xx <- matrix(1.1, length(x), nk - 2)
  knot1   <- knots[1     ]
  knotnk  <- knots[nk    ]
  knotnk1 <- knots[nk - 1]
  kd <- if(norm == 0) 1 else if(norm == 1) knotnk - knotnk1 else
    (knotnk - knot1) ^ (2 / 3)

  power <- if(type=="integral") 4 else 3

  for(j in 1 : (nk - 2)) {
    xx[,j] <- pmax((x - knots[j]) / kd, 0) ^ power + 
      ((knotnk1 - knots[j]) * pmax((x - knotnk) / kd, 0) ^ power -
       (knotnk - knots[j]) * (pmax((x - knotnk1) / kd, 0) ^ power)) / 
         (knotnk - knotnk1)
  }
  
  if(power == 4)   xx <- cbind(x, x * x / 2, xx * kd / 4) else
  if(inclx) xx <- cbind(x, xx)
  
  if(pc) {
    p <- prcomp(xx, scale=TRUE, center=TRUE)
    pcparms <- p[c('center', 'scale', 'rotation')]
    xx <- p$x
    attr(xx, 'pcparms') <- pcparms
  }
  attr(xx, 'knots') <- knots
  xx
}
