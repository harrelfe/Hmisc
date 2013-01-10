confbar <- function(at, est, se, width,
                    q=c(.7,.8,.9,.95,.99), 
                    col=gray(c(0,.25,.5,.75,1)),
                    type=c("v","h"), labels=TRUE, ticks=FALSE,
                    cex=.5, side="l", lwd=5, clip=c(-1e30, 1e30),
                    fun=function(x)x, 
                    qfun=function(x)
                           ifelse(x==.5, qnorm(x),
                                  ifelse(x<.5,qnorm(x/2),qnorm((1+x)/2))))
{
  type <- match.arg(type)
  iusr <- if(type=="v")
            1:2
          else
            3:4
  
  if(missing(width))
    width <- diff(par("usr")[iusr])*.02
  
  if(side=="b")
    side <- "l"    #treat bottom as left
  
  if(length(q)!=length(col))
    stop("q and col must have same length")
  
  q <- c(1-rev(q), .5, q)
  ##qe <- seq(.01, .99, length=n)
  ##col <- seq(.8,.01, length=n/2)
  col <- c(rev(col), col)
  w <- width/2
  if(type=="v") {
    polyg <- function(a, b, col, clip)
    {
      b[b < clip[1] | b > clip[2]] <- NA
      polygon(a, b, col=col)
    }
    
    Lines <- function(a, b, lwd=1, clip)
    {
      b[b < clip[1] | b > clip[2]] <- NA
      lines(a, b, lwd=lwd)
    }
    
    Text  <- function(a, b, clip, ...)
    {
      b[b < clip[1] | b > clip[2]] <- NA
      text(a, b, ...)
    }
    
    srt <- 0
  } else {
    polyg <- function(a, b, col, clip)
    {
      b[b < clip[1] | b > clip[2]] <- NA
      polygon(b, a, col=col)
    }
    
    Lines <- function(a, b, lwd=1, clip)
    {
      b[b < clip[1] | b > clip[2]] <- NA
      lines(b, a, lwd=lwd)
    }
    
    Text  <- function(a, b, clip, ...)
    {
      b[b < clip[1] | b > clip[2]] <- NA
      text(b, a, ...)
    }
    
    srt   <- 45
  }
  for(i in 1:(length(q)-1))
    polyg(c(at-w,at+w,at+w,at-w),fun(est+se*qfun(c(q[i],q[i],q[i+1],q[i+1]))),
          col=col[i], clip=clip)
  
  a <- fun(est)
  z <- w*.24
  Lines(c(at-w-3.5*z, at+w+3.5*z), c(a,a), lwd=lwd, clip=clip)
  a <- fun(est+se*qfun(q))
  do <- TRUE
  if(labels || ticks)
    for(i in 1:length(q)) {
      b <- c(a[i], a[i])
      if(ticks) {
        Lines(c(at-w-z,at-w),b, clip=clip)
        Lines(c(at+w+z,at+w),b, clip=clip)
      }
      
      if(labels && do && q[i]!=.5) {
        if(side=="l")
          Text(at-w-2*z, a[i], format(max(1-q[i],q[i])), 
               cex=cex, adj=1, srt=srt, clip=clip)
        else
          Text(at+w+2*z, a[i], format(max(1-q[i],q[i])), 
               cex=cex, adj=0, srt=srt, clip=clip)
      }
      
      if(q[i]!=.5)
        do <- !do
    }
  
  names(a) <- format(q)
  invisible(a)
}
