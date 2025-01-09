## Function like cut but left endpoints are inclusive and labels are of
## the form [lower, upper), except that last interval is [lower,upper].
## F. Harrell  3 Dec 90, modified 7 Mar 92, mod 30May95 (more efficient digits)
## Modified 2Jun95 (preserve label attribute)
## Modified 16Jun95 (categories with 1 unique value -> label=value, not interval)
## Modified 1Jul95 - if specified cuts, mindif would cause improper
##   categorization if a cut was close to but not equal an actual value
## Modified 21oct18 - added formatfun
## Added cutGn 2024-12-25

cut2 <- function(x, cuts, m=150, g, levels.mean=FALSE, digits, minmax=TRUE,
		 oneval=TRUE, onlycuts=FALSE, formatfun = format, ...)
{
  if (inherits(formatfun, "formula")) {
    if (!requireNamespace("rlang"))
      stop("Package 'rlang' must be installed to use formula notation")
    formatfun <- getFromNamespace('as_function', 'rlang')(formatfun)

  }

  
  method <- 1 ## 20may02
  x.unique <- sort(unique(c(x[!is.na(x)],if(!missing(cuts))cuts)))
  min.dif <- min(diff(x.unique))/2
  min.dif.factor <- 1

  ## Make formatted values look good
  if(missing(digits))
    digits <- if(levels.mean) 5 else 3
  
  ## add digits to formatfun's arguments if relevant
  format.args <- 
    if(any(c("...","digits") %in%  names(formals(args(formatfun)))))
      c(digits = digits, list(...))
    else list(...)
  
  oldopt <- options('digits')
  options(digits=digits)
  on.exit(options(oldopt))

  xlab <- attr(x, 'label')

  if(missing(cuts)) {
    nnm <- sum(!is.na(x))
    if(missing(g)) g <- max(1,floor(nnm/m))
    if(g < 1)
      stop('g must be >=1, m must be positive')

    options(digits=15)
    n <- table(x)
    xx <- as.double(names(n))
    options(digits=digits)
    cum <- cumsum(n)
    m <- length(xx)

    y <- as.integer(ifelse(is.na(x),NA,1))
    labs <- character(g)
    cuts <- approx(cum, xx, xout=(1:g)*(nnm/g),
                   method='constant', rule=2, f=1)$y
    cuts[length(cuts)] <- max(xx)
    lower <- xx[1]
    upper <- 1e45
    up <- low <- double(g)
    i <- 0
    for(j in 1:g) {
      cj <- if(method==1 || j==1) cuts[j] else {
        if(i==0)
          stop('program logic error')
        s <- if(is.na(lower)) FALSE else xx >= lower
        cum.used <- if(all(s)) 0 else max(cum[!s])
        if(j==m) max(xx) else if(sum(s)<2) max(xx) else
        approx(cum[s]-cum.used, xx[s], xout=(nnm-cum.used)/(g-j+1),
               method='constant', rule=2, f=1)$y
      }
      
      if(cj==upper) next
      
      i <- i + 1
      upper <- cj
      y[x >= (lower-min.dif.factor*min.dif)]  <- i
      low[i] <- lower
      lower <- if(j==g) upper else min(xx[xx > upper])
      
      if(is.na(lower)) lower <- upper
      
      up[i]  <- lower
    }
    
    low  <- low[1:i]
    up   <- up[1:i]
    variation <- logical(i)
    for(ii in 1:i) {
      r <- range(x[y==ii], na.rm=TRUE)
      variation[ii] <- diff(r) > 0
    }
    if(onlycuts) return(unique(c(low, max(xx))))
    flow <- do.call(formatfun,c(list(low), format.args))
    fup  <- do.call(formatfun,c(list(up),  format.args))
    bb   <- c(rep(')',i-1),']')
    labs <- ifelse(low==up | (oneval & !variation), flow,
                   paste('[',flow,',',fup,bb,sep=''))
    ss <- y==0 & !is.na(y)
    if(any(ss))
      stop(paste('categorization error in cut2.  Values of x not appearing in any interval:\n',
                 paste(format(x[ss],digits=12),collapse=' '),
                 '\nLower endpoints:',
                 paste(format(low,digits=12), collapse=' '),
                 '\nUpper endpoints:',
                 paste(format(up,digits=12),collapse=' ')))

    y <- structure(y, class='factor', levels=labs)
  } else {
    if(minmax) {
      r <- range(x, na.rm=TRUE)
      if(r[1]<cuts[1]) cuts <- c(r[1], cuts)
      if(r[2]>max(cuts)) cuts <- c(cuts, r[2])
    }
    
    l <- length(cuts)
    k2 <- cuts-min.dif
    k2[l] <- cuts[l]
    y <- cut(x, k2)
    
    if(!levels.mean) {
      brack <- rep(")",l-1)
      brack[l-1] <- "]"
      fmt <- do.call(formatfun,c(list(cuts), format.args))
      ## If any interval has only one unique value, set label for
      ## that interval to that value and not to an interval
      labs <- paste("[",fmt[1:(l-1)],",",fmt[2:l],
                    brack,sep="")   
    
      if(oneval) {
        nu <- table(cut(x.unique,k2))
        
        if(length(nu)!=length(levels(y)))
          stop('program logic error')
        levels(y) <- ifelse(nu==1,c(fmt[1:(l-2)],fmt[l]),labs)
      } else
        levels(y) <- labs
    }
  }

  if(levels.mean) {
    means <- tapply(x, y, function(w)mean(w,na.rm=TRUE))
    levels(y) <- do.call(formatfun,c(list(means), format.args))
  }
  attr(y,'class') <- "factor"
  if(length(xlab)) label(y) <- xlab
  y
}

cutGn <- function(x, m, what=c('mean', 'factor', 'summary', 'cuts', 'function'),
                  rcode=FALSE) {
  what  <- match.arg(what) 
  notna <- which(! is.na(x))
  y <- x[notna]
  n <- length(y)
  if(n <= m) stop('number of non-NA observations must exceed m')
  # Create a group for every m observations in ascending order
  io <- order(y)
  s  <- y[io]
  ie <- 0
  g  <- 0
  G  <- rep(0L, n)
  if(rcode) {
  while(TRUE) {
    is     <- ie + 1
    lte    <- is + m - 1   # last targeted observation in group
    # Just use the insufficient group < m obs.; pool it with previous group
    # by not incrementing g
    if(lte > n) {
      G[is : n] <- g
      break
    }
    # If the mth observation is the nth of the non-NAs, finish with
    # the current group as the final group
    g <- g + 1
    if(lte == n) {
      G[is : n] <- g
      break
    }
    # There are observations beyond the last of the m in the current group
    # See if the values beyond the mth current group's value are tied
    # with the last value in the current group.  If so, pool observations
    # into the current group up intil the observation that differs from
    # the last of the m
    lastval <- s[lte]
    if(s[lte + 1] == lastval) {
      # See how far the tied values go, and consume all of those tied at lastval
      k  <- rle(as.vector(s)[(lte + 1) : n])$lengths[1]
      ie <- lte + k
      G[is : ie] <- g
    } else {
      ie <- lte
      G[is : ie] <- g
    }
    if(ie > n) stop('logic problem')
    if(ie == n) break
  }
  }   # end if(rcode)
  else {
    storage.mode(s) <- 'double'
    G <- .Fortran(F_cutgn, s, n, as.integer(m), G=G)$G
  }

  # Put data in original x order
  j <- order(io)
  s <- s[j]
  G <- G[j]

  g <- max(G)
  if(what %in% c('mean', 'function')) {
    smean    <- tapply(s, G, mean)
    x[notna] <- smean[G]
    if(what == 'mean') return(x)
  }

  # For each group get min and max original y
  ymin  <- tapply(s, G, min)
  ymax  <- tapply(s, G, max)

  if(what == 'cuts') return(unique(c(ymin, max(ymax))))

  if(what == 'summary') {
    count <- tapply(s, G, length)
    return(cbind(min=ymin, max=ymax, n=count))
  }

  # Create factor variable with character string interval labels
  # When an interval is a point just use the point
  lev <- ifelse(ymin == ymax, format(ymin),
                paste0('[', format(ymin), ',', format(ymax), ']'))
  if(what == 'factor') return(factor(G, 1 : max(G), lev))

  h <- function(x, lower, upper, means, levels, what) {
    what <- match.arg(what)
    nint <- length(lower)
    u <- unique(c(lower, max(upper)))
    y <- approx(u, 1 : length(u), xout=x, method='constant')$y
    # y values > # intervals are equal to last upper and need -1
    y[! is.na(y) & y > nint] <- nint
    if(what== 'mean') means[y] else factor(y, 1 : nint, levels)
  }
  formals(h) <- list(x=numeric(0), lower=unname(ymin),
                     upper=unname(ymax), means=unname(smean),
                     levels=unname(lev), what=c('mean', 'factor'))
  h
}
