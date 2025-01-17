rcspline.plot <- function(x, y, model=c("logistic","cox","ols"), xrange,
                          event, nk=5, knots=NULL, show=c("xbeta", "prob"),
                          adj=NULL, xlab, ylab, ylim, plim=c(0,1),
                          plotcl=TRUE, showknots=TRUE, add=FALSE, subset,
                          lty=1, noprint=FALSE, m, smooth=FALSE, bass=1,
                          main="auto", statloc)
{
  model <- match.arg(model)
  show <- match.arg(show)
  
  if(! missing(event))
    model<-"cox"
  
  if(model == "cox" & missing(event))
    stop('event must be given for model="cox"')
  
  if(show == "prob" & ! missing(adj))
    stop('show="prob" cannot be used with adj')
  
  if(show == "prob" & model != "logistic")
    stop('show="prob" can only be used with model="logistic"')
  
  if(length(x) != length(y))
    stop('x and y must have the same length')
  
  if(! missing(event) && length(event) != length(y))
    stop('y and event must have the same length')
  
  if(! missing(adj)) {
    if(! is.matrix(adj)) adj <- as.matrix(adj)
    if(dim(adj)[1] != length(x))
      stop('x and adj must have the same length')
  }
  
  if(missing(xlab))
    xlab <- label(x)
  
  if(missing(ylab))
    ylab <- label(y)
  
  isna <- is.na(x) | is.na(y) 
  if(! missing(event))
    isna <- isna | is.na(event)
  
  nadj <- 0
  if(! missing(adj)) {
    nadj <- ncol(adj)
    isna <- isna | apply(is.na(adj), 1, sum) > 0
  }
  
  if(! missing(subset))
    isna <- isna | (! subset)
  
  x <- x[! isna]
  y <- y[! isna]
  if(! missing(event))
    event <- event[! isna]
  
  if(! missing(adj))
    adj <- adj[! isna, ]
  
  n <- length(x)
  if(n<6)
    stop('fewer than 6 non-missing observations')
  
  if(missing(xrange)) {
    frac<-10./max(n, 200)
    xrange<-quantile(x, c(frac, 1.-frac))
  }
  
  if(missing(knots))
    xx <- rcspline.eval(x, nk=nk)
  else xx <- rcspline.eval(x, knots)
  
  knots <- attr(xx, "knots")
  nk <- length(knots)

  df1 <- nk-2
  if(model == "logistic") {
    if (!requireNamespace("rms", quietly = TRUE))
      stop("The 'logistic' model requires the 'rms' package.")
    b <- rms::lrm.fit(cbind(x, xx, adj),  y)
    beta <- b$coef
    cov  <- vcov(b)
    model.lr <- b$stats["Model L.R."]
    offset <- 1 	#to skip over intercept parameter
    ylabl <-
      if(show == "prob")
        "Probability"
      else "log Odds"
    
    sampled <- paste("Logistic Regression Model,  n=", n," d=", sum(y), sep="")
  }
  
  if(model == "cox") {
    sRequire('survival')

    ##11mar04
    
    ## added coxph.control around iter.max, eps  11mar04
    lllin <- survival::coxph.fit(cbind(x, adj), cbind(y, event), strata=NULL,
                       offset=NULL, init=NULL,
                       control=survival::coxph.control(iter.max=10, eps=.0001), 
                       method="efron", rownames=NULL)$loglik[2]
    b <- survival::coxph.fit(cbind(x, xx, adj), cbind(y, event), strata=NULL,
                   offset=NULL, init=NULL,
                   control=survival::coxph.control(iter.max=10, eps=.0001), 
                   method="efron", rownames=NULL)
    beta <- b$coef
    if(! noprint) {
      print(beta);
      print(b$loglik)
    }
    
    beta <- b$coef
    cov <- vcov(b)
    model.lr <- 2*(b$loglik[2]-b$loglik[1])
    offset <- 0
    ylabl <- "log Relative Hazard"
    sampled <- paste("Cox Regression Model, n=",n," events=",sum(event),
                     sep="")
  }
  
  if(model == "logistic" | model == "cox") {
    model.df <- nk - 1 + nadj
    model.aic <- model.lr-2.*model.df
    v <- solve(cov[(1 + offset) : (nk + offset - 1), (1 + offset) : (nk + offset - 1)])
    assoc.chi <- beta[(1 + offset) : (nk + offset - 1)] %*% v %*%
      beta[(1 + offset) : (nk + offset - 1)]
    assoc.df <- nk - 1   #attr(v,"rank")
    assoc.p <- 1.-pchisq(assoc.chi, nk - 1)
    v <- solve(cov[(2 + offset) : (nk + offset - 1), (2 + offset) : (nk + offset - 1)])
    linear.chi <- beta[(2 + offset) : (nk + offset - 1)] %*% v %*%
      beta[(2 + offset) : (nk + offset - 1)]
    linear.df <- nk - 2   #attr(v,"rank")
    linear.p <- 1. - pchisq(linear.chi, linear.df)
    if(nadj > 0) {
      ntot <- offset + nk - 1 + nadj
      v <- solve(cov[(nk + offset) : ntot, (nk + offset) : ntot])
      adj.chi <- beta[(nk + offset) : ntot] %*% v %*%
        beta[(nk + offset) : ntot]
      adj.df <- ncol(v)   #attr(v,"rank")
      adj.p <- 1. - pchisq(adj.chi, adj.df)
    } else {
      adj.chi <- 0
      adj.p <- 0
    }
  }

  ## Evaluate xbeta for expanded x at desired range
  xe <- seq(xrange[1], xrange[2], length=600)
  if(model == "cox")
    xx <- rcspline.eval(xe, knots, inclx=TRUE)
  else
    xx<- cbind(rep(1, length(xe)), rcspline.eval(xe, knots, inclx=TRUE))
  
  xbeta <- xx %*% beta[1 : (nk - 1 + offset)]
  var <- drop(((xx %*% cov[1 : (nk - 1 + offset), 1 : (nk - 1 + offset)])*xx) %*% 
              rep(1, ncol(xx)))
  lower <- xbeta - 1.96*sqrt(var)
  upper <- xbeta + 1.96*sqrt(var)
  if(show == "prob") {
    xbeta <- 1./(1. + exp(-xbeta))
    lower <- 1./(1. + exp(-lower))
    upper <- 1./(1. + exp(-upper))
  }
  
  xlim <- range(pretty(xe))
  if(missing(ylim))
    ylim <- range(pretty(c(xbeta, if(plotcl) lower, if(plotcl) upper)))
  
  if(main == "auto") {
    if(show == "xbeta")
      main <- "Estimated Spline Transformation"
    else main <- "Spline Estimate of Prob{Y=1}"
  }
  
  if(! interactive() & missing(statloc))
    statloc<-"ll"
  
  if(! add) {
    oldmar<-par("mar")
    if(! missing(statloc) && statloc[1] == "ll")
      oldmar[1]<- 11
    
    oldpar <- par(err= - 1, mar=oldmar)
    plot(xe, xbeta, type="n", main=main, xlab=xlab, ylab=ylabl,
         xlim=xlim, ylim=ylim)
    lines(xe, xbeta, lty=lty)
    ltext<-function(z, line, label, cex=.8, adj=0)
    {
      zz<-z
      zz$y<-z$y-(line - 1)*1.2*cex*par("csi")*(par("usr")[4]-par("usr")[3])/
        (par("fin")[2])   #was 1.85
      text(zz, label, cex=cex, adj=adj)
    }
    
    sl<-0
    if(missing(statloc)) {
      cat("Click left mouse button at upper left corner for statistics\n")
      z<-locator(1)
      statloc<-"l"
    } else if(statloc[1] != "none") {
      if(statloc[1] == "ll") {
        z<-list(x=par("usr")[1], y=par("usr")[3])
        sl<-3
      } else z<-list(x=statloc[1], y=statloc[2])
    }
    
    if(statloc[1] != "none" & (model == "logistic" | model == "cox"))	{
      rnd <- function(x, r=2) as.single(round(x, r))
      
      ltext(z, 1 + sl, sampled)
      ltext(z, 2 + sl, "    Statistic        X2  df")
      chistats<-format(as.single(round(c(model.lr, model.aic,
                                         assoc.chi, linear.chi, adj.chi), 2)))
      pvals<-format(as.single(round(c(assoc.p, linear.p, adj.p), 4)))
      ltext(z, 3 + sl, paste("Model        L.R. ", chistats[1], model.df,
                         " AIC=", chistats[2]))
      ltext(z, 4 + sl, paste("Association  Wald ", chistats[3], assoc.df,
                         " p= ", pvals[1]))
      ltext(z, 5 + sl, paste("Linearity    Wald ", chistats[4], linear.df, 
                         " p= ", pvals[2]))
      if(nadj > 0)ltext(z, 6 + sl, paste("Adjustment   Wald " , chistats[5],
                                   adj.df, " p= ", pvals[3]))}
  } else lines(xe, xbeta, lty=lty)
  
  if(plotcl) {
    prn(cbind(xe, lower, upper))
    lines(xe, lower, lty=2)
    lines(xe, upper, lty=2)	
  }

  if(showknots) {
    bot.arrow <- par("usr")[3]
    top.arrow <- bot.arrow + .05 * (par("usr")[4]-par("usr")[3])
    for(i in 1 : nk)
        arrows(knots[i], top.arrow, knots[i], bot.arrow, length=.1)
  }
  
  if(model == "logistic" & nadj == 0) {
    if(smooth) {
      z<-supsmu(x, y, bass=bass)
      if(show == "xbeta") z$y <- logb(z$y/(1.-z$y))
      points(z, cex=.4)
    }
    
    if(! missing(m)) {
      z<-groupn(x, y, m=m)
      if(show == "xbeta") z$y <- logb(z$y/(1.-z$y))
      
      points(z, pch=2, mkh=.05)}
  }
  
  if(! add)
    par(oldpar)
  
  invisible(list(knots=knots, x=xe, xbeta=xbeta, lower=lower, upper=upper))
}
