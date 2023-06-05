## $Id$

transcan <-
  function(x, method=c("canonical","pc"),
           categorical=NULL, asis=NULL, nk, 
           imputed=FALSE, n.impute, 
           boot.method=c('approximate bayesian', 'simple'),
           trantab=FALSE, transformed=FALSE,
           impcat=c("score","multinom","rpart"),
           mincut=40,
           inverse=c('linearInterp','sample'), tolInverse=.05,
           pr=TRUE, pl=TRUE, allpl=FALSE, show.na=TRUE,
           imputed.actual=c('none','datadensity','hist','qq','ecdf'),
           iter.max=50, eps=.1, curtail=TRUE, 
           imp.con=FALSE, shrink=FALSE, init.cat="mode",
           nres=if(boot.method=='simple')200 else 400,
           data, subset, na.action, treeinfo=FALSE,
           rhsImp=c('mean','random'),
           details.impcat='', ...)
{
  ##This is a non-.Internal version of the approx function.  The
  ##S-Plus version of approx sometimes bombs with a bus error.

  asing <- function(x)x
  
  call        <- match.call()
  method      <- match.arg(method)
  impcat      <- match.arg(impcat)
  boot.method <- match.arg(boot.method)
  imputed.actual <- match.arg(imputed.actual)
  inverse     <- match.arg(inverse)
  rhsImp      <- match.arg(rhsImp)

  if(missing(n.impute))  n.impute <- 0
  
  if(n.impute > 0) {
    imputed <- TRUE
    if(impcat == 'rpart')
      stop('n.impute not supported for impcat="rpart"')
    
    warning('transcan provides only an approximation to true multiple imputation.\nA better approximation is provided by the aregImpute function.\nThe MICE and other S libraries provide imputations from Bayesian posterior distributions.')
  }

  if(imputed.actual!='none') imputed <- TRUE

#  if(impcat=='multinom') require(nnet)
#  if(impcat=='rpart') require(rpart)

  formula <- nact <- NULL

  if(inherits(x,"formula")) {
    formula <- x
    y <- match.call(expand.dots=FALSE)
    y$x <- y$method <- y$categorical <- y$asis <- y$nk <- y$imputed <- 
      y$trantab <- y$impcat <- y$mincut <- y$pr <- y$pl <- y$allpl <- y$show.na <-
        y$iter.max <- y$eps <- y$curtail <- y$imp.con <- y$shrink <-
          y$init.cat <- y$n.impute <- y$... <- y$nres <- y$boot.method <-
            y$transformed <- y$treeinfo <- y$imputed.actual <-
              y$inverse <- y$tolInverse <- y$details.impcat <-
                y$rhsImp <- NULL
    y$formula <- x
  
    if(missing(na.action))
      y$na.action <- na.retain

    y[[1]] <- as.name("model.frame")
    y <- eval(y, sys.parent())
    nact <- attr(y,"na.action")
    d <- dim(y)

    # Error if user is trying to use a non-allowed formula
    if(length(attr(y, "terms")) > 2)
      stop('transcan does not support a left hand side variable in the formula')


    nam <- all.vars(attr(y, "terms"))

    # Error if user has passed an invalid formula
    if(length(nam) != d[2])
      stop(paste('Formula', formula,
                 'does not have a dominant inner variable.'))
    
    if(!length(asis)) {
      Terms <- terms(formula, specials='I')
      asis <- nam[attr(Terms,'specials')$I]
      ## all.vars will cause I() wrapper to be ignored
    }

    x <- matrix(NA,nrow=d[1],ncol=d[2],
                dimnames=list(attr(y,"row.names"),nam))
    for(i in 1:d[2]) {
      w <- y[[i]]
      if(is.character(w))
        w <- factor(w)

      if(is.factor(w)) { 
        x[,i] <- unclass(w)
        categorical <- c(categorical, nam[i])
      } else {
        x[,i] <- w
        nu <- length(unique(w[!is.na(w)]))
        if(nu<2)
          stop(paste("variable",nam[i],"has only one value"))
        
        if(nu==2)
          asis <- c(asis, nam[i])
        else if(nu==3) categorical <- c(categorical, nam[i])
      }
    }
  }

  nam <- dimnames(x)[[2]]
  rnam <- dimnames(x)[[1]]
  if(length(rnam)==0)
    rnam <- as.character(1:nrow(x))
  
  p <- ncol(x)
  if(!length(nam))
    stop("x must have column names")

  n <- nrow(x)
  if(missing(nk))
    nk <- 3*(n<30)+4*(n>=30 & n<100)+5*(n>=100)

  ## Compute constant to multiply canonical variates by to get a variance of 1.0
  varconst <- sqrt(n-1)

  if(length(categorical)) { 
    if(length(categorical)==1 && categorical=="*")
      categorical <- nam

    ## oldopts <- options(c('na.action','contrasts'))
    ## R does not allow multiple options to be spec.
    oldopts <- options()
    ##  names(oldopts) <- c('na.action','contrasts') #windows can mess this up
    if(impcat == 'rpart') {
      options(contrasts=c("contr.treatment","contr.poly"))
      on.exit(options(oldopts))
    }
  }

  if(length(asis)==1 && asis=="*")
    asis <- nam

  R <- parms <- coef <- fill.con <- Imputed <- Trantab <- vector("list",p)
  fillin <- rep(NA,p);
  names(fillin) <- nam
  scale <- rep(1,p);
  names(scale) <- nam;
  
  names(Trantab) <- nam
  nparm <- shr <- fillin
  if(n.impute > 0) {
    Resid <- vector("list",p)
    names(Resid) <- nam
  } else Resid <- NULL

  datad <- list();
  datad.ranges <- list()

  ## For canonical-variate expansions (standardized), use scale of 1
  xcoef <- matrix(NA, nrow=p, ncol=p+1, dimnames=list(nam,c("intercept",nam)))
  usefill <- 1*(is.logical(imp.con) && imp.con)+2*(is.numeric(imp.con))
  if(usefill==2 && length(imp.con)!=p)
    stop("length of imp.con != ncol(x)")

  for(i in 1:p) {
    lab <- nam[i]
    y <- x[,i]
    na <- is.na(y)
    w <- y[!na]
    if(imputed && n.impute==0)
      Imputed[[i]] <- double(sum(na))

    if(lab %in% asis) {
      fillin[i] <- if(usefill==2) imp.con[i]
                   else median(w)

      scale[i] <- mean(abs(w-fillin[i]))
      if(is.na(fillin[i]))
        stop(paste("fillin value for",lab,"is NA"))

      coef[[i]] <- c(0,1)
      nparm[i] <- 1
    } else {
      if(lab %in% categorical) {
        w <- table(y)
        z <- as.numeric(names(w))
        if(usefill==2) fillin[i] <- imp.con[i]
        else fillin[i] <- z[w==max(w)][1] #most freq. category

        assign("Y", as.factor(y))
        opold <- options(na.action="na.retain")
        w <- model.matrix(~Y) # uses contr.treatment (reference cell coding)
        options(na.action=opold[[1]]) #for some reason Windows needs opt name
        r <- attr(w,"contrasts")[[1]]
        attr(r,"codes") <- z
        parms[[i]] <- r
        R[[i]] <- w[,-1,drop=FALSE]   #kill intercept column
        nparm[i] <- length(z)-1
        if(usefill>0) {
          fill.con[[i]] <- w[y==imp.con[i],-1,drop=FALSE][1,,drop=FALSE]
          ##select first hit
          if(length(fill.con[[i]])==0)
            stop("imp.con has a code not in the data for a categorical var")
        }
      } else {
        fillin[i] <- if(usefill==2) imp.con[i]
                     else median(y[!is.na(y)])
        R[[i]] <- rcspline.eval(y, nk=nk, inclx=TRUE)
        parms[[i]] <- attr(R[[i]], "knots")
        if(usefill>0)
          fill.con[[i]] <- 
            rcspline.eval(fillin[i], parms[[i]], inclx=TRUE)

        nparm[i] <- length(parms[[i]])-1
      }
    }
  }

  xt <- x
  if(init.cat %in% c("mode","random")) for(i in (1:p)[nam %in% categorical])
    xt[,i] <-
      if(init.cat=="mode") {
        if(is.na(fillin[i]))
          stop(paste("fillin value for",nam[i],"is NA"))

        xt[,i]==fillin[i]
      } else runif(n)

  p1 <- p-1
  R2 <- R2.adj <- double(p)

  r2 <- r2.adj <- NA
  Details.impcat <- NULL

  last.iter <- FALSE
  if(pr) cat("Convergence criterion:")

  milab <- as.character(1:n.impute)

  predSamp <- function(res, yhat, rnam, allowed.range, n.impute,
                       boot.method)
  {
    m <- length(yhat)
    yhat <- matrix(rep(yhat, n.impute), ncol=n.impute,
                   dimnames=list(rnam, as.character(1:n.impute)))
    errors <- if(boot.method=='simple')
      sample(res, m*n.impute, replace=TRUE)
    else {
      ## From Jeff Longmate (jlongmat@coh.org):
      n <- length(res)
      i <- ceiling(runif(n*n.impute, 0, n))
      j <- ceiling(runif(m*n.impute, 0, n)) +
        rep((0:(n.impute-1))*n, rep(m, n.impute))
      res[i[j]]
    }

    structure(pmax(pmin(yhat + errors, allowed.range[2]), allowed.range[1]),
              names=NULL)
  }

  anyVarNA <- rep(FALSE, n)

  for(iter in 1:iter.max) {
    dmax <- 0
    if(last.iter) xtl <- xt
    for(i in 1:p) {
      lab <- nam[i]
      catg <- lab %in% categorical
      xx <- xt[,-i,drop=FALSE]
      k.other <- sum(pmax(nparm[-i]-1,0))/(p-1)+p-1  #effective d.f.
      if(iter==1) {
        for(j in 1:p1) {
          if(any(z <- is.na(xx[,j])))	{
            l <- (nam[-i])[j]
            if(is.na(fillin[l]))
              stop(paste("variable",l,"has fillin value of NA"))

            xx[z,j] <- fillin[l]
          }
        }
      }

      if(method=="pc") {
        z <- xx
        for(k in 1:p1) {
          y <- z[,k];
          z[,k] <- (y-mean(y))/sqrt(var(y))
        }

        P <- prcomp(z)$x[,1]   # 1st prin. comp.
      }

      j <- is.na(x[,i])
      anyVarNA[j] <- TRUE
      if(lab %in% asis) {
        y <- x[!j, i]
        f <- lm.fit.qr.bare(xx[!j,,drop=FALSE], y)
        newy <- x[,i]
        names(newy) <- NULL
        xcof <- f$coef
        r2 <- f$rsquared
        nn <- length(y)
        r2.adj <- max(0,1-(1-r2)*(nn-1)/(nn-k.other-1))
        if(shrink) {
          ybar <- mean(y)
          shr[i] <- h <- (nn-k.other-1)*r2/(nn-1)/r2.adj
          xcof <- c(ybar*(1-h)+h*xcof[1],h*xcof[-1])
        }

        if(any(j))
          newy[j] <-
            if(usefill>0) fillin[i]
            else cbind(1,xx[j,,drop=FALSE]) %*% xcof

        res <- f$residuals
        
        if(last.iter) {
          ybar <- mean(y)
          if(imputed & any(j)) {
            r <- range(newy[!j])
            Imputed[[i]] <-
              if(n.impute==0)structure(pmax(pmin(newy[j],r[2]),r[1]),
                                       names=rnam[j])
              else
                predSamp(res, newy[j], rnam[j], r, n.impute, boot.method)

            NULL
          }

          xcoef[i, c("intercept",nam[-i])] <- xcof
          if(trantab) {
            rr <- range(y);
            Trantab[[i]] <- list(x=rr, y=rr);
            NULL
          }

          if(n.impute > 0)
            Resid[[i]] <- 
              if(length(res) <= nres) asing(res)
              else asing(sample(res, nres))

          ## was f$residuals 3 times
        }
      } else {
        f <- cancor(xx[!j,,drop=FALSE], R[[i]][!j,,drop=FALSE])
        r2 <- f$cor[1]^2
        xcof <- c(intercept=-sum(f$xcoef[,1] * f$xcenter),
                  f$xcoef[,1])*varconst
        cof <-
          if(method=="canonical") 
            c(intercept=-sum(f$ycoef[,1] * f$ycenter), 
              f$ycoef[,1])*varconst
          else {
            g <- lm.fit.qr.bare(R[[i]][!j,,drop=FALSE], P[!j])
            g$coef
          }

        newy <- drop(cbind(1,R[[i]]) %*% cof)
        if((n.impute > 0 && last.iter) || rhsImp=='random') 
          res <-
            if(method=='canonical') 
              newy[!j] - cbind(1,xx[!j,,drop=FALSE]) %*% xcof
            else g$residuals

        
        if(n.impute > 0 && last.iter) {
          Resid[[i]] <-
            if(length(res) <= nres) asing(res)
            else asing(sample(res, nres))
        }

        nn <- n - sum(j)
        k <- nparm[i]-1+k.other
        r2.adj <- max(0,1-(1-r2)*(nn-1)/(nn-k-1))
        if(shrink) {
          shr[i] <- h <- (nn-k-1)*r2/(nn-1)/r2.adj
          xcof <- h*xcof   #mean of can var=0
        }

        if(any(j)) newy[j] <- 
          if(usefill>0) drop(cbind(1,fill.con[[i]]) %*% cof)
          else drop(cbind(1,xx[j,,drop=FALSE]) %*% xcof)

        if(last.iter) {
          coef[[i]] <- cof
          xcoef[i,c("intercept",nam[-i])] <- xcof
          if(trantab || (any(j) && catg &&
                         impcat %in% c("score","multinom"))) {
            xa <- x[!j, i]
            ya <- newy[!j]
            tab <- table(paste(as.character(xa),
                               as.character(ya),sep=';'))
            vals <- names(tab)
            uvals <- unPaste(vals, ';')
            names(tab) <- NULL
            Trantab[[i]] <- 
              list(x=as.numeric(uvals[[1]]), y=as.numeric(uvals[[2]]),
                   frequency=unname(tab))
            NULL
          }

          if(imputed & any(j)) {
            if(catg) {
              if(usefill>0)
                pred <- rep(fillin[i], sum(j))
              else {
                if(impcat == 'rpart') {
                  y <- as.factor(x[,i])
                  zdf <- list(xx=xx, y=y)
                  f <- rpart(y ~ xx,
                             control=rpart.control(minsplit=mincut), data=zdf)

                  ## won't work because rpart will not allow matrix x
                  pred <-
                    (t(apply(-predict(f,zdf)[j,,drop=FALSE],1,order)))[,1]
                  if(treeinfo) {
                    cat('\nProbabilities of Category Membership and Category Selected for',lab,'\n\n')
                    print(cbind(round(predict(f,zdf)[j,,drop=FALSE],3),
                                Mode=pred))
                  }

                  ## Gets level number of most probable category
                } else if(impcat=='score') {
                  ##Get category code with score closest to pred. score
                  ti <- Trantab[[i]]
                  if(n.impute==0) {
                    ##ww <- apply(outer(newy[j], ti$y,
                    ##function(x,y)abs(x-y)),1,order)[1,]
                    ww <- order(ti$y)[round(approx(sort(ti$y),
                                                   1:length(ti$y),
                                                   xout=newy[j], rule=2)$y)]

                    ## Thanks from Alan Zaslavsky <zaslavsk@hcp.med.harvard.edu>:
                    ## "The idea is to interpolate (after arranging in order) and then round the
                    ## index, since the fractional part of the index represents the relative 
                    ## distance from the two adjacent values."
                    
                    ##pred <- round(approx(ti$y, ti$x, xout=newy[j], rule=2)$y)
                    pred <- ti$x[ww]
                  } else {
                    sval <- predSamp(0*res, newy[j], rnam[j], c(-Inf,Inf),
                                     n.impute, boot.method)
                    ww <- order(ti$y)[round(approx(sort(ti$y),
                                                   1:length(ti$y),
                                                   xout=sval,
                                                   rule=2)$y)]
                    pred <- matrix(ti$x[ww], ncol=n.impute,
                                   dimnames=list(rnam[j],milab))
                    names(pred) <- NULL
                    if(lab==details.impcat)
                      Details.impcat <-
                        list(pred.trans.na=sval,imputed=pred,
                             pred.trans.nona=cbind(1,xx[!j,]) %*%	xcof,
                             obs=x[!j,i],trantab=ti)
                  }
                } else {
                  ## Multinomial logit
                  zdf <- list(y=as.factor(x[!j,i]),
                              xx=xx[!j,,drop=FALSE])
                  f <- multinom(y ~ xx, data=zdf,
                                trace=FALSE, maxit=200)
                  ncat <- length(levels(zdf$y))
                  ## bug in predict.multinom when predictor is a matrix
                  cf <- coef(f)
                  zdf <- cbind(1,xx[j,,drop=FALSE]) %*%
                    (if(is.matrix(cf)) t(cf) else as.matrix(cf))
                  pred <- exp(cbind(0,zdf))/
                    (1 + apply(exp(zdf),1,sum))
                  dimnames(pred)[[2]] <- as.character(1:ncat)
                  pred <-
                    if(n.impute==0) (t(apply(-pred,1,order)))[,1] 
                    else rMultinom(pred, n.impute)
                }
              }

              if(n.impute==0)
                names(pred) <- rnam[j]

              Imputed[[i]] <- pred
              NULL
            } else {
              if(n.impute == 0) {
                if(usefill > 0)
                  Im <- rep(fillin[i], sum(j))
                else
                  Im <- invertTabulated(x[!j,i], newy[!j], aty=newy[j],
                                        name=nam[i], inverse=inverse,
                                        tolInverse=tolInverse)

                names(Im) <- rnam[j]
                Imputed[[i]] <- Im
                NULL
              } else {
                
                sval <- predSamp(res, newy[j], rnam[j], c(-Inf,Inf),
                                 n.impute, boot.method)

                sval.orig <- matrix(invertTabulated(x[!j,i], newy[!j],
                                                    aty=sval,
                                                    name=nam[i],
                                                    inverse=inverse,
                                                    tolInverse=tolInverse),
                                    ncol=n.impute,
                                    dimnames=list(rnam[j],milab))

                names(sval.orig) <- NULL
                Imputed[[i]] <- sval.orig
                NULL
              }
            }
          }   ##end imputed
        }       ##end last.iter
      }         ##end non-asis

      if(curtail && any(j)) {
        r <- range(newy[!j])
        newy[j] <- pmax(pmin(newy[j],r[2]),r[1])
      }

      if(iter>1) {
        jj <- if(rhsImp=='mean')TRUE
              else TRUE

        dmax <- max(dmax, min(max(abs(xt[jj,i]-newy[jj]),na.rm=TRUE),
                              max(abs(-xt[jj,i]-newy[jj]),na.rm=TRUE))/scale[i])
        ##Allows for arbitrary flips (negation) of current transformation
      }

      if(rhsImp=='random')
        newy[j] <- newy[j] + sample(res, sum(j), replace=TRUE)
    
      if(last.iter) xtl[,i] <- newy
      else xt[,i] <- newy

      ##don't update working transformations
      ##during last iteration since recomputing x-coefficients
      ##on the basis of current transformations, which may flip rapidly
	
      if((pl & last.iter) | allpl) {
        xti <- if(last.iter) xtl[,i]
               else xt[,i]

        plot(x[,i], xti, xlab=lab,ylab=paste("Transformed",lab))
        title(sub=paste("R2=",format(round(r2,2)),sep=""),cex=.4,adj=0)
        if(any(j))
          title(sub=paste(sum(j),"missing"),cex=.4,adj=1)

        if(show.na && any(j)) {
          scat1d(xti[j], 4, ...)
          if(imputed && last.iter)
            scat1d(as.numeric(Imputed[[i]]), 3, ...)
        }
      }


      if(last.iter && imputed.actual!='none' && any(j)) {
        v1n <- nam[i]; v2n <- paste('Imputed',v1n)
        datad[[v1n]] <- x[!j,i]
        datad[[v2n]] <- Imputed[[i]]
        datad.ranges[[v1n]] <- datad.ranges[[v2n]] <-
          range(c(x[!j,i], Imputed[[i]]), na.rm=TRUE)
      }
    
      R2[i] <- r2; R2.adj[i] <- r2.adj
	
    }   #end i

    if(pr && iter>1)
      cat(format(round(dmax,3)),"")

    if(pr && (iter %% 10 == 0))
      cat("\n")

    niter <- iter
    if(last.iter)
      break

    last.iter <- (iter==(iter.max-1)) || (iter>1 && dmax<eps) ||
                 (rhsImp=='random' && iter==5)
  }        #end iter

  if(pr) cat("\n")

  if(iter.max>3 & niter==iter.max & dmax>=eps)
    stop(paste("no convergence in",iter.max,"iterations"))

  ## Use xtl instead of xt, otherwise transformed variables will not
  ## match ones from predict() or Function() since coefficients have
  ## been updated

  if(pr && rhsImp=='mean')
    cat("Convergence in",niter,"iterations\n")

  if(imputed.actual=='datadensity') {
    lab <- names(datad)
    datadensity.data.frame(datad, ranges=datad.ranges,
                           labels=ifelse((1:length(lab)) %% 2, lab,'Imputed'))
  } else if(imputed.actual !='none') {
    namdd <- names(datad)
    for(i in seq(1,length(datad),by=2)) {
      if(imputed.actual=='hist')
        histbackback(datad[i:(i+1)])
      else {
        v1 <- datad[[i]]; v2 <- datad[[i+1]]
        n1 <- namdd[i]; n2 <- namdd[i+1]
        if(imputed.actual=='ecdf' && is.numeric(datad[[i]]))
          Ecdf(c(v1,v2), xlab=n1,
               group=c(rep('actual',length(v1)),
                       rep('imputed',length(v2))))
        else {
          qqplot(v1, v2, xlab=n1, ylab=n2)
          abline(a=0, b=1, lty=2)
        }
      }
    }
  }

  names(R2) <- nam

  if(pr) {
    cat("R-squared achieved in predicting each variable:\n\n")
    print(round(R2, 3))
  }
  names(R2.adj) <- nam

  if(pr) {
    cat("\nAdjusted R-squared:\n\n")
    print(round(R2.adj, 3))
  }

  if(shrink) {
    names(shr) <- nam
    if(pr) {
      cat("\nShrinkage factors:\n\n")
      print(round(shr,3))
    }
  } else shr <- NULL

  names(parms) <- names(coef) <- nam
  r <- apply(xtl, 2, range)
  dimnames(r) <- list(c("low","high"), nam)

  if(imputed) {
    names(Imputed) <- nam
  } else Imputed <- NULL

  structure(list(call=call, formula=formula, niter=niter, imp.con=usefill>0,
                 n.impute=n.impute, residuals=Resid, rsq=R2, rsq.adj=R2.adj, 
                 shrinkage=shr,
                 inverse=inverse, tolInverse=tolInverse,
                 categorical=categorical, asis=asis, parms=parms, coef=coef,
                 xcoef=xcoef, fillin=fillin, scale=scale, ranges=r,
                 transformed=if(transformed)xtl,
                 trantab=if(trantab)Trantab, imputed=Imputed, na.action=nact,
                 rhsImp=rhsImp, details.impcat=Details.impcat),
            class='transcan')
}


summary.transcan <- function(object, long=FALSE, digits=6, ...)
{
  ## Check for old style object
  if(!is.list(object))
    object <- attributes(object)

  dput(object$call);
  cat("\n")
  if(length(nact <- object$na.action))
    naprint(nact)

  cat("Iterations:",object$niter,"\n\n")
  cat("R-squared achieved in predicting each variable:\n\n")
  print(round(object$rsq,3))
  cat("\nAdjusted R-squared:\n\n")
  print(round(object$rsq.adj,3))
  if(length(shr <- object$shrink))
    {
      cat("\nShrinkage factors:\n\n")
      print(round(shr,3))
    }

  cat("\nCoefficients of canonical variates for predicting each (row) variable\n\n")
  xcoef <- object$xcoef[,-1]
  g <- format(round(xcoef,2))
  g[is.na(xcoef)] <- ""
  print(g, quote=FALSE)

  imp <- object$imputed
  if(length(imp))
    {
      nimp <- TRUE
      for(nn in names(imp))
        {
          if(length(z <- imp[[nn]]))
            {
              if(nimp & !long)
                cat("\nSummary of imputed values\n\n");
              
              nimp <- FALSE
              if(long)
                {
                  cat("\nImputed values for",nn,"\n\n");
                  print(z)
                }

              if(nn %in% object$categorical) {
                print(describe(as.vector(z), nn))
              } else {
                print(describe(signif(as.vector(z), digits), nn))
              }
            }
        }	
    }
  if(object$imp.con) 
    cat("\nImputed values set to these constants:\n\n")
  else cat("\nStarting estimates for imputed values:\n\n")
  
  print(signif(object$fillin, digits))
  invisible()
}


print.transcan <- function(x, long=FALSE, ...) {
  trans <- x$transformed
  form  <- x$formula
  
  print(form, showEnv=FALSE); cat("\n")
  if(length(trans)) {
    if(long) print(unclass(x))
    else print.default(trans)
  }

  invisible()
}

impute.transcan <-
  function(x, var, imputation,
           name=as.character(substitute(var)),
           pos.in, data, list.out=FALSE,
           pr=TRUE, check=TRUE, ...)
{
  if(!missing(imputation) && length(imputation) > 1)
    stop('imputation must be a single number')
  
  ## Check for old style
  imp <- if(is.list(x)) x$imputed
         else attr(x, 'imputed')
  
  if(!length(imp)) { 
    if(missing(var) && missing(name)) 
      stop('imputed=TRUE was not specified to transcan')
    
    warning("imputed was not specified to transcan")
    return(if(! missing(var)) var)
  }

  if(missing(var) && missing(name)) {
    nams   <- names(imp)
    if(list.out) {
      outlist <- vector('list', length(nams))
      names(outlist) <- nams
    }
    if(missing(data)) {
      if(missing(pos.in))
        pos.in <- find(nams[1])[1]

      var1   <- get(nams[1], pos=pos.in)
    }
    else {
      if(any(ni <- nams %nin% names(data)))
        stop(paste('variable', paste(nams[ni],collapse=','),
                   'not in data'))
      var1 <- data[[nams[1]]]
    }
    
    namvar <- names(var1)
    if(! length(namvar) && ! missing(data))
      namvar <- row.names(data)
    
    if(check && length(namvar)==0)
      warning(paste('variable', nams[1],
                    'does not have a names() attribute\nand data does not have row.names. Assuming row names are integers.'))
    
    nimp <- integer(length(nams))
    names(nimp) <- nams
    
    for(nam in nams) {
      i <- imp[[nam]]
      if(!length(i)) {
        if(list.out) outlist[[nam]] <-
          if(missing(data)) get(nam, pos=pos.in) else data[[nam]]
        
        next
      }
      
      d <- dim(i)
      obsImputed <- if(length(d)) dimnames(i)[[1]] else names(i)
      ## i[,imputation] drops names if only one obs. imputed
      if(!missing(imputation)) {
        if(!length(d)) 
          stop('imputation can only be given when transcan used n.impute')
              
        if(imputation < 1 || imputation > d[2])
          stop(paste('imputation must be between 1 and',d[2]))

        i <- i[, imputation]
      }
      else if(length(d)) 
        stop('imputation must be specified when transcan used n.impute')

      v <- if(missing(data)) get(nam, pos=pos.in)
           else data[[nam]]
      if(is.character(v)) v <- as.factor(v)   #####

      ## Below was names(i) instead of match(...)
      if(length(namvar)) {
        sub <- match(obsImputed, namvar, nomatch=0)
        i <- i[sub > 0]
        sub <- sub[sub > 0]
      }
      else {
        if(! all.is.numeric(obsImputed))
          stop(paste('names attribute of ',nam,
                     ' is not all numeric\n',
                     'and original observations did not have names',sep=''))
        
        sub <- as.integer(obsImputed)
      }
      if(check)
        if((missing(imputation) || imputation == 1) &&
           ! all(is.na(v[sub])))
          stop(paste('variable',nam,
                     'does not have same missing values as were present when transcan was run'))
      v[sub] <- if(is.factor(v)) levels(v)[as.integer(i)]
                else
                  if(is.logical(v)) i == 1
                else
                  i
      ## Note: if v was empty before, new v would have arbitrary length
      ## Avoid problem by requiring all variables to be in data
      attr(v,'imputed') <- sub
      attr(v,'class') <- c('impute', attr(v,'class'))
      
      nimp[nam] <- length(i)
      if(list.out)
        outlist[[nam]] <- v
    }
    
    if(pr) {
      cat('\n\nImputed missing values with the following frequencies\n',
          'and stored them in variables with their original names:\n\n')
      print(nimp[nimp > 0])
    }
    
    if(list.out) {
      z <- sapply(outlist, length)
      if(diff(range(z)) > 0) {
        cat('\n\nLengths of variable vectors:\n\n')
        print(z)
        stop('inconsistant naming of observations led to differing length vectors')
      }
      return(outlist)
    }
    
    return(invisible(nimp))
  }
  
  impval <- imp[[name]]
  if(name %nin% names(imp))
    warning(paste('Variable',name,
                  'was not specified to transcan or had no NAs'))
  
  if(!length(impval)) return(var)
  
  d <- dim(impval)
  
  if(!missing(imputation)) {
    if(!length(d)) 
      stop('imputation can only be given when transcan used n.impute')
    
    if(imputation < 1 || imputation > d[2])
      stop(paste('imputation must be between 1 and',d[2]))
    
    impval <- impval[,imputation]
  }
  else if(length(d)) 
    stop('imputation must be specified when transcan used n.impute')
  
  namvar <- names(var)
  
  if(!length(namvar)) {
    if(missing(data))
      stop(paste('variable',name,
                 'does not have a names() attribute\nand data= was not given.\nAssuming identifiers stored by transcan are integer subscripts'))
    else
      namvar <- row.names(data)
    
    if(!length(namvar))
      stop(paste('variable',name,
                 'does not have a names() attribute\nand data has no row.names'))
  }
  
  if(length(namvar)) {
    sub <- match(names(impval), namvar, nomatch=0)
    impval <- impval[sub > 0]
    sub <- sub[sub > 0]
  }
  else {
    if(!all.is.numeric(names(impval)))
      stop(paste('names attribute of ',name,
                 ' is not all numeric\n',
                 'and original observations did not have names',sep=''))
    
    sub <- as.integer(names(impval))
  }
  
  ##Now take into account fact that transcan may have been
  ##run on a superset of current data frame
  
  m <- length(sub)
  if(check)
    if(missing(imputation) || imputation==1)
      if(m!=sum(is.na(var)))
        warning("number of NAs in var != number of imputed values from transcan.")
  
  if(m == 0)
    return(var)
  var[sub] <- if(is.factor(var)) levels(var)[as.integer(impval)]
              else
                if(is.logical(var)) impval == 1
              else
                impval
  
  attr(var, 'imputed') <- sub
  attr(var, 'class') <- c("impute", attr(var,'class'))
  var
}

"[.transcan" <- function(x, rows=1:d[1], cols=1:d[2], ..., drop=TRUE)
{
  ## Check for old style object
  if(is.list(x))
    {
      if(length(x$imputed) && sum(sapply(x$imputed,length)))
        {
          d <- dim(x$transformed)
          original.rownames <- dimnames(x$transformed)[[1]]
          subset.rownames   <- original.rownames[rows]
          for(v in names(x$imputed))
            {
              z <- x$imputed[[v]]
              if(length(z))
                {
                  use <- names(z) %in% subset.rownames
                  x$imputed[[v]] <- z[use]
                }
            }
        }
      x$transformed <- x$transformed[rows,cols, drop=drop]
      return(x)
    }

  ats <- attributes(x)
  ats$dimnames <- ats$dim <- ats$names <- NULL
  attr(x, 'class') <- NULL
  y <- x[..., drop = drop]
  attributes(y) <- c(attributes(y), ats)
  if(!length(dim(y)))
    {
      aty <- attributes(y)
      aty$call <- aty$iter <- aty$rsq <- aty$parms <- aty$coef <- 
        aty$xcoef <- aty$rsq.adj <- aty$shrink <-
          aty$fillin <- aty$imputed <- aty$class <- aty$ranges <- 
            aty$imp.con <- aty$scale <- aty$categorical <- aty$asis <-
              aty$trantab <- NULL
      attributes(y) <- aty
      if(is.character(z <- list(...)[[1]]))
        attr(y,"label") <- paste("Transformed",z)
      ##May someday have to use label(y) <- for this ?
    }
  
  y
}


predict.transcan <- function(object, newdata=NULL, iter.max=50, eps=.01, 
                             curtail=TRUE, type=c("transformed","original"),
                             inverse, tolInverse, check=FALSE, ...)
{
  type <- match.arg(type)
  
  if(!is.list(object)) object <- attributes(object)
  parms  <- object$parms
  coef   <- object$coef
  xcoef  <- object$xcoef
  fillin <- object$fillin
  ranges <- object$ranges
  scale  <- object$scale
  imp.con<- object$imp.con
  rhsImp <- object$rhsImp
  trantab<- object$trantab
  categorical <- object$categorical
  formula <- object$formula

  inverse <- if(missing(inverse)) object$inverse
  
  if(!length(inverse)) inverse <- 'linearInterp'
  tolInverse <- if(missing(tolInverse)) object$tolInverse
  
  if(!length(tolInverse)) tolInverse <- 0.05

  if(type=="original" & !length(trantab))
    stop('type="trantab" and trantab=TRUE not specified to transcan')

  if(length(formula))
    {
      oldop <- options(na.action="na.retain")
      y <- model.frame(formula, data=newdata)

      options(oldop)
      d <- dim(y)
      p <- d[2]
      newdata <- matrix(NA, nrow=d[1], ncol=p,
                        dimnames=list(attr(y,"row.names"), names(y)))
      for(i in 1:p)
        {
          w <- y[[i]]
          if(is.character(w))
            {
              warning("character predictor present.  Depending on levels being same as in original fit,\nthat all levels are present in the data, and that levels were in alphabetical order")
              w <- factor(w)
            }

          newdata[,i] <- unclass(w)
        }
    }
  else
    {
      if(!length(newdata))
        stop("newdata must be given (unless formula was given to transcan)")

      p <- ncol(newdata)
    }
  if(!is.matrix(newdata))
    {
      if(!length(names(newdata)))
        names(newdata) <- dimnames(object)[[2]]
      
      newdata <- t(as.matrix(newdata))
    }

  if(imp.con || !any(is.na(newdata)))
    iter.max <- 1  

  ##only 1 iteration needed if no NAs   (imp.con)
  xt <- newdata
  nam <- dimnames(ranges)[[2]]
  if(ncol(ranges)!=p)
    stop("wrong number of columns in newdata")

  if(!length(dimnames(xt)[[2]]))
    dimnames(xt) <- list(dimnames(xt)[[1]],nam)
  else if(check && any(dimnames(newdata)[[2]]!=nam))
    warning("column names in newdata do not match column names in object")

  if(length(dimnames(xt)[[1]])==0)
    dimnames(xt) <- list(as.character(1:nrow(xt)),
                         dimnames(xt)[[2]])
  
  for(iter in 1:iter.max)
    {
      dmax <- 0
      for(i in 1:p)
        {
          lab <- nam[i]
          j <- is.na(newdata[,i])
          prm <- parms[[lab]]
          if(length(prm)==0)
            {
              newy <- newdata[,i]
              if(any(j))newy[j] <- if(iter==1) fillin[i] else
              drop(cbind(1,xt[j,-i,drop=FALSE]) %*% xcoef[i,-i-1])
            }
          else
            {
              if(is.matrix(prm))
                {
                  lev <- attr(prm, "codes")
                  consec.lev <- match(newdata[,i], lev)  #may give NAs - OK for next line
                  R <- prm[consec.lev,, drop=FALSE]
                  if(iter==1 && any(match(newdata[!j,i], lev, 0)==0))
                    stop("codes for categorical variable not in original list")
                } else R <- rcspline.eval(newdata[,i], prm, inclx=TRUE)
              
              newy <- drop(cbind(1,R) %*% coef[[i]])
              if(any(j))
                newy[j] <-
                  if(iter==1) 0
                  else
                    drop(cbind(1, xt[j,-i,drop=FALSE]) %*%xcoef[i, -i-1])
            }

          if(curtail) newy <- pmax(pmin(newy,ranges[2,i]),ranges[1,i])
          if(iter>1) dmax <- max(dmax, min(max(abs(xt[,i]-newy),na.rm=TRUE),
                                           max(abs(-xt[,i]-newy),na.rm=TRUE))/
                                 scale[i])
          xt[,i] <- newy
        }	#end i

      niter <- iter
      if(niter>1 && dmax<eps)
        break

      if(rhsImp=='random' && niter>4)
        break
    }	#end iter

  if(rhsImp=='mean')
    {
      if(iter.max>3 & niter==iter.max)
        stop(paste("no convergence in",iter.max,"iterations"))
      cat("Convergence in",niter,"iterations\n")
    }
  
  if(type=="transformed") return(xt)

  for(i in 1:p)
    {
      ft <- trantab[[i]]
      j <- is.na(newdata[,i])
      if(any(j))
        {
          newdata[j,i] <- if(imp.con) fillin[i]
          else
            {
              ww <- invertTabulated(ft, aty=xt[j,i], name=nam[i],
                                    inverse=inverse, tolInverse=tolInverse)
              if(nam[i] %in% categorical)
                ww <- round(ww)
              
              ww
            }
        }
    }

  newdata
}


Function <- function(object, ...) UseMethod("Function")


Function.transcan <- function(object, prefix=".", suffix="", pos=-1, ...)
{
  at <- if(is.list(object)) object
        else attributes(object)

  Nam <- names(at$coef)
  p <- length(Nam)
  categorical <- at$categorical
  asis        <- at$asis
  coef        <- at$coef
  parms       <- at$parms
  fnames      <- character(p)

  for(i in 1:p)
    {
      nam <- Nam[i]
      cof <- coef[[nam]]
      if(nam %in% asis)
        f <- function(x) x
      else if(nam %in% categorical)
        {
          codes <- attr(parms[[nam]], "codes")
          g <- "{x <- unclass(x);"
          cof[-1] <- cof[-1] + cof[1]  #convert from ref cell to cell means model
          for(j in 1:length(codes))
            {
              if(j>1 && cof[j]>0) g <- paste(g,"+")
              g <- paste(g, format(cof[j]),
                         "*(x==",format(codes[j]),")",sep="")
            }

          g <- paste(g, "}", sep="")
          f <- function(x) NULL
          f[[2]] <- parse(text=g)[[1]]
        }
      else f <- attr(rcspline.restate(parms[[nam]], cof), "function")
 
      fun.name <- paste(prefix,nam,suffix,sep="")
      cat("Function for transforming",nam,"stored as",fun.name,"\n")
      assign(fun.name, f, pos=pos)
      fnames[i] <- fun.name
    }
  
  invisible(fnames)
}


na.retain <- function(mf) mf


plot.transcan <- function(x, ...)
{
  ## check for old style object
  if(!is.list(x)) x <- attributes(x)

  trantab <- x$trantab
  imputed <- x$imputed
  n.impute <- x$n.impute
  if(length(trantab)==0)
    stop('you did not specify trantab=TRUE to transcan()')

  p   <- length(trantab)
  nam <- names(trantab)
  for(w in nam) {
    z <- trantab[[w]]
    plot(z, xlab=w, ylab=paste('Transformed',w))
    title(sub=paste('R2=',format(round(x$rsq[w],2)),sep=''),cex=.4,adj=0)
    if(length(imputed))
      {
        m <- imputed[[w]]
        if(L <- length(m))
          {
            title(sub=paste(L / n.impute, 'missing'),cex=.4,adj=1)
            m.trans <- approx(z, xout=m, rule=2)$y
            scat1d(m, 3, ...)
            scat1d(m.trans, 4, ...)
          }
      }
  }
}

ggplot.transcan <- function(data, mapping, scale=FALSE, ..., environment)
{
  x <- data
  trantab  <- x$trantab
  imputed  <- x$imputed
  n.impute <- max(1, x$n.impute)
  rsq      <- x$rsq
  if(length(trantab) == 0)
    stop('you did not specify trantab=TRUE to transcan()')

  p   <- length(trantab)
  nam <- names(trantab)
  data <- adata <- NULL
  for(w in nam) {
    z <- trantab[[w]]
    x <- z[[1]]
    y <- z[[2]]
    if(scale) {
      r <- range(y)
      y <- (y - r[1]) / (r[2] - r[1])
      z <- list(x=x, y=y)
    }
    data  <- rbind(data, data.frame(type='transform', X=w, x=x, y=y))
    loc   <- largest.empty(x, y, xlim=range(x), ylim=range(y))
    lab   <- paste('R^2==', round(rsq[w], 2), sep='')
    if(length(imputed)) {
      m <- as.vector(imputed[[w]])
      if(L <- length(m)) {
        lab <- paste('paste(', lab, ',"   ',
                     L / n.impute, ' missing")', sep='')
        m.trans <- approx(z, xout=m, rule=2)$y
        data <- rbind(data,
                      data.frame(type='imputed', X=w, x=m, y=m.trans))
      }
      adata <- rbind(adata, data.frame(X=w, x=loc$x, y=loc$y,
                                       lab=lab, type='transform'))
    }
  }

  # scale*manual calls are from David Norris
  # The implicit (alphabetical) order of 'imputed' and 'transform'
  # reverses the intended symbol & color assignments. But naming
  # the vectors corrects this.

  g <- ggplot(data, aes(x=x, y=y, color=type, shape=type, size=type)) + geom_point() +
       facet_wrap(~ X, scales=if(scale) 'free_x' else 'free', ...) +
       xlab(NULL) + ylab('Transformed') +
       scale_color_manual(values = c(transform="#00000059", imputed="#FF000059")) +
       scale_shape_manual(values = c(transform=1, imputed=3)) +
       scale_size_manual(values = c(transform=1.3, imputed=2.25)) +
    theme(legend.position='none')
  if(length(adata))
    g <- g + geom_text(data=adata, aes(label=lab), parse=TRUE, size=1.65, col='black')
  g
}



##The following needed if Design is not in effect, to make anova work

vcov.default <- function(object, regcoef.only=FALSE, ...)
{
  vc <- object$Varcov
  if(length(vc))
    {
      if(regcoef.only) return(object$var)
      else return(vc(object, which='var'))
    }

  cov <- object$var
  if(!length(cov))
    stop("object does not have variance-covariance matrix")
  
  if(regcoef.only)
    {
      p <- length(object$coef)
      cov <- cov[1:p, 1:p, drop=FALSE]
    }

  cov
}


if(FALSE) Varcov.lm <- function(object, ...)
{
  cof <- object$coefficients
  Qr <- object$qr
  cov <- chol2inv(Qr$qr)
  
  cov <- sum(object$residuals^2)*cov/object$df.residual
  nm  <- names(cof)
  dimnames(cov) <- list(nm, nm)
  cov
}


if(FALSE) Varcov.glm <- function(object, ...)
{
  if(length(object$var))
    return(object$var)  ## for glmD

  s <- summary.glm(object)
  s$cov.unscaled * s$dispersion
}


#Varcov.multinom <- function(object, ...) vcov(object)

invertTabulated <- function(x, y, freq=rep(1,length(x)),
                            aty, name='value',
                            inverse=c('linearInterp','sample'),
                            tolInverse=0.05, rule=2)
{
  inverse <- match.arg(inverse)
  if(is.list(x))
    {
      freq <- x[[3]]
      y <- x[[2]]
      x <- x[[1]]
  }
  
  if(inverse=='linearInterp')
    return(approx(y, x, xout=aty, rule=rule, ties=mean)$y)

  del <- diff(range(y, na.rm=TRUE))
  m <- length(aty)
  yinv <- double(m)
  
  cant <- double(0)

  for(i in 1:m)
    {
      a <- aty[i]
      s <- abs(y-a) < (tolInverse * del)
      nclose <- sum(s)
      if(nclose < 2)
        {
          if(nclose==0)
            cant <- c(cant, a)
          
          xest <- approx(y, x, xout=a, rule=rule)$y
          ## If a outside range of y, approx(rule=2) will return min or max
          ## x.  There may be many x's with y values near this extreme x.
          ## Take a random draw from them.
          a <- approx(x, y, xout=xest, rule=rule)$y
          s <- abs(y - a) < (tolInverse * del)
          nclose <- sum(s)
          if(nclose > 1)
            {
              maxdist <- max((y[s] - a)^2)
              wt <- if(maxdist==0) freq[s]
              else (1 - ((y[s] - a)^2) / maxdist) * freq[s]

              if(all(wt==0)) wt <- freq[s]  # y[s] all the same

        if(any(wt==0)) wt[wt==0] <- min(wt[wt>0])/2

              xest <- x[s][sample(nclose, 1, replace=FALSE,  prob=wt/sum(wt))]
            }
    }
      else
        {
          maxdist <- max((y[s] - a)^2)
          wt <- if(maxdist==0) freq[s]
          else (1 - ((y[s] - a)^2) / maxdist) * freq[s]

          if(all(wt==0))
            wt <- freq[s]  # y[s] all the same

          if(any(wt==0))
            wt[wt==0] <- min(wt[wt>0])/2
          
          xest <- x[s][sample(nclose, 1, replace=FALSE,  prob=wt/sum(wt))]
          ## sample(x[s],...) fails if x[s] is scalar; thanks: Bill Dunlap
        }
      yinv[i] <- xest
    }
  
  if(length(cant))
    warning(paste('No actual ',name, ' has y value within ',
                  format(tolInverse),
                  '* range(y) (',format(del),
                  ') of the following y values:',
                  paste(format(sort(unique(cant))),collapse=' '),
                  '.\nConsider increasing tolInverse. ',
                  'Used linear interpolation instead.',sep=''))
  
  yinv
}


## Trick taken from MICE impute.polyreg
rMultinom <- function(probs, m)
{
  d <- dim(probs)
  n <- d[1]
  k <- d[2]
  lev <- dimnames(probs)[[2]]
  if(!length(lev))
    lev <- 1:k

  ran <- matrix(lev[1], ncol=m, nrow=n)
  z <- apply(probs, 1, sum)
  if(any(abs(z-1) > .00001))
     stop('error in multinom: probabilities do not sum to 1')

  U <- apply(probs, 1, cumsum)
  for(i in 1:m)
    {
      un <- rep(runif(n), rep(k,n))
      ran[,i] <- lev[1 + apply(un > U, 2, sum)]
    }
  
  ran
}
utils::globalVariables('type')
