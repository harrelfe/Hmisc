if(!.R.) {
  mulbar.chart<-function(z, x, y, fun = mean, marginals=TRUE, subset, prt=TRUE,
                         zlab = label(z), xlab=label(x), ylab=if(!missing(y))label(y), 
                         varwidth=TRUE, overall, ...)
  {
    xl<-xlab
    yl<-ylab
    zl<-zlab
    if(!missing(subset)) {
      x <- x[subset]
      if(!missing(y)) y <- y[subset]
      z <- z[subset]
    }
    
    x<-as.category(x)
    count <- function(ww) sum(!is.na(ww))
    
    oldpar <- par(mar=c(7,4,3,2)+.1)
    if(marginals)
      ntext <- "n="
    else ntext <- "Maximum n="
    
    if(missing(y)){
      tabln <- tapply(z, list(x), count)
      tabl <- tapply(z, list(x), fun)
      nmin <- min(tabln)
      nmax <- max(tabln)
      cx <- category(row(tabl), label=levels(x))
      if(marginals) {
        tabln <- c(tabln, 1)
        tabl  <- c(tabl,
                   if(missing(overall)) fun(z)
                   else overall)
        
        levels(cx) <- c(levels(cx),"All")
      }
      
      names(tabl) <- levels(cx)
      names(tabln) <- levels(cx)
      if(varwidth)
        barplot(tabl, tabln, names=levels(cx), xlab=xl, main=zl)
      else barplot(tabl, names=levels(cx), xlab=xl, main=zl)
      
      mtext(paste("n=",count(z)," (",nmin,"-",nmax,")",sep=""),
            side=1,line=5,adj=0)
      
      if(varwidth)
        mtext("Width proportional to sample size",side=1,line=6,adj=0)
    } else {
      y<-as.category(y)
      tabl <- tapply(z, list(y,x), fun)
      tabln <- tapply(z, list(y,x), count)
      nmin <- min(tabln)
      cy <- category(row(tabl), label = levels(y))
      cx <- category(col(tabl), label = levels(x))
      if(marginals) {
        tabl <- cbind(tabl, tapply(z, list(y), fun))
        tabl <- rbind(tabl, c(tapply(z, list(x), fun), 
                              if(missing(overall)) fun(z)
                              else overall))
        
        tabln <- cbind(tabln, tapply(z, list(y), count))
        tabln <- rbind(tabln,c(tapply(z, list(x), count), 1))
        levels(cx) <- c(levels(cx),"All")
        levels(cy) <- c(levels(cy),"All")	}
      dimnames(tabl) <- list(levels(cy),levels(cx))
      dimnames(tabln) <- list(levels(cy),levels(cx))
      if(varwidth)
	mulbar(tabln, tabl, collab=levels(cx), rowlab = levels(cy), 
               main=zl, ylab=yl, ...)
      else
	mulbar(1+0*tabl, tabl, collab=levels(cx), rowlab=levels(cy), main=zl,
               ylab=yl, ...)
      
      mtext(xl,side=1,line=3)
      if(varwidth)
	mtext("Width proportional to sample size",side=1,line=6,adj=0)
      
      mtext(paste("n=",count(z)," (",nmin,"-",max(tabln),")",
                  "   Height=",signif(as.single(min(tabl)),5),
                  "-",signif(as.single(max(tabl)),5),sep=""),
            side=1,line=5,adj=0)
    }
    
    par(oldpar)
    if(prt) {
      print(zl,quote=FALSE)
      print(tabl,digits=4)
      print("------- n -------",quote=FALSE)
      print(tabln)
    }
    
    invisible()
  }
  
  NULL
}
