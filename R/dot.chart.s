dot.chart<-function(z, major, minor, fun = mean, subset, pch=18, mkh=.035,
	cex=.5, xlab = label(z), prt=TRUE, ...)
{
count <- function(ww) sum(!is.na(ww))

xl<-xlab
#Note: dotchart does not pass the following parameters to points and mtext
oldpar<-par(mkh=mkh, cex=cex)
if(!missing(subset))	{
	z <- z[subset]
	major <- major[subset]
	if(!missing(minor))minor <- minor[subset]	}
major<-as.category(major)
if(missing(minor)){
	tabl <- tapply(z, list(major), fun)
	tabln <- tapply(z, list(major), count)
	names(tabl) <- levels(major)
	names(tabln) <- levels(major)
	cmajor <- category(row(tabl), label=levels(major))
	dotchart(tabl, labels=levels(cmajor)[cmajor], xlab="", pch=pch,
	 ...)
	}
else {
	minor<-as.category(minor)
	tabl <- tapply(z, list(major, minor), fun)
	tabln <- tapply(z, list(major, minor), count)
	dimnames(tabl) <- list(levels(major),levels(minor))
	dimnames(tabln) <- list(levels(major),levels(minor))
	cminor <- category(col(tabl), label = levels(minor))
	cmajor <- category(row(tabl), label = levels(major))
	dotchart(tabl, labels = levels(cminor)[cminor], groups = cmajor, 
		xlab = "", pch=pch,  ...)
	}
par(oldpar)
if(xl!="" & xl!=" ") title(xlab=xl)

if(prt)	{
	print(xl,quote=FALSE)
	print(tabl,digits=4)
	print("------- n -------",quote=FALSE)
	print(tabln)	}


invisible()
}
