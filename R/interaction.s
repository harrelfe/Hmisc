#Modification of S-supplied interaction function to keep levels in
#correct order if drop=T. Also, sep argument added.

interaction <- function(..., drop = FALSE, sep=".", left=FALSE)
{
	g <- if(left) function(x) format(x) else function(x) x
	allf <- list(...)
	if(length(allf) == 1 && is.list(ttt <- oldUnclass(allf[[1]])))
		allf <- ttt
	nterms <- length(allf)
	what <- allf[[nterms]]

	if(!length(levels(what)))
		what <- factor(what)
	levs <- oldUnclass(what) - 1
	labs <- g(levels(what))
    rev.allf <- rev(allf[ - nterms])
    for(k in seq(along = rev.allf)) {
      what <- as.factor(rev.allf[[k]])
      wlab <- g(levels(what))
      i <- oldUnclass(what) - 1
      levs <- levs * length(wlab) + i
      labs <- as.vector(outer(wlab, labs, paste, sep = sep))
	}
	levs <- levs + 1
	if(drop) {
      ulevs <- sort(unique(levs[!is.na(levs)]))   #sort() added FH
      levs <- match(levs, ulevs)
      labs <- labs[ulevs]
    }
	levels(levs) <- labs
    storage.mode(levs) <- 'integer'
    oldClass(levs) <- 'factor'
    levs
}
