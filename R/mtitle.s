## Thanks for Rick Becker for suggestions
mtitle <-
  function(main,ll,lc,
           lr=format(Sys.time(),'%d%b%y'),
           cex.m=1.75, cex.l=.5, ...)
{
  out <- any(par()$oma!=0)
  g <-
    if(out) function(...) mtext(..., outer=TRUE)
    else  function(z, adj, cex, side, ...) 
      if(missing(side))
        title(z, adj=adj, cex=cex)
      else
	title(sub=z, adj=adj, cex=cex)
  
  if(!missing(main))
    g(main,cex=cex.m,adj=.5)
  
  if(!missing(lc))
    g(lc,side=1,adj=.5,cex=cex.l,...)
  
  if(!missing(ll))
    g(ll,side=1,adj=0,cex=cex.l,...)
  
  if(lr!="")
    g(lr,side=1,adj=1,cex=cex.l,...)
  
  invisible()
}
