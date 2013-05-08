##This has code from Bill Dunlap's "set.work" function
store <- function(object, name=as.character(substitute(object)), 
                  where=if(under.unix || .SV4.)".Data"
                        else "_Data")
  stop('function not available for R')

stores <- function(...) stop('function not available for R')

storeTemp <- function(object, name=deparse(substitute(object)))
{
  pos <- match('.GlobalTemp', search())
  if(is.na(pos)) {
    attach(NULL,name='.GlobalTemp')
    pos <- match('.GlobalTemp', search())
  }
  assign(name, object, pos)
  invisible()
}
