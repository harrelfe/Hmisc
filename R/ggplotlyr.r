##' Render `plotly` Graphic from a `ggplot2` Object
##'
##' Uses `plotly::ggplotly()` to render a `plotly` graphic with a specified tooltip attribute, removing extraneous text that `ggplotly` puts in hover text when `tooltip='label'`
##' @title ggplotlyr
##' @param ggobject an object produced by `ggplot`
##' @param tooltip attribute specified to `ggplot` to hold hover text
##' @param remove extraneous text to remove from hover text.  Default is set to assume `tooltip='label'` and assumed the user specified `aes(..., label=txt)`.  If you instead specified `aes(..., label=myvar)` use `remove='myvar: '`.
##' @param ... other arguments passed to `ggplotly`
##' @return a `plotly` object
##' @author Frank Harrell
##' @export
##' @md
ggplotlyr <- function(ggobject, tooltip='label', remove='txt: ', ...) {
  
  if (!requireNamespace("plotly"))
    stop("This function requires the 'plotly' package.")
  
# Get around a bug in tooltip construction with ggplotly
# See https://stackoverflow.com/questions/66316337
  g <- plotly::ggplotly(ggobject, tooltip=tooltip, ...)
	if(! length(remove) || remove == '') return(g)
  d <- g$x$data
  for(i in 1 : length(d)) {
    w <- d[[i]]$text
     if(length(w)) d[[i]]$text <- gsub(remove, '', w)
	  }
   g$x$data <- d
   g
	}
