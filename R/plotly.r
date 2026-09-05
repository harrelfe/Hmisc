#' Save a Plotly Graphic to PNG
#'
#' `plotlySave` saves a plotly graphic with name `foo.png` where `foo` is
#' the name of the current chunk. You must have a free `plotly` account
#' from `plot.ly` to use this function, and you must have run
#' `Sys.setenv(plotly_username="your_plotly_username")` and
#' `Sys.setenv(plotly_api_key="your_api_key")`. The API key can be found
#' in one's profile settings. See
#' <http://stackoverflow.com/questions/33959635/exporting-png-files-from-plotly-in-r>.
#'
#' @param x a plotly object, or a named list of plotly objects (excluding
#'   objects of class `'plotly_hash'`)
#' @param ... additional arguments passed to `plotly::plotly_IMAGE`
#'
#' @return called for its side effect of writing one or more PNG files;
#'   returns `invisible(NULL)`
#'
#' @details
#' The output path and base file name are taken from
#' `knitr::opts_chunk$get('fig.path')` and
#' `knitr::opts_current$get("label")` respectively. If `x` is a
#' named list (and not a `plotly_hash` object), one PNG file is written
#' per list element, with the element name appended to the chunk name.
#' Otherwise a single PNG file is written using just the chunk name.
#'
#' @md
#' @export
plotlySave <- function(x, ...) {

  if (!requireNamespace("plotly"))
    stop("This function requires the 'plotly' package.")

  chunkname <- knitr::opts_current$get("label")
  path      <- knitr::opts_chunk$get('fig.path')
  if(is.list(x) & ! inherits(x, 'plotly_hash')) {
    for(w in names(x)) {
      file <- paste0(path, chunkname, '-', w, '.png')
      plotly::plotly_IMAGE(x[[w]], format='png', out_file=file, ...)
    }
  }
  else {
    file <- paste0(path, chunkname, '.png')
    plotly::plotly_IMAGE(x, format='png', out_file=file, ...)
    }
  invisible()
}

## plotlyParm is a list of functions useful for specifying parameters to plotly graphics.
plotlyParm = list(
  ## Needed height in pixels for a plotly dot chart given the number of
  ## rows in the chart
  heightDotchart = function(rows, per=25, low=200, high=800)
    min(high, max(low, per * rows)),

  ## Given a vector of row labels that appear to the left on a dot chart,
  ## compute the needed chart height taking label line breaks into account
  ## Since plotly devotes the same vertical space to each category,
  ## just need to find the maximum number of breaks present
  heightDotchartb = function(x, per=40,
      low=c(200, 200, 250, 300, 375)[min(nx, 5)],
      high=1700) {
    x  <- if(is.factor(x)) levels(x) else sort(as.character(x))
    nx <- length(x)
    m <- sapply(strsplit(x, '<br>'), length)
    # If no two categories in a row are at the max # lines,
    # reduce max by 1
    mx   <- max(m)
    lm   <- length(m)
    mlag <- if(lm == 1) 0 else c(0, m[1:(lm - 1)])
    if(! any(m == mx & mlag == mx)) mx <- mx - 1
    z <- 1 + (if(mx > 1) 0.5 * (mx - 1) else 0)
    min(high, max(low, per * length(x) * z))
  },

  ## Colors for unordered categories
  colUnorder = function(n=5, col=colorspace::rainbow_hcl) {
    if(! is.function(col)) rep(col, length.out=n)
    else col(n)
  },

  ## Colors for ordered levels
  colOrdered = function(n=5, col=viridisLite::viridis) {
    if(! is.function(col)) rep(col, length.out=n)
    else col(n)
  },

  ## Margin to leave enough room for long labels on left or right as
  ## in dotcharts
  lrmargin = function(x, wmax=190, mult=7) {
    if(is.character(x)) x <- max(nchar(x))
    min(wmax, max(70, x * mult))
    }

  )

#' Generic Method for Making Plotly Graphics
#'
#' `plotp` is a generic function that dispatches to `plotp`
#' methods to make `plotly` graphics.
#'
#' @param data an object having a `plotp` method
#' @param ... additional arguments passed to the specific `plotp`
#'   method
#'
#' @return typically a plotly object, as produced by the dispatched method
#'
#' @md
#' @export
plotp <- function(data, ...) UseMethod("plotp")
