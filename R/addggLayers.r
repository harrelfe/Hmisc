##' Add Spike Histograms and Extended Box Plots to `ggplot`
##'
##' For an example see [this](https://hbiostat.org/rflow/analysis.html#fig-table1).  Note that it was not possible to just create the layers needed to be added, as creating these particular layers in isolation resulted in a `ggplot` error.
##' @title addggLayers
##' @param g a `ggplot` object
##' @param data data frame/table containing raw data
##' @param type specifies either extended box plot or spike histogram.  Both are horizontal so are showing the distribution of the x-axis variable.
##' @param ylim y-axis limits to use for scaling the height of the added plots, if you don't want to use the limits that `ggplot` has stored
##' @param by the name of a variable in `data` used to stratify raw data
##' @param value name of x-variable
##' @param frac fraction of y-axis range to devote to vertical aspect of the added plot
##' @param mult fudge factor for scaling y aspect
##' @param facet optional faceting variable
##' @param pos position for added plot
##' @param showN sete to `FALSE` to not show sample sizes
##' @return the original `ggplot` object with more layers added
##' @seealso `spikecomp()`
##' @author Frank Harrell
##' @md
addggLayers <- function(g, data,
                        type=c('ebp', 'spike'),
                        ylim=layer_scales(g)$y$get_limits(),
                        by='variable', value='value',
                        frac=0.065, mult=1., facet=NULL,
                        pos=c('bottom', 'top'), showN=TRUE) {
  type <- match.arg(type)
  pos  <- match.arg(pos)
  d    <- data.table::copy(data)
  data.table::setDT(d)
  if(by %nin% names(data)) {
    by <- '.by.'
    d[, .by. := rep('', .N)]
    }
  data.table::setnames(d, c(by, value), c('.by.', '.value.'))

  scomp <- function(x)
    list(segments = spikecomp(x, normalize=TRUE, tresult='segments'))
  comp  <- switch(type,
                  ebp   = ebpcomp,
                  spike = scomp)

  vars <- d[, unique(.by.)]
  r    <- list()
  for(v in vars) {
    x <- d[.by. == v, .value.]
    r[[v]] <- comp(x)
  }
  R <- list()
  for(n in names(r[[1]]))
    R[[n]] <- data.table::rbindlist(lapply(r, function(z) z[[n]]), idcol=by)

  ## Transform y from ebpcomp which has y in [-1, 1]
  ## -->  ylim[1] + (y + 1.) * diff(ylim) * frac / 2.
  b <- mult * diff(ylim) * frac / 2.

  switch(type,
         ebp   = switch(pos,
                        bottom = {a <- ylim[1] + b},
                        top    = {a <- ylim[2] - b} ),
         spike = switch(pos,
                        bottom = {a <- ylim[1]},
                        top    = {a <- ylim[2]; b <- -b} )          )

  for(geo in names(R)) {
    dat <- R[[geo]]
    if(length(facet)) dat[, names(facet) := facet]
    g <- g +
      switch(geo,
             lines    = geom_path(aes(x=x, y=a + b * y), alpha=I(0.6),
                                  data=dat),
             segments = geom_segment(aes(x=x, xend=x,
                                         y=a + b * y1, yend=a + b * y2),
                                     data=dat),
             points   = geom_point(aes(x=x, y=a + b * y),
                                       col=I('blue'), size=I(0.8), data=dat),
             points2  = geom_point(aes(x=x, y=a + b * y),
                                       size=I(0.2), alpha=I(0.4),
                                   data=dat) )
  }

  if(showN && 'points' %in% names(R) && 'N' %in% names(R$points) &&
     diff(range(R$points[, N])) > 0)
    g <- g + geom_text(aes(x=Inf, y=Inf, label=paste0('N=', N),
                           hjust=1, vjust=1, size=I(2)), data=R$points)

  g
  }

utils::globalVariables(c('y', '.by.','.N','.value.','N','..v','.SD','variable'))
