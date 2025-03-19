histSpikeg <- function(formula=NULL, predictions=NULL, data, plotly=NULL,
                       lowess=FALSE, xlim=NULL, ylim=NULL,
                       side=1, nint=100,
                       frac=function(f) 0.01 + 0.02*sqrt(f-1) /
                         sqrt(max(f,2)-1),
                       span=3/4, histcol='black', showlegend=TRUE) {

  ## Raw data in data, predicted curves are in predictions
  ## If predictions is not given, side (1 or 3) is used
  v  <- all.vars(formula)
  yv <- v[ 1]
  xv <- v[-1]
  X  <- xv[1]

  if(lowess) {
    if(yv %nin% names(data))
      stop(paste(yv, 'must be in data if lowess=TRUE'))
    yval <- data[[yv]]
    iter <- if(length(unique(yval[! is.na(yval)])) > 2) 3 else 0
    lows <- function(x, y, iter, span) {
      i <- ! is.na(x + y)
      lowess(x[i], y[i], iter=iter, f=span)
    }
  }

  if(length(xv) > 1 && ! (lowess || length(predictions)))
    stop('predictions must be given or lowess=TRUE if formula has > 1 variable on the right')
  x1 <- data[[X]]
  if(length(xlim)) {
    data <- data[x1 >= xlim[1] & x1 <= xlim[2], ]
    x1 <- data[[X]]
  }
  if(length(unique(x1)) > nint) {
    eps <- diff(range(x1, na.rm=TRUE)) / nint
    x1  <- round(x1 / eps) * eps
    data[[X]] <- x1
  }
  p  <- predictions
#  xr <- NULL

#  if(length(p))   xr <- range(p[[X]],    na.rm=TRUE)
#  else if(lowess) xr <- range(data[[X]], na.rm=TRUE)
  xr <- xlim

  if(length(xv) == 1) {
    tab      <- as.data.frame(do.call(table, data[X]))
    tab[[X]] <- as.numeric(as.character(tab[[X]]))
    if(length(xr)) tab    <- tab[tab[[X]] >= xr[1] & tab[[X]] <= xr[2], ]
    if(lowess) {
      p <- as.data.frame(lows(data[[X]], yval, iter, span))
      names(p) <- c(X, yv)
    }

    if(length(p)) tab$.yy. <- approxExtrap(p[[X]], p[[yv]], xout=tab[[X]])$y
  } else {  ## grouping variable(s) present
    tab <- as.data.frame(do.call(table, data[xv]))
    tab <- tab[tab$Freq > 0, ]
    tab[[X]] <- as.numeric(as.character(tab[[X]]))
    if(length(xr)) tab    <- tab[tab[[X]] >= xr[1] & tab[[X]] <= xr[2], ]
    tab$.yy. <- rep(NA, nrow(tab))
    gv <- xv[-1]           # grouping variables
    U <- unique(tab[gv])   # unique combinations
    if(! lowess) {
      for(k in 1 : nrow(U)) {
        i <- rep(TRUE, nrow(tab))
        j <- rep(TRUE, nrow(p))
        for(l in 1 : ncol(U)) {
          currgroup <- as.character(U[k, gv[l], drop=TRUE])
          i <- i & (tab[[gv[l]]] == currgroup)
          j <- j & (p[[gv[l]]]   == currgroup)
        }   ## now all grouping variables intersected
        tab$.yy.[i] <- approxExtrap(p[[X]][j], p[[yv]][j], xout=tab[[X]][i])$y
      }
    } ## end if(! lowess)
    else {  # lowess; need to compute p
      p <- NULL
      for(k in 1 : nrow(U)) {
        i <- rep(TRUE, nrow(tab))
        j <- rep(TRUE, nrow(data))
        for(l in 1 : ncol(U)) {
          currgroup <- as.character(U[k, gv[l], drop=TRUE])
          i <- i & (tab[[gv[l]]]  == currgroup)
          j <- j & (data[[gv[l]]] == currgroup)
        }  ## now all grouping variables intersected
        sm <- lows(data[[X]][j], data[[yv]][j], iter, span)
        Sm <- sm; names(Sm) <- c(X, yv)
        Uk <- U[k, , drop=FALSE]; row.names(Uk) <- NULL
        p <- rbind(p, data.frame(Uk, Sm))
        tab$.yy.[i] <- approxExtrap(sm, xout=tab[[X]][i])$y
      }
    } ## end lowess
  }  ## end grouping variables present

  if(! length(ylim)) {
    if(length(data) && yv %in% names(data))
      ylim <- range(data[[yv]], na.rm=TRUE)
    else if(length(p)) ylim <- range(p[[yv]], na.rm=TRUE)
  }
  if(! length(ylim)) stop('no way to infer ylim from information provided')

  tab$.rf. <- frac(tab$Freq) * diff(ylim)
  n <- nrow(tab)
  tab$.ylo. <- if(length(p)) tab$.yy. - tab$.rf.
  else if(side == 1) rep(ylim[1], n) else rep(ylim[2], n)

  tab$.yhi. <- if(length(p)) tab$.yy. + tab$.rf.
               else if(side == 1) ylim[1] + tab$.rf. else ylim[2] - tab$.rf.

  P <- plotly
  if(! is.null(P) && requireNamespace("plotly")) {
    tab$.xx. <- tab[[X]]
    
## Got NA/NaN argument, numerical expression has 8 elements
## when using add_segments
#    P <- plotly:add_segments(P, data=tab,
#                             x=~ .xx.,  xend=~ .xx.,
#                             y=~ .ylo., yend=~ .yhi.,
#                             hoverinfo='none',
#                             name='Histogram', legendgroup='Histogram')
    n <- 3 * nrow(tab)
    .xx. <- .yy. <- rep(NA, n)
    .xx.[seq(1, n, by=3)] <- tab[[X]]
    .xx.[seq(2, n, by=3)] <- tab[[X]]
    .yy.[seq(1, n, by=3)] <- tab$.ylo.
    .yy.[seq(2, n, by=3)] <- tab$.yhi.
    P <- plotly::add_lines(P, data=data.frame(.xx., .yy.),
                           x=~.xx., y=~.yy.,
                           line=list(color=histcol, width=1.4),
                           hoverinfo='none', showlegend=showlegend,
                           name='Histogram', legendgroup='Histogram')
    if(lowess) {
      af <- function(x) as.formula(paste('~', x))
      P <- plotly::add_lines(P, data=p, x=af(X), y=af(yv),
                             hoverinfo='none', showlegend=showlegend,
                             name='loess', legendgroup='loess')
      }
    return(P)
  }
  res <- list(hist=geom_segment(data=tab, aes(x=.data[[X]], xend=.data[[X]],
                                              y=.data$.ylo., yend=.data$.yhi.),
                                          size=0.25, col=histcol),
              lowess=if(lowess) geom_line(data=p, aes(x=.data[[X]], y=.data[[yv]])) )
  res
}

# utils::globalVariables(c('aes', 'Freq', '.ylo.', '.yhi.', 'x', 'y'))