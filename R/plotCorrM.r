##' Plot Correlation Matrix and Correlation vs. Time Gap
##'
##' Constructs two `ggplot2` graphics.  The first is a half matrix of rectangles where the height of the rectangle is proportional to the absolute value of the correlation coefficient, with positive and negative coefficients shown in different colors.  The second graphic is a variogram-like graph of correlation coefficients on the y-axis and absolute time gap on the x-axis, with a `loess` smoother added.  The times are obtained from the correlation matrix's row and column names if these are numeric.  If any names are not numeric, the times are taken as the integers 1, 2, 3, ...  The two graphics are `ggplotly`-ready if you use `plotly::ggplotly(..., tooltip='label')`.
##' @title plotCorrM
##' @param r correlation matrix
##' @param what specifies whether to return plots or the data frame used in making the plots
##' @param type specifies whether to use bottom-aligned rectangles (the default) or centered circles
##' @param xlab x-axis label for correlation matrix
##' @param ylab y-axis label for correlation matrix
##' @param maxsize maximum circle size if `type='circle'`
##' @param xangle angle for placing x-axis labels, defaulting to 0.  Consider using `xangle=45` when labels are long.
##' @return a list containing two `ggplot2` objects if `what='plots'`, or a data frame if `what='data'`
##' @author Frank Harrell
##' @md
##' @export
##' @examples
##' set.seed(1)
##' r <- cor(matrix(rnorm(100), ncol=10))
##' g <- plotCorrM(r)
##' g[[1]]  # plot matrix
##' g[[2]]  # plot correlation vs gap time
##' # ggplotlyr(g[[2]])
##' # ggplotlyr uses ggplotly with tooltip='label' then removes
##' # txt: from hover text
plotCorrM <- function(r, what=c('plots', 'data'),
                      type=c('rectangle', 'circle'),
                      xlab='', ylab='', maxsize=12, xangle=0) {
  what <- match.arg(what)
  type <- match.arg(type)
  p <- dim(r)[1]
  v <- dimnames(r)[[1]]
  if(! length(v)) v <- as.character(1 : p)
  vn <- as.numeric(v)
  if(any(is.na(vn))) vn <- 1 : p

  mn  <- min(abs(r))
  R   <- as.vector(r)
  x   <- as.vector(col(r))
  y   <- as.vector(row(r))
  txt <- paste0(v[x], ' vs. ', v[y], '<br>r=', round(R, 3))
  d   <- subset(data.frame(x, y, delta=abs(vn[x] - vn[y]), r=R, txt), x < y)
  if(what == 'data') return(d)
  mx <- max(abs(d$r))
  
  g1 <-
    switch(type,
           rectangle = ggplot(d,
                              aes(x=x, y=y,
                                  color=ifelse(r > 0, '+', '-'), label=txt)) +
             geom_segment(aes(x=x, y=y, xend=x, yend=y + 0.9 * abs(r) / mx),
                          size=3),
           circle    = ggplot(d,
                              aes(x=x, y=y,
                                  color=ifelse(r > 0, '+', '-'), label=txt,
                                  size=abs(r))) +
             geom_point() + scale_size(range = c(0, maxsize)) )
           
    g1 <- g1 + scale_x_continuous(breaks = 1 : p, labels=v) +
      scale_y_continuous(breaks = 1 : p, labels=v) +
      guides(color = guide_legend(title=''),
             size  = guide_legend(title='r')) +
      xlab(xlab) + ylab(ylab) +
      theme(axis.text.x=element_text(angle=xangle,
                                     hjust=if(xangle != 0) 1)) +
      labs(subtitle=paste0('max |r|:', round(mx, 3),
                           '  min |r|:', round(mn, 3)))
  
  # Would not run loess if use text=txt
  # Need to run ggplotly with tooltip='label'
  g2 <- ggplot(d, aes(x=delta, y=r, label=txt)) +
    geom_point() + geom_smooth(method=loess) +
    xlab('Absolute Time Difference') + ylab('Correlation')
  list(g1, g2)
  }

utils::globalVariables('delta')
