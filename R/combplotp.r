#' Combination Plot
#' 
#' Generates a plotly attribute plot given a series of possibly overlapping binary variables
#' 
#' Similar to the \code{UpSetR} package, draws a special dot chart sometimes called an attribute plot that depicts all possible combination of the binary variables.  By default a positive value, indicating that a certain condition pertains for a subject, is any of logical \code{TRUE}, numberic 1, \code{"yes"}, \code{"y"}, \code{"positive"}, \code{"+} or \code{"present"} value, and all others are considered negative.  The user can override this determination by specifying her own \code{pos} function.  Case is ignored in the variable values.
#' 
#' The plot uses solid dots arranged in a vertical line to indicate which combination of conditions is being considered.  Frequencies of all possible combinations are shown above the dot chart.  Marginal frequencies of positive values for the input variables are shown to the left of the dot chart.  More information for all three of these component symbols is provided in hover text.
#' 
#' @param formula a formula containing all the variables to be cross-tabulated, on the formula's right hand side.  There is no left hand side variable.  If \code{formula} is omitted, then all variables from \code{data} are analyzed.
#' @param data input data frame.  If none is specified the data are assumed to come from the parent frame.
#' @param subset an optional subsetting expression applied to \code{data}
#' @param na.action see \code{lm} etc.
#' @param vmames set to \code{"names"} to use variable names to label axes instead of variable labels.  When using the default \code{labels}, any variable not having a label will have its name used instead.
#' @param pos a function of vector returning a logical vector with \code{TRUE} values indicating positive
#' @param width width of \code{plotly} plot
#' @param height height of \code{plotly} plot
#' @param \dots optional arguments to pass to \code{table}
#' 
#' @return \code{plotly} object
#' @author Frank Harrell
#' @examples
#' g <- function() sample(0:1, 100, replace=TRUE)
#' set.seed(2)
#' x1 <- g(); label(x1) <- 'A long label for x1 that describes it'
#' x2 <- g()
#' x3 <- g(); label(x3) <- 'This is<br>a label for x3'
#' x4 <- g()
#' combplotp(~ x1 + x2 + x3 + x4)
#' @export

combplotp <- function(formula, data=NULL, subset, na.action=na.retain,
                      vnames=c('labels', 'names'),
                      pos=function(x) 1 * (toupper(x) %in% 
                        c('true', 'yes', 'y', 'positive', '+', 'present', '1')),
                      width=NULL, height=NULL,
                      ...) {
  vnames <- match.arg(vnames)
  Y <- if(missing(formula)) {
    if(! missing(subset)) stop('subset not allowed if formula missing')
    if(! length(data)) stop('data must be specified if formula missing')
    data
    } else {
      if(!missing(subset) && length(subset))
        model.frame(formula, data=data, subset=subset, na.action=na.action)
      else
        model.frame(formula, data=data, na.action=na.action)
      }

  # Get variable labels, defaulting to variable names
  labs <- if(vnames == 'names') structure(names(Y), names=names(Y))
  else {
    lbs <- sapply(Y, label)
    ifelse(lbs == '', names(Y), lbs)
  }

  # Convert Y to logical TRUE/FALSE
  Y <- lapply(Y, pos)
  # Compute marginal frequencies
  m <- sapply(Y, sum, na.rm=TRUE)
  # Sort variables in order of descending marginal frequency
  Y <- Y[order(m)]
  f <- as.data.frame(table(Y, ...))
  n <- nrow(f)        # no. combinations
  p <- ncol(f) - 1    # no. variables
  X <- as.matrix(1 * (f[, 1 : p] == '1'))
  Freq <- f$Freq
  
  # String out information
  x <- y <- present <- txt <- xn <- NULL
  namx <- colnames(X)
  for(i in 1 : n) {
    x <- c(x, rep(i, p))
    y <- c(y, 1 : p)
    xi <- X[i, ]
    present <- c(present, xi)
    namespres <- if(! any(xi == 1)) 'none' else
      paste(labs[namx][xi == 1], collapse='<br>')
    tx <- paste0('Conditions:<br>', namespres, '<br>Frequency:', Freq[i])
    txt <- c(txt, rep(tx, p))
    xn  <- c(xn, namx)
  }
  
  hdc <- plotlyParm$heightDotchartb
  if(! length(height)) height <- hdc(c(labs, '', ''), low=250)
  if(! length(width)) {
    # Find longest html line in labs
    w <- unlist(strsplit(labs, '<br>'))
    longest <- w[which.max(nchar(w))]
    width <- hdc(c(rep('', n), longest))
  }
  P <- plotly::plot_ly(height=height, width=width)
  
  # Add grid lines to take control of their span
  yy <- 1 : p
  P <- plotly::add_segments(P,
                            x = ~ rep(-2, p), xend = ~ rep(n, p),
                            y = ~ 1 : p, yend = ~ 1 : p,
                            color = I('gray80'), line=list(width=0.75),
                            showlegend=FALSE)
  
  P <- plotly::add_segments(P,
                            x = ~ 1 : n, xend = ~ 1 : n,
                            y = ~ rep(1, n), yend = ~ rep(p + 1.5, n),
                            color = I('gray80'), line=list(width=0.75),
                            showlegend=FALSE)
  
  P <- plotly::add_markers(P,
                           x = ~ x[present == 1],
                           y = ~ y[present == 1],
                           text = ~ txt[present == 1],
                           hoverinfo='text',
                           color=I('black'), size=I(35),
                           showlegend=FALSE)
  
  # Add a trace showing marginal frequencies on the left as segments
  relfreq <- m[namx] / max(m)
  P <- plotly::add_segments(P,
                            x = ~ rep(0, p), xend= ~ -2 * relfreq,
                            # y = ~ labs[namx], yend ~ labs[namx],
                            y = ~ 1 : p, yend ~ 1 : p,
                            text = ~ paste0(labs[namx], '<br>Marginal<br>frequency:', m[namx]),
                            hoverinfo='text', color=I('blue'), 
                            name='Marginal Frequencies',
                            showlegend=TRUE,
                            line=list(width=3)
                            )

  # Add a trace showing the individual combination frequencies on top
  relfreqc <- Freq / max(Freq)
  nn <- 1 : n
  xi <- X[i, ]
  present <- c(present, xi)
  namespres <- if(! any(xi == 1)) 'none' else
    paste(labs[namx][xi == 1], collapse='<br>')
  txtc <- character(n)
  for(i in 1 : n) {
    xi <- X[i, ]
    txtc[i] <- if(! any(xi == 1)) 'none' else
      paste(labs[namx][xi == 1], collapse='<br>')
  }
  txtc <- paste0('Combination:<br>', txtc, '<br>Frequency:', Freq)
  
  P <- plotly::add_segments(P,
                            x    = ~ nn,
                            xend = ~ nn,
                            y    = ~ rep(p + 0.5, n),
                            yend = ~ p + 0.5 + relfreqc,
                            text = ~ txtc,
                            hoverinfo='text', color=I('black'),
                            name='Combination Frequencies',
                            showlegend=TRUE, line=list(width=3))
  
  # Add variable labels as annotations
  P <- plotly::add_text(P,
                        x = ~ rep(n + 1, p), y = 1 : p,
                        text = ~ labs[namx],
                        textposition="middle right",
                        showlegend=FALSE)
  
                          
  nc <- nchar(longest)  # leave extra space for long label
  P <- plotly::layout(P,
                      xaxis = list(title='', tickvals=1 : n,
                                   range=c(-2, n + 0.4 * nc),
                                   showgrid=FALSE,
                                   showticklabels=FALSE, zeroline=FALSE),
                      yaxis = list(title='', tickvals=1 : p,
                                   showgrid=FALSE,
                                   showticklabels=FALSE),
                      legend= list(x=0.5, y=0, xanchor='center', yanchor='top', orientation='h'))

  P
}
