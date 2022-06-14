#' Combination Plot
#' 
#' Generates a plotly attribute plot given a series of possibly overlapping binary variables
#' 
#' Similar to the \code{UpSetR} package, draws a special dot chart sometimes called an attribute plot that depicts all possible combination of the binary variables.  By default a positive value, indicating that a certain condition pertains for a subject, is any of logical \code{TRUE}, numeric 1, \code{"yes"}, \code{"y"}, \code{"positive"}, \code{"+"} or \code{"present"} value, and all others are considered negative.  The user can override this determination by specifying her own \code{pos} function.  Case is ignored in the variable values.
#' 
#' The plot uses solid dots arranged in a vertical line to indicate which combination of conditions is being considered.  Frequencies of all possible combinations are shown above the dot chart.  Marginal frequencies of positive values for the input variables are shown to the left of the dot chart.  More information for all three of these component symbols is provided in hover text.
#'
#' Variables are sorted in descending order of marginal frqeuencies and likewise for combinations of variables.
#' 
#' @param formula a formula containing all the variables to be cross-tabulated, on the formula's right hand side.  There is no left hand side variable.  If \code{formula} is omitted, then all variables from \code{data} are analyzed.
#' @param data input data frame.  If none is specified the data are assumed to come from the parent frame.
#' @param subset an optional subsetting expression applied to \code{data}
#' @param na.action see \code{lm} etc.
#' @param vnames set to \code{"names"} to use variable names to label axes instead of variable labels.  When using the default \code{labels}, any variable not having a label will have its name used instead.
#' @param includenone set to \code{TRUE} to include the combination where all conditions are absent
#' @param showno set to \code{TRUE} to show a light dot for conditions that are not part of the currently tabulated combination
#' @param maxcomb maximum number of combinations to display
#' @param minfreq if specified, any combination having a frequency less than this will be omitted from the display
#' @param N set to an integer to override the global denominator, instead of using the number of rows in the data
#' @param pos a function of vector returning a logical vector with \code{TRUE} values indicating positive
#' @param obsname character string noun describing observations, default is \code{"subjects"}
#' @param ptsize point size, defaults to 35
#' @param width width of \code{plotly} plot
#' @param height height of \code{plotly} plot
#' @param \dots optional arguments to pass to \code{table}
#' 
#' @return \code{plotly} object
#' @author Frank Harrell
#' @examples
#' if (requireNamespace("plotly")) {
#'   g <- function() sample(0:1, n, prob=c(1 - p, p), replace=TRUE)
#'   set.seed(2); n <- 100; p <- 0.5
#'   x1 <- g(); label(x1) <- 'A long label for x1 that describes it'
#'   x2 <- g()
#'   x3 <- g(); label(x3) <- 'This is<br>a label for x3'
#'   x4 <- g()
#'   combplotp(~ x1 + x2 + x3 + x4, showno=TRUE, includenone=TRUE)
#'
#'   n <- 1500; p <- 0.05
#'   pain       <- g()
#'   anxiety    <- g()
#'   depression <- g()
#'   soreness   <- g()
#'   numbness   <- g()
#'   tiredness  <- g()
#'   sleepiness <- g()
#'   combplotp(~ pain + anxiety + depression + soreness + numbness +
#'             tiredness + sleepiness, showno=TRUE)
#' }
#' @export

combplotp <- function(formula, data=NULL, subset, na.action=na.retain,
                      vnames=c('labels', 'names'),
                      includenone=FALSE, showno=FALSE,
                      maxcomb=NULL, minfreq=NULL, N=NULL,
                      pos=function(x) 1 * (tolower(x) %in% 
                        c('true', 'yes', 'y', 'positive', '+', 'present', '1')),
                      obsname='subjects',
                      ptsize=35, width=NULL, height=NULL,
                      ...) {

  if (!requireNamespace("plotly"))
    stop("This function requires the 'plotly' package.")
  
  vnames <- match.arg(vnames)
  frac   <- markupSpecs$html$frac
  fr2    <- function(a, b) paste0(frac(a, b), ' = ', round(a / b, 3))
  
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
  if(! length(N)) N <- length(Y[[1]])    # no. obs
  f <- as.data.frame(table(Y, ...))
  f <- f[f$Freq > 0, ]   # subset didn't work
  p <- ncol(f) - 1       # no. variables
  numcondpresent <- apply(f[, 1 : p], 1, function(u) sum(u == 1))
  Nc <- sum(f$Freq[numcondpresent > 0])  # no. obs with any condition
  if(! includenone && any(numcondpresent == 0))
      f <- f[numcondpresent > 0, ]

  # Sort combinations in descending order of frequency
  # Tie-breaker is row order when a combination has only one condition
  mdesc <- sort(m)
  mdesc <- 1 : length(mdesc)
  names(mdesc) <- names(sort(m))
  g <- function(x) {
    i <- x > 0
    ifelse(sum(i) == 1, mdesc[names(x)[i]], 0)
  }

  tiebr <- apply(f[, 1 : p], 1, g)
  i <- order(-f$Freq, -tiebr)
  f <- f[i, ]


  if(length(maxcomb) && maxcomb < nrow(f))     f <- f[1 : maxcomb, ]
  if(length(minfreq) && any(f$Freq < minfreq)) f <- f[f$Freq >= minfreq, ]
  
  n <- nrow(f)        # no. combinations
  X <- as.matrix(1 * (f[, 1 : p] == '1'))
  Freq <- f$Freq
  
  # String out information
  x <- y <- present <- txt <- xn <- frq <- NULL
  namx <- colnames(X)
  for(i in 1 : n) {
    x         <- c(x, rep(i, p))
    y         <- c(y, 1 : p)
    xi        <- X[i, ]
    present   <- c(present, xi)
    namespres <- if(! any(xi == 1)) 'none' else
                  paste(labs[namx][xi == 1], collapse='<br>')
    k         <- Freq[i]
    tx <- paste0('<b>', namespres, '</b><br>',
                 '<br>Count: ',                                k,
                 '<br>Fraction of ', obsname, ': ',            fr2(k, N),
                 '<br>Fraction of ', obsname, ' w/any cond: ', fr2(k, Nc))
    txt <- c(txt, rep(tx, p))
    xn  <- c(xn,  namx)
    frq <- c(frq, rep(k, p))
  }
  txt <- paste0(txt, '<br>Fraction of ', obsname, ' w/', namx[y], ': ',
                fr2(frq, m[namx[y]]))
  hdc <- plotlyParm$heightDotchartb
  if(! length(height)) height <- hdc(c(labs, '', ''), low=250, per=30)
  if(! length(width)) {
    # Find longest html line in labs
    w        <- unlist(strsplit(labs, '<br>'))
    longest  <- w[which.max(nchar(w))]
    nlongest <- nchar(longest)
    width    <- hdc(rep('X', n), per=23, low=450) + 8 * nlongest
  }
  auto <- .Options$plotlyauto
  if(length(auto) && auto) {height <- width <- NULL}
  P <- plotly::plot_ly(height=height, width=width)
  
  # Add grid lines to take control of their span
  yy <- 1 : p
  P <- plotly::add_segments(P,
                            x = ~ rep(-2, p), xend = ~ rep(n, p),
                            y = ~ 1 : p, yend = ~ 1 : p,
                            color = I('gray80'), line=list(width=0.75),
                            hoverinfo='none', showlegend=FALSE)
  
  P <- plotly::add_segments(P,
                            x = ~ 1 : n, xend = ~ 1 : n,
                            y = ~ rep(1, n), yend = ~ rep(p + 1.5, n),
                            color = I('gray80'), line=list(width=0.75),
                            hoverinfo='none', showlegend=FALSE)

  # Show main result as dot chart
  P <- plotly::add_markers(P,
                           x    = ~ x[present == 1],
                           y    = ~ y[present == 1],
                           text = ~ txt[present == 1],
                           hoverinfo='text',
                           color=I('black'), size=I(ptsize),
                           showlegend=FALSE)

  if(showno)
    P <- plotly::add_markers(P,
                             x = ~ x[present == 0],
                             y = ~ y[present == 0],
                             hoverinfo='none',
                             color=I('gray90'), size=I(ptsize),
                             showlegend=FALSE)
                             
  
  # Add a trace showing marginal frequencies on the left as segments
  relfreq <- m[namx] / max(m)
  tmf <- paste0('<b>', labs[namx],
                '</b><br><br>Marginal count: ',               m[namx],
                '<br>Fraction of ', obsname, ': ',            fr2(m[namx], N),
                '<br>Fraction of ', obsname, ' w/any cond: ', fr2(m[namx], Nc))
                
  P <- plotly::add_segments(P,
                            x = ~ rep(0, p), xend= ~ -2 * relfreq,
                            # y = ~ labs[namx], yend ~ labs[namx],
                            y = ~ 1 : p, yend ~ 1 : p,
                            text = ~ tmf,
                            hoverinfo='text', color=I('blue'), 
                            name='Marginal Counts',
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
  txtc <- paste0('<b>', txtc, '</b>',
                 '<br><br>Count: ',                            Freq,
                 '<br>Fraction of ', obsname, ': ',            fr2(Freq, N),
                 '<br>Fraction of ', obsname, ' w/any cond: ', fr2(Freq, Nc))
  
  P <- plotly::add_segments(P,
                            x    = ~ nn,
                            xend = ~ nn,
                            y    = ~ rep(p + 0.5, n),
                            yend = ~ p + 0.5 + relfreqc,
                            text = ~ txtc,
                            hoverinfo='text', color=I('black'),
                            name='Combination Counts',
                            showlegend=TRUE, line=list(width=3))
  
  # Add variable labels as annotations
  P <- plotly::add_text(P,
                        x = ~ rep(n + 0.7, p), y = 1 : p,
                        text = ~ labs[namx],
                        textposition="middle right",
                        hoverinfo='none',
                        showlegend=FALSE)
  
                          
  # leave extra space for long label
  P <- plotly::layout(P,
                      xaxis = list(title='', tickvals=1 : n,
                                   range=c(-2, n + 0.4 * nlongest),
                                   showgrid=FALSE,
                                   showticklabels=FALSE, zeroline=FALSE),
                      yaxis = list(title='', tickvals=1 : p,
                                   showgrid=FALSE,
                                   showticklabels=FALSE),
                      legend= list(x=0.5, y=0, xanchor='center', yanchor='top', orientation='h'))

  P
}
