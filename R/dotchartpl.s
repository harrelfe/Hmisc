dotchartpl <- function(x, major, minor=NULL, group=NULL, mult=NULL,
                       big=NULL, htext=NULL,
                       num=NULL, denom=NULL,
                       lower=NULL, upper=NULL,
                       xlim=NULL, xlab='Proportion',
                       tracename=NULL, limitstracename=NULL,
                       width=800,
                       col=colorspace::rainbow_hcl
                       ) {

  mu   <- markupSpecs$html
  bold <- mu$bold

  if(! length(xlim)) xlim <- c(min(c(x, lower), na.rm=TRUE),
                               max(c(x, upper), na.rm=TRUE))
  
  minorpres <- length(minor) > 0
  grouppres <- length(group) > 0
  multpres  <- length(mult)  > 0
  limspres  <- length(lower) * length(upper) > 0

  cols <- if(length(col)) {
            if(! is.function(col)) col
            else
              if(grouppres) col(length(unique(as.character(group))))
            else
              col(1) }
  if(! length(col) && ! grouppres) col <- 'black'

  ht <- htext
  if(length(num))
    ht <- paste0(ht, if(length(htext)) mu$lspace,
                 round(x, 3), mu$lspace,
                 mu$frac(num, denom, size=95))
  ht <- paste(ht, if(length(ht)) '<br>', as.character(major))
  if(minorpres) ht <- paste0(ht, ': ',   as.character(minor))
  if(grouppres) ht <- paste0(ht, '<br>', as.character(group))
  if(multpres)  ht <- paste0(ht, '<br>', as.character(mult))

  n <- length(x)

  minor <- if(minorpres) as.character(minor) else rep('', n)
  group <- if(grouppres) as.character(group) else rep('', n)
  mult  <- if(multpres)  as.character(mult)  else rep('', n)
  if(! length(big)) big <- rep(TRUE, n)

  w <- c(length(x), length(major), length(minor), length(group),
         length(mult), length(big), length(htext), length(lower), length(upper))
  w <- w[w > 0]
  if(diff(range(w)) > 0)
    stop('x, major, minor, group, mult, big, htext, lower, upper must all have same length when used')

  y <- 1
  X <- Y <- Lower <- Upper <- numeric(0)
  Group <- Htext <- Htextl <- character(0)
  Big <- logical(0)
  
  yl <- numeric(0); yt <- ytnb <- character(0)
  lines <- 0

  for(ma in unique(major)) {
    y    <- y - 1
    yl   <- c(yl, y)
    yt   <- c(yt, bold(ma))
    ytnb <- c(ytnb, ma)
    
    lminor <- unique(minor[major == ma])
    y <- y + if(all(lminor == '')) 0.4 else 0
    lines <- lines + 1
    for(mi in lminor) {
      y     <- y - 0.4
      lines <- lines + (mi != '')
      j     <- which(major == ma & minor == mi)
      m     <- length(j)
      X     <- c(X, x[j])
      Y     <- c(Y, ifelse(big[j], y, y - .14))
      if(limspres) {
        Lower  <- c(Lower, lower[j])
        Upper  <- c(Upper, upper[j])
        Htextl <- paste0('[', format(lower[j], digits=4), ', ',
                              format(upper[j], digits=4), ']')
        }
      Group <- c(Group, group[j])
      Big   <- c(Big, big[j])
      Htext <- c(Htext, ht[j])
      
      if(mi != '') {
        yl   <- c(yl, y)
        yt   <- c(yt, mi)
        ytnb <- c(ytnb, mi)
      }
    } # end mi in lminor
  } # end ma in lmajor

  d  <- data.frame(X, Y, Group, Htext, Big)

  if(limspres) {
    d$Lower  <- Lower
    d$Upper  <- Upper
    d$Htextl <- Htextl
    }

  if(! grouppres) d$Group <- NULL  ####
  if(any(d$Big)) {
    db <- subset(d, Big)
    p <- plotly::plot_ly(data=db,
                         height=plotlyParm$heightDotchart(lines), width=width)
    if(limspres)
      p <- if(grouppres)
             plotly::add_segments(p, x=~ Lower, xend=~ Upper,
                                     y=~ Y,     yend=~ Y,
                                     color=~ Group, text= ~Htextl,
                                     colors=cols, hoverinfo='text')
      else   plotly::add_segments(p, x=~ Lower, xend=~ Upper,
                                     y=~ Y,     yend=~ Y,
                                     text= ~Htextl,
                                     color=I('lightgray'), hoverinfo='text',
                                     name=limitstracename)

    p <- if(grouppres)
           plotly::add_markers(p, x=~ X, y=~ Y,
                               color=~ Group, text=~ Htext,
                               colors=cols, hoverinfo='text')
   else   plotly::add_markers(p, x=~ X, y=~ Y,
                              text=~ Htext, color=I('black'),
                              hoverinfo='text', name=tracename)
  }
  else
    p <- plotly::plot_ly()
    
  if(any(! d$Big)) {
    dnb <- subset(d, ! Big)
    if(limspres)
      p <- if(grouppres)
             plotly::add_segments(p, data=dnb,
                                  x=~ Lower, xend=~ Upper,
                                  y=~ Y,     yend=~ Y,
                                  color=~ Group, text=~ Htextl,
                                  colors=cols, hoverinfo='text')
       else  plotly::add_segments(p, data=dnb,
                                  x=~ Lower, xend=~ Upper,
                                  y=~ Y,     yend=~ Y,
                                  text=~ Htextl,
                                  color=I('lightgray'), hoverinfo='text',
                                  name=limitstracename)

    p <- if(grouppres)
           plotly::add_markers(p, data=dnb, x=~ X, y=~ Y,
                               color=~ Group, text=~ Htext,
                               colors=cols,
                               marker=list(opacity=0.45, size=4),
                               hoverinfo='text')
    else   plotly::add_markers(p, data=dnb, x=~ X, y=~ Y,
                               text=~ Htext,
                               marker=list(opacity=0.45, size=4),
                               color=I('black'), hoverinfo='text',
                               name=tracename)
    }
  leftmargin <- plotlyParm$lrmargin(ytnb)
  plotly::layout(p,
                 xaxis=list(title=xlab,
                            range=xlim,
                            zeroline=FALSE),
                 yaxis=list(title='',
                            range=c(min(Y) - 0.2, 0.2),
                            zeroline=FALSE, tickvals=yl, ticktext=yt),
                 margin=list(l=leftmargin))
  }
