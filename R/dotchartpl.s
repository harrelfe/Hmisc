dotchartpl <- function(x, major, minor=NULL, group=NULL, mult=NULL,
                       big=NULL, htext=NULL,
                       num=NULL, denom=NULL,
                       xlim=NULL, xlab='Proportion',
                       width=800,
                       col=colorspace::rainbow_hcl
                       ) {

  mu   <- markupSpecs$html
  bold <- mu$bold
  
  minorpres <- length(minor) > 0
  grouppres <- length(group) > 0
  multpres  <- length(mult)  > 0

  cols <- if(length(col)) {
            if(! is.function(col)) col
            else
              if(grouppres) col(length(unique(as.character(group))))
            else
              col(1) }

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
         length(mult), length(big), length(htext))
  w <- w[w > 0]
  if(diff(range(w)) > 0)
    stop('x, major, minor, group, mult, big, htext must all have same length when used')

  y <- 1
  X <- Y <- numeric(0)
  Group <- Htext <- character(0)
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
  if(any(d$Big)) {
    db <- subset(d, Big)
    p <- plotly::plot_ly(data=db, x=~ X, y=~ Y, color=~ Group, text=~ Htext,
                         type='scatter', mode='markers', colors=cols,
                         hoverinfo='text',
                         height=plotlyParm$heightDotchart(lines), width=width)
  }
  else
    p <- plotly::plot_ly()
    
  if(any(! d$Big)) {
    dnb <- subset(d, ! Big)
    p <- plotly::add_markers(p, data=dnb, x=~ X, y=~ Y,
                             color=~ Group, text=~ Htext,
                             colors=cols,  # mode='markers'
                             marker=list(opacity=0.45, size=4),
                             hoverinfo='text')
    }
  leftmargin <- plotlyParm$lrmargin(ytnb)
  plotly::layout(p,
                 xaxis=list(title=xlab,
                            range=if(length(xlim)) xlim else range(x),
                            zeroline=FALSE),
                 yaxis=list(title='',
                            range=c(min(Y) - 0.2, 0.2),
                            zeroline=FALSE, tickvals=yl, ticktext=yt),
                 margin=list(l=leftmargin))
#                 colors=cols)
  }
