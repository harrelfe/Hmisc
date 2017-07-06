dotchartpl <- function(x, major=NULL, minor=NULL, group=NULL, mult=NULL,
                       big=NULL, htext=NULL,
                       num=NULL, denom=NULL,
                       lower=NULL, upper=NULL,
                       refgroup=NULL, sortdiff=TRUE, conf.int=0.95,
                       minkeep=NULL,
                       xlim=NULL, xlab='Proportion',
                       tracename=NULL, limitstracename=NULL,
                       width=800,
                       col=colorspace::rainbow_hcl
                       ) {

  mu   <- markupSpecs$html
  bold <- mu$bold

  if(! length(xlim)) xlim <- c(min(c(x, lower), na.rm=TRUE),
                               max(c(x, upper), na.rm=TRUE))

  majorpres <- length(major) > 0
  major <- if(majorpres) as.character(major) else rep('', length(x))
  minorpres <- length(minor) > 0
  grouppres <- length(group) > 0
  multpres  <- length(mult)  > 0
  limspres  <- length(lower) * length(upper) > 0
  rgpres    <- length(refgroup) > 0

  if(minorpres)     minor <- as.character(minor)
  if(grouppres)     group <- as.character(group)
  if(multpres)      mult  <- as.character(mult)
  ugroup <- if(grouppres) unique(group)

  if(rgpres) {
    if(! grouppres || multpres || length(big))
      stop('when refgroup is given, group must be given and mult and big must not be used')
    if(length(ugroup) != 2)
      stop('refgroup only works for 2 groups')
    if(refgroup %nin% unique(group))
      stop(paste('refgroup must be one of the following:',
                 paste(unique(group), collapse=', ')))
    altgroup <- setdiff(ugroup, refgroup)
    }

  cols <- if(length(col)) {
            if(! is.function(col)) col
            else
              if(grouppres) col(length(unique(group)))
            else
              col(1) }
  if(! length(col) && ! grouppres) col <- 'black'

  levelsRemoved <- character(0)
  
  if(rgpres && (sortdiff || length(minkeep)) && minorpres) {
    ## For each major x minor subgroup, hold on to x for both groups

    g <- function(i, wcat) {
      gr <- group[i]
      if(any(gr == wcat)) x[i[gr == wcat]] else NA
    }

    xa <- tapply(1 : length(x), llist(major, minor), g, wcat=refgroup)
    xb <- tapply(1 : length(x), llist(major, minor), g, wcat=altgroup)

    ## Bug in x[cbind()] if major is constant
    if(! majorpres) {
      xa <- xa[, minor]
      xb <- xb[, minor]
    }
    else {
      xa <- xa[cbind(major, minor)]
      xb <- xb[cbind(major, minor)]
    }

    if(length(minkeep)) {
      w <- if(majorpres) paste0(major, ':', minor) else minor
      levelsRemoved <- unique(w[! is.na(xa + xb) &
                                xa < minkeep & xb < minkeep])
      j <- which(! is.na(xa + xb) & (xa >= minkeep | xb >= minkeep))
    }
    else
      j <- 1 : length(x)

    ## Sort minor categories by descending order of between-group differences

    i <- order(major, ifelse(is.na(xa + xb), -Inf, - (xb - xa)))
    i <- intersect(i, j)
    x     <- x[i]
    major <- major[i]
    minor <- minor[i]
    group <- group[i]
    if(length(num))   num   <- num[i]
    if(length(denom)) denom <- denom[i]
    if(length(mult))  mult  <- mult[i]
    if(length(big))   big   <- big[i]
    if(length(lower)) lower <- lower[i]
    if(length(upper)) upper <- upper[i]
    if(length(htext)) htext <- htext[i]
    }


  ht <- htext
  if(length(num))
    ht <- paste0(ht, if(length(htext)) mu$lspace,
                 round(x, 3), mu$lspace,
                 mu$frac(num, denom, size=95))
  ht <- paste0(ht, if(length(ht)) '<br>',
               if(majorpres) paste0(major, ': '))
  if(minorpres) ht <- paste0(ht, minor)
  if(grouppres) ht <- paste0(ht, '<br>', group)
  if(multpres)  ht <- paste0(ht, '<br>', mult)

  n <- length(x)

  minor <- if(minorpres) minor else rep('', n)
  group <- if(grouppres) group else rep('', n)
  mult  <- if(multpres)  mult  else rep('', n)
  if(! length(big)) big <- rep(TRUE, n)

  w <- c(length(x), length(major), length(minor), length(group),
         length(num), length(denom),
         length(mult), length(big), length(htext),
         length(lower), length(upper))
  w <- w[w > 0]
  if(diff(range(w)) > 0)
    stop('x, major, minor, group, num, denom, mult, big, htext, lower, upper must all have same length when used')

  y <- 1
  X <- Y <- Ydiff <- Lower <- Upper <- numeric(0)
  Group <- Htext <- Htextl <- character(0)
  Big <- logical(0)
  
  yl <- numeric(0); yt <- ytnb <- character(0)
  difflower <- diffupper <- Diff <- numeric(0)
  coldiff <- htdiff <- character(0)
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
      if(rgpres) {
        ja <- which(major == ma & minor == mi & group == refgroup)
        jb <- which(major == ma & minor == mi & group == altgroup)
        diff <- x[jb] - x[ja]
        coldiff <- c(coldiff,
                     ifelse(diff > 0,
                            paste0(altgroup, ' ',
                                   htmlTranslate('>'), ' ', refgroup),
                            paste0(refgroup, ' ',
                                   htmlTranslate('>='), ' ', altgroup)))
        Ydiff <- c(Ydiff, y)

        htd <- if(majorpres) paste0(ma, ': ') else ''
        if(minorpres) htd <- paste0(htd, mi)
  
        htd   <- paste0(htd, '<br>', altgroup, ' - ', refgroup, ': ',
                        round(diff, 3))

        if(! is.logical(conf.int) && length(num) && length(denom)) {
          va <- x[ja] * (1 - x[ja]) / denom[ja]
          vb <- x[jb] * (1 - x[jb]) / denom[jb]
          se <- sqrt(va + vb)
          zcrit  <- qnorm((1 + conf.int) / 2)
          dlower <- format(round(x[jb] - x[ja] - zcrit * se, 3), nsmall=3)
          dupper <- format(round(x[jb] - x[ja] + zcrit * se, 3), nsmall=3)
          htd <- paste0(htd, '<br>', conf.int, ' C.L.: [', dlower,
                          ', ', dupper, ']')
          Diff      <- c(Diff, x[jb] - x[ja])
          difflower <- c(difflower, (x[jb] + x[ja]) / 2 - 0.5 * zcrit * se)
          diffupper <- c(diffupper, (x[jb] + x[ja]) / 2 + 0.5 * zcrit * se)
        }
        htdiff <- c(htdiff, htd)
      }
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

    if(length(difflower)) {
      ddiff <- data.frame(Diff, difflower, diffupper, Ydiff, coldiff, htdiff)
      ## Could not get color=coldiff to work; perhaps conflicted with
      ## earlier use of color =
      if(any(Diff > 0))
        p <- plotly::add_segments(p, data=subset(ddiff, Diff > 0),
                                  x= ~ difflower, xend= ~ diffupper,
                                  y= ~ Ydiff, yend= ~ Ydiff,
                                  color = I('lightgray'),
                                  text = ~ htdiff, hoverinfo='text',
                                  name = paste0(htmlSpecial('half'),
                                                ' CL of difference<br>',
                                                coldiff[Diff > 0][1]))
      if(any(Diff <= 0))
        p <- plotly::add_segments(p, data=subset(ddiff, Diff <= 0),
                                  x= ~ difflower, xend= ~ diffupper,
                                  y= ~ Ydiff, yend= ~ Ydiff,
                                  color = I('lavender'),
                                  text = ~ htdiff, hoverinfo='text',
                                  name = paste0(htmlSpecial('half'),
                                                ' CL of difference<br>',
                                                coldiff[Diff <= 0][1]))
      }
    if(limspres)
      p <- if(grouppres)
             plotly::add_segments(p, x=~ Lower, xend=~ Upper,
                                  y=~ Y,     yend=~ Y,
                                  color=~ Group, text= ~ Htextl,
                                  colors=cols, hoverinfo='text')
           else
             plotly::add_segments(p, x=~ Lower, xend=~ Upper,
                                  y=~ Y,     yend=~ Y,
                                  text= ~ Htextl,
                                  color=I('lightgray'), hoverinfo='text',
                                  name=limitstracename)

    p <- if(grouppres)
           plotly::add_markers(p, x=~ X, y=~ Y,
                               color=~ Group, text=~ Htext,
                               colors=cols, hoverinfo='text')
         else
           plotly::add_markers(p, x=~ X, y=~ Y,
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
  p <- plotly::layout(p,
                 xaxis=list(title=xlab,
                            range=xlim,
                            zeroline=FALSE),
                 yaxis=list(title='',
                            range=c(min(Y) - 0.2, 0.2),
                            zeroline=FALSE, tickvals=yl, ticktext=yt),
                 margin=list(l=leftmargin),
                 legend=list(traceorder=if(length(difflower))
                                          'reversed' else 'normal'))

  attr(p, 'levelsRemoved') <- levelsRemoved
  p
  }
