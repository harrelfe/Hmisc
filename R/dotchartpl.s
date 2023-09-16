dotchartpl <- function(x, major=NULL, minor=NULL,
                       group=NULL, mult=NULL,
                       big=NULL, htext=NULL,
                       num=NULL, denom=NULL,
                       numlabel='', denomlabel='',
                       fun=function(x) x, ifun=function(x) x,
                       op='-',
                       lower=NULL, upper=NULL,
                       refgroup=NULL, sortdiff=TRUE, conf.int=0.95,
                       minkeep=NULL,
                       xlim=NULL, xlab='Proportion',
                       tracename=NULL, limitstracename='Limits',
                       nonbigtracename='Stratified Estimates',
                       dec=3, width=800, height=NULL,
                       col=colorspace::rainbow_hcl
                       ) {

  if (!requireNamespace("plotly"))
    stop("This function requires the 'plotly' package.")
  
  mu   <- markupSpecs$html
  bold <- mu$bold

  if(! length(xlim)) xlim <- c(min(c(x, lower), na.rm=TRUE),
                               max(c(x, upper), na.rm=TRUE))

  majorpres <- length(major) > 0
  major <- if(majorpres) as.character(major) else rep(' ', length(x))
  minorpres <- length(minor) > 0
  if(! (majorpres || minorpres)) stop('must specify major or minor or both')
  
  grouppres <- length(group) > 0  ## superpositioning variable
  multpres  <- length(mult)  > 0
  limspres  <- length(lower) * length(upper) > 0
  rgpres    <- length(refgroup) > 0

  if(minorpres) minor <- as.character(minor)
  if(grouppres) group <- as.character(group)
  if(multpres)  mult  <- as.character(mult)
  ugroup <- if(grouppres) unique(group) else ''

  fmt <- function(x) format(round(x, dec))

  if(rgpres) {
    if(! grouppres || multpres || length(big))
      stop('when refgroup is given, group must be given and mult and big must not be used')
    ## big=TRUE for non-stratified estimates
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
  if(! length(col) && ! grouppres) cols <- 'black'

  dropped <- character(0)
  D       <- NULL
  
  if(rgpres && minorpres) {

    z <- pairUpDiff(x, major, minor, group,
                    refgroup=refgroup, lower=lower, upper=upper,
                    minkeep=minkeep, sortdiff=sortdiff, conf.int=conf.int)
    
    Z       <- z$X
    D       <- z$D
    dropped <- z$dropped
    i       <- Z[,'subscripts']
  
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
    }    # end if(rgpres && ...

  ht <- htext
  if(numlabel != '')   numlabel   <- paste0(' ', numlabel)
  if(denomlabel != '') denomlabel <- paste0(' ', denomlabel)
  if(length(num))
    ht <- paste0(ht, if(length(htext)) mu$lspace,
                 fmt(fun(x)), mu$lspace,
                 mu$frac(paste0(num, numlabel),
                         paste0(denom, denomlabel), size=95))

  ## if confidence interval for differences are not to be displayed,
  ## put point estimate confidence intervals in point hypertext
  if(length(ugroup) != 2 && limspres)
    ht <- paste0(ht, ' [',
                 fmt(fun(lower)), ', ',
                 fmt(fun(upper)), ']')

  ht <- paste0(ht, if(length(ht)) '<br>',
               if(majorpres) paste0(major, ': '))
  if(minorpres) ht <- paste0(ht, minor)
  if(grouppres) ht <- paste0(ht, '<br>',
                             gsub(' stratified<br>by .*', '', group))
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
      X     <- c(X, x[j])
      Y     <- c(Y, ifelse(big[j], y, y - .14))
      if(length(D)) {
        k <- which(D$major == ma & D$minor == mi)
        if(length(k) != 1)
          stop('must have one observation for a major/minor/group combination')
        diff <- D$diff[k]
        coldiff <- c(coldiff,
                     ifelse(diff > 0,
                            paste0(altgroup, ' ',
                                   htmlTranslate('>'), ' ', refgroup),
                            paste0(refgroup, ' ',
                                   htmlTranslate('>='), ' ', altgroup)))
        Ydiff <- c(Ydiff, y)

        htd <- if(majorpres) paste0(ma, ': ') else ''
        if(minorpres) htd <- paste0(htd, mi)

        htd   <- paste0(htd, '<br>', altgroup, ' ', op, ' ',
                        refgroup, ': ', fmt(fun(diff)))
        if(! is.logical(conf.int) && limspres && length(D)) {
          dlower <- fmt(fun(D$lower[k]))
          dupper <- fmt(fun(D$upper[k]))
          htd <- paste0(htd, '<br>', conf.int, ' C.L.: [', dlower,
                          ', ', dupper, ']')
          Diff      <- c(Diff, diff)
          difflower <- c(difflower, D$lowermid[k])
          diffupper <- c(diffupper, D$uppermid[k])
        }
        htdiff <- c(htdiff, htd)
      }   # end if(length(D))
      if(limspres && ! length(D)) {
        Lower  <- c(Lower, lower[j])
        Upper  <- c(Upper, upper[j])
        Htextl <- c(Htextl,
                    paste0('[',
                           fmt(fun(lower[j])), ', ',
                           fmt(fun(upper[j])), ']' ) )
      }
      Group <- c(Group, group[j])
      Big   <- c(Big,   big[j])
      Htext <- c(Htext, ht[j])
      
      if(mi != '') {
        yl   <- c(yl, y)
        yt   <- c(yt, mi)
        ytnb <- c(ytnb, mi)
      }
    } # end mi in lminor
  } # end ma in lmajor

  d  <- data.frame(X, Y, Group, Htext, Big)

  if(limspres && ! length(D)) {
    d$Lower  <- Lower
    d$Upper  <- Upper
    d$Htextl <- Htextl
  }

  if(! grouppres) d$Group <- NULL

    if(any(d$Big)) {
    db <- subset(d, Big)  # non-stratified estimates
    ## For some reason, colors= in add_marker did not always take
    if(! length(height)) height <- plotlyParm$heightDotchart(lines)
    auto <- .Options$plotlyauto
    if(length(auto) && auto) height <- width <- NULL
    p <- plotly::plot_ly(data=db, colors=cols, height=height, width=width)
    
    if(limspres && length(D)) {
      ddiff <- data.frame(Diff, difflower, diffupper, Ydiff, coldiff, htdiff)

      ## Could not get color=coldiff to work; perhaps conflicted with
      ## earlier use of color =

      nDiff <- Diff[! is.na(Diff)]
      if(length(nDiff)) {
        if(any(nDiff > 0))
          p <- plotly::add_segments(p, data=subset(ddiff, Diff > 0),
                                    x= ~ difflower, xend= ~ diffupper,
                                    y= ~ Ydiff, yend= ~ Ydiff,
                                    color = I('lightgray'),
                                    text = ~ htdiff, hoverinfo='text',
                                    name = paste0(htmlSpecial('half'),
                                                  ' CL of difference<br>',
                                                  coldiff[Diff > 0][1]))

        if(any(nDiff <= 0))
          p <- plotly::add_segments(p, data=subset(ddiff, Diff <= 0),
                                    x= ~ difflower, xend= ~ diffupper,
                                    y= ~ Ydiff, yend= ~ Ydiff,
                                    color = I('lavender'),
                                    text = ~ htdiff, hoverinfo='text',
                                    name = paste0(htmlSpecial('half'),
                                                  ' CL of difference<br>',
                                                  coldiff[Diff <= 0][1]))
      }
    }

    ## tracename and limitstracename are used if groups not used

    if(limspres && ! length(D)) ##  && length(ugroup) == 2) why ??
      p <- if(grouppres)
             plotly::add_segments(p, data=db,
                                  x=~ Lower, xend=~ Upper,
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
                               hoverinfo='text',
                               name=if(length(tracename)) tracename
                                    else
                                      if(any(! d$Big)) 'All' else '')
    
  } else p <- plotly::plot_ly(colors=cols)   # Big is not used
    
  if(any(! d$Big)) {
    dnb <- subset(d, ! Big)  # stratified estimates
    if(limspres)
      p <- if(grouppres && length(ugroup) == 2)
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

    dnb$sGroup <- paste0(dnb$Group, '\nby ',
                         gsub('Stratified by\n', '', nonbigtracename))
    ## Don't understand why if use ~ sGroup right below the symbols no
    ## longer appear in the legend for the non-stratified estimates
    p <- if(grouppres)
           plotly::add_markers(p, data=dnb,
                               x=~ X, y=~ Y,
                               color=~ Group,
                               text=~ Htext,
                               colors=cols,
                               marker=list(opacity=0.45, size=4),
                               hoverinfo='text')
#                               name=nonbigtracename)
         else
           plotly::add_markers(p, data=dnb,
                               x=~ X, y=~ Y,
                               text=~ Htext,
                               marker=list(opacity=0.45, size=4),
                               color=I('black'), hoverinfo='text',
                               name=nonbigtracename)

  }  # end if(any(! Big))

  leftmargin <- plotlyParm$lrmargin(ytnb)
  tickvals <- pretty(fun(xlim), n=10)
  xaxis <- list(title=xlab, range=xlim, zeroline=FALSE,
                tickvals=ifun(tickvals), ticktext=format(tickvals))
  yaxis <- list(title='',
                range=c(min(Y) - 0.2, 0.2),
                zeroline=FALSE, tickvals=yl, ticktext=yt)
  p <- plotly::layout(p,
                 xaxis=xaxis,
                 yaxis=yaxis,
                 margin=list(l=leftmargin),
                 legend=list(traceorder=if(length(difflower))
                                          'reversed' else 'normal'))

  attr(p, 'levelsRemoved') <- dropped
  p
  }
