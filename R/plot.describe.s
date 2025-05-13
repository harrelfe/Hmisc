plot.describe <- function(x, which=c('both', 'continuous', 'categorical'),
                          what=NULL, sort=c('ascending', 'descending', 'none'),
                          n.unique=10, digits=5, bvspace=2, ...) {

  which <- match.arg(which)
  pty <- grType() == 'plotly' && requireNamespace("plotly")
  auto <- .Options$plotlyauto
  auto <- length(auto) && auto
  
  if(length(what)) x <- x[what]

  sort <- match.arg(sort)

  specs <- if(pty) markupSpecs$html else markupSpecs$plain

  if(bvspace == 1) stop('bvspace may not be 1.0')

  format_counts <- function(s) {
    bl <- '                                     '
    na  <- substring(paste(names(s), bl, sep=''), 1,
                     max(nchar(names(s))))
    va  <- paste(substring(bl, 1,
                           max(nchar(s)) - nchar(s) + 1), s, sep='')
    zz  <- paste(na, va, sep=':')
    gsub(' ', specs$space, zz)
  }

  fmtlab <- function(x) {
    lab <- sub('^.*:', '', x$descript)
    if(length(x$units)) lab <- specs$varlabel(lab, x$units)
    lab
  }

  ge <- function(i) unlist(lapply(w, function(x) x[[i]]))

  if(which != 'continuous') {
    f <- function(x) {
      s <- x$counts
      v <- x$values
      type <- if('Sum' %in% names(s)) 'binary'
              else
                if(length(v) && is.list(v) &&
                   all(names(v) == c('value', 'frequency')) &&
                   is.character(v$value) && length(v$value) <= 20) 'categorical'
              else 'none'
                                                 
      if(type == 'none')
        return(data.frame(prop=numeric(0), text=character(0)))
      n    <- as.numeric(s['n'])
      if(type == 'binary') {
        val  <- ''
        freq <- as.numeric(s['Sum'])
      }
      else {
        val  <- v$value
        freq <- v$frequency
      }
      category  <- if(type == 'categorical') val else ''
      text <- 
        paste(category, if(type == 'categorical') ' ',
              round(freq / n, 3), specs$frac(freq, n, size=90))

      ## Details about variable for minimum frequency category
      left <- which.min(freq)[1]
      text[left] <- paste(c(fmtlab(x), text[left], format_counts(s)),
                          collapse=specs$br)

      y <- if(type == 'binary') 0 else 1 : length(freq)
      y <- c(bvspace, rep(1, length(freq) - 1))
      ## extra 1 for between-variable spacing

      j <- switch(sort,
                  ascending = order(freq),
                  descending= order(-freq),
                  none      = TRUE)
      
      data.frame(prop=freq[j] / n, y=y, text=I(text[j]),
                 category=I(category[j]),
                 missing=as.numeric(s['missing']), nlev=length(freq))
    }

    w <- lapply(x, f)
    nam <- names(w)
    
    for(na in nam) {
      l <- length(w[[na]]$prop)
      w[[na]]$xname <- if(l) rep(na, l) else character(0)
    }

    if(length(ge('xname')) == 0) {
      warning('no categorical variables found')
      pcat <- NULL
    } else {
 
      z <- data.frame(xname      = I(ge('xname')),
                      Proportion = ge('prop'),
                      y          = ge('y'),
                      text       = I(ge('text')),
                      category   = I(ge('category')),
                      Missing    = ge('missing'),
                      nlev       = ge('nlev'))
      
      un <- unique(z$xname)
      nv  <- length(un)
      z$xnamef <- factor(z$xname, levels=un)
      z$xnamen <- as.integer(z$xnamef) - z$y / pmax(0.7 * nv, z$nlev) 
      
      z$cumy <- cumsum(z$y)
      if(! pty) z$cumy <- - z$cumy

      tly <- z$cumy[z$y == bvspace]

      if(! pty) {
        r <- range(z$Proportion)
        z$proplev <- r[2] + .2
        pcat <-
          if(any(z$Missing > 0))
            ggplot(z, aes(text=text)) +
              geom_point(aes(x=Proportion, y=cumy, color=Missing)) +
              geom_text(aes(x=proplev, y=cumy, label=category),
                        size=2.5, hjust=1) +
              scale_y_continuous(breaks=tly, labels=un) +
              scale_x_continuous(breaks=pretty(r)) +
              scale_color_gradientn(colors=viridisLite::viridis(10)) +
              ylab(NULL) +
              theme(panel.grid.minor.y = element_blank())
          else
            ggplot(z, aes(text=text)) + geom_point(aes(x=Proportion, y=cumy)) +
              geom_text(aes(x=proplev, y=cumy, label=category),
                        size=2.5, hjust=1) +
              scale_y_continuous(breaks=tly, labels=un) +
              scale_x_continuous(breaks=pretty(r)) +
              ylab(NULL) +
              theme(panel.grid.minor.y = element_blank())
      }
      else
      {
        z$proplev <- 1.15
        pcat <- if(any(z$Missing > 0))
                plotly::plot_ly(z, x = ~ Proportion, y= ~ cumy, text= ~ text,
                             color=~ Missing, mode='markers',
                             hoverinfo='text',
                             type='scatter', name='',
                             height=if(! auto) plotlyParm$heightDotchart(nrow(z)))
           else
             plotly::plot_ly(z, x=~ Proportion, y=~ cumy, text=~ text,
                             mode='markers', hoverinfo='text',
                             type='scatter', name='',
                             height=if(! auto) plotlyParm$heightDotchart(nrow(z)))
      
        pcat <-
          plotly::add_trace(pcat,
                            data=z, x=~ proplev, y=~ cumy, text=~ category,
                            mode='text', textposition='left',
                            textfont=list(size=9), hoverinfo='none',
                            name='Levels')
      
      tl <- seq(0, 1, by=0.05)
      ## X tick mark labels for multiples of 0.1
      tt <- ifelse(tl %% 0.1 == 0, as.character(tl), '')
      tly <- z$cumy[z$y == bvspace]
      
      pcat <- plotly::layout(pcat,
                             xaxis=list(range=c(0,1.15), zeroline=TRUE,
                                        tickvals=tl, ticktext=tt,
                                        title='Proportion'),
                             yaxis=list(title='', autorange='reversed',
                                        tickvals=tly, ticktext=un),
                             margin=list(l=plotlyParm$lrmargin(un)))
      }
    }
  }

  if(which != 'categorical') {
    f <- function(x) {
      s <- x$counts
      v <- x$values
      isn <- function(z) is.numeric(z) || testDateTime(z, 'either')
      if(! (as.numeric(s['distinct']) >= n.unique &&
            length(v) && is.list(v) &&
            all(names(v) == c('value', 'frequency')) &&
            isn(v$value)) )
        return(data.frame(X=numeric(0), count=numeric(0), text=character(0)))
      X  <- v$value
      Y  <- v$frequency
      Xn <- as.numeric(X)
      
      text <- paste(format(X, digits=digits), ' (n=', Y, ')', sep='')
      X    <- (Xn - Xn[1]) / diff(range(Xn))
      zz   <- format_counts(s)
      lab  <- fmtlab(x)
      
      text[1] <- paste(c(lab, text[1], zz), collapse='<br>')
      ## Note: plotly does not allow html tables as hover text
      m <- rep(as.numeric(s['missing']), length(X))
      list(X=X, prop=Y / sum(Y), text=I(text), missing=m)
    }
    
    w <- lapply(x, f)
    nam <- names(w)
    for(n in nam) {
      l <- length(w[[n]]$X)
      w[[n]]$xname <- if(l) rep(n, l) else character(0)
    }
    
    if(length(ge('xname')) == 0) {
      warning('no continuous variables found')
      pcon <- NULL
    } else {
    
      z <- data.frame(xname      = I(ge('xname')),
                      X          = ge('X'),
                      Proportion = round(ge('prop'), 4),
                      text       = I(ge('text')),
                      Missing    = ge('missing'))
      z <- z[nrow(z) : 1, ]   # so plotly will keep right ordering
      unam <- unique(z$xname)
      z$yy <- match(as.character(z$xname), unam)
      ## Scale Proportion so that max over all variables is 0.9
      z$Proportion <- 0.9 * z$Proportion / max(z$Proportion)
      g <- if(any(z$Missing > 0))
             ggplot(z, aes(text=text)) +
               geom_segment(aes(x=X, xend=X, y=yy, yend=yy + Proportion,
                                color=Missing)) +
               scale_y_continuous(breaks=1 : length(unam), labels=unam) +
               scale_color_gradientn(colors=viridisLite::viridis(10)) +
               xlab(NULL) + ylab(NULL) +
               theme(axis.text.x = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     axis.ticks.x = element_blank())
           else
             ggplot(z, aes(text=text)) +
               geom_segment(aes(x=X, xend=X, y=yy, yend=yy + Proportion)) + 
               scale_y_continuous(breaks=1 : length(unam), labels=unam) +
               xlab(NULL) + ylab(NULL) +
               theme(axis.text.x = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     axis.ticks.x = element_blank())

      ## ggplotly would not hover text at x=0 when height < 350 px
      curtail <- function(x) min(1000, max(x, 350))
      pcon <- if(! pty) g
              else
                plotly::ggplotly(g, tooltip='text', width=if(! auto) 800,
                               height=if(! auto) curtail(60 + 25 * length(unam)))

## If don't run plot_ly, hovering will pop up all vertical points
#      pcon <- if(any(z$missing > 0))
#                plotly::plot_ly(z, x=X, y=yy, mode='markers',
#                                color=missing, colors=colors, name='')
#              else
#                plotly::plot_ly(z, x=X, y=yy, mode='none', name='')
#
#      pcon <- with(z, histSpikep(NULL, x=X, y=yy, z=Proportion,
#                                 hovertext=text, tracename='',
#                                 color=if(any(missing > 0)) missing,
#                                 colors=colors))
                                 
## Note: plotly does not allow font, color, size changes for hover text
#      pcon <- if(any(z$missing > 0))
#             plotly::plot_ly(z, x=X, y=xname, size=Proportion, text=text,
#                             color=missing, mode='markers',
#                             marker=list(symbol='line-ns-open'),
#                             type='scatter', hoverinfo='text')
#           else
#                   plotly::plot_ly(z, x=X, y=xname, size=Proportion,
#                                   text=text,
#                                   mode='markers',
#                                   marker=list(symbol='line-ns-open'),
#                                   type='scatter', hoverinfo='text')
#
#      maxlen <- max(nchar(as.character(unam)))
#      pcon <- plotly::layout(pcon, xaxis=list(title='', showticklabels=FALSE,
#                                              zeroline=FALSE, showgrid=FALSE),
#                             yaxis=list(title='',
#                                        tickvals=1 : length(unam),
#                                        ticktext=unam),
#                             margin=list(l=max(70, maxlen * 6)),
#                             autosize=TRUE, evaluate=TRUE)
      }
  }

  if(which == 'both') {
    if(! length(pcat)) return(pcon)
    if(! length(pcon)) return(pcat)
    }

  ## knitr.in.progress not set to TRUE unless explicitly select menu
  ## choice knit to html.  Even when properly set, resulting html
  ## file was messed up as plotly widgets were duplicated

  switch(which,
         categorical = pcat,
         continuous  = pcon,
         both        = if(FALSE && pty &&
                          isTRUE(getOption('knitr.in.progress')))
                         htmltools::tagList(list(plotly::as.widget(pcat),
                                                 plotly::as.widget(pcon)))
                       else
                         list(Categorical=pcat, Continuous=pcon))
}

## Some of these are for dotchart3.s
utils::globalVariables(c('X', 'Proportion', 'xname', 'cumy', 'proplev',
                         'category', 'xb', 'Missing', 'yy'))
