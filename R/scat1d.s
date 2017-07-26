## Improvements due to Martin Maechler <maechler@stat.math.ethz.ch>

scat1d <- function(x, side=3, frac=.02, jitfrac=.008, tfrac, 
                   eps=ifelse(preserve,0,.001),
                   lwd=0.1, col=par('col'), y=NULL, curve=NULL,
                   bottom.align=FALSE, preserve=FALSE, fill=1/3, limit=TRUE, 
                   nhistSpike=2000, nint=100, 
                   type=c('proportion','count','density'),
                   grid=FALSE,
                   ...)
{
  type <- match.arg(type)
  if(length(x) >= nhistSpike)
    return(histSpike(x, side=side, type=type,
                     frac=2.5 * frac, col=col, y=y, curve=curve,
                     bottom.align=if(type == 'density') TRUE else bottom.align, 
                     add=TRUE, nint=nint, grid=grid, ...))

  gfun <- ordGridFun(grid)
  if(side == 1 || side == 3 || length(y) || length(curve)) {
    l <- 1:2
    ax <- 1
  } else {
    l <- 3:4
    ax <- 2
  }
  
  pr <- parGrid(grid)
  usr <- pr$usr; pin <- pr$pin; uin <- pr$uin
  
  u     <- usr[  l]
  u.opp <- usr[- l]
  w     <- u[2] - u[1]
  ## Start JOA 12.8.97 : handle xy missings parallel
  if (length(y) > 1) { ## length=1 special case needed for datadensity
    if (length(x) != length(y))
      stop("y must have same length as x (or length(y)=1)")

    selector <- ! (is.na(x) | is.na(y))
    x <- unclass(x[selector])
    y <- unclass(y[selector])
  } else x <- unclass(x[! is.na(x)])
  ## Stop JOA 12.8.97
  
  if(length(curve))
    y <- approx(curve, xout=x, rule=2)$y

  n <- length(x)
  if(missing(tfrac))
    tfrac <- if(n < 125) 1 else max(.1, 125 / n)
  else if (tfrac < 0 || tfrac > 1)
    stop("must have  0 <= tfrac <= 1")

  ## Start JOA 19.8.97
  if(jitfrac > 0 && anyDuplicated( if(eps > 0) round(x / w / eps) else x ))
    if (preserve)
      x <- jitter2(x, fill=fill, limit=limit, eps=w * eps)
    else
      ## Stop JOA 19.8.97
      x <- x + runif(n, -w * jitfrac, w * jitfrac)
  h <- min(pin) * frac / uin[- ax]
  h2 <- h / 2
  if(grid && length(y) && inherits(y, 'unit')) {
    h  <- unit(frac,   'npc')
    h2 <- unit(frac/2, 'npc')
  }
  if(length(y)) {
    a <- y - h2
    b <- y + h2
  } else {
    a <- if(side < 3) u.opp[1]
    else u.opp[2] - h
    b <- if(side < 3) u.opp[1] + h
    else u.opp[2]
  }
  if(tfrac < 1) {
    l <- tfrac * (b - a)
    a <- a + runif(n) * (b - l - a)   ##runif(n, a, b-l) if frac>0
    b <- a + l
  }
  if(ax == 1 && bottom.align) {
    a <- a + h2
    b <- b + h2
  }
  if(ax == 1)
    gfun$segments(x, a, x, b, lwd=lwd, xpd=frac < 0, col=col)
  else
    gfun$segments(a, x, b, x, lwd=lwd, xpd=frac < 0, col=col)
  invisible()
}


jitter2 <- function(x,...) UseMethod("jitter2")

jitter2.default <- function(x, fill=1/3, limit=TRUE, eps=0,
                            presorted=FALSE, ...)
{ 
  x2 <- x[!is.na(x)]
  if (!presorted){
    o <- order(x2)
    x2 <- x2[o]
  }

  r <- if (eps > 0) rle(round(x2 / eps) * eps) else rle(x2)

  if ( length(r$length) < 2 || max(r$length) < 2 )
    return(x)

  d <- abs(diff(r$values))
  d <- pmin( c(d[1], d), c(d, d[length(d)]) )
  who <- rep(r$lengths > 1, r$lengths)
  d <- d[r$lengths > 1] * fill / 2
  if (is.logical(limit) && limit) limit <- min(d)

  if (limit) d <- pmin(d,limit)

  r$values  <- r$values[r$lengths > 1] - d
  r$lengths <- r$lengths[r$lengths > 1]
  d <- d * 2 / (r$lengths - 1)
  k <- length(r$lengths)
  n <- sum(who)
  val <- rep(r$values, r$lengths)
  add <- (0 : (n - 1)) - rep(c(0, cumsum(r$lengths[-k])), r$lengths)
  add <- add[order(rep(1 : k, r$lengths), runif(n))]
  add <- add * rep(d, r$lengths)
  val <- val + add
  x2[who] <- val
  if (!presorted)
    x2[o]<-x2

  x[!is.na(x)] <- x2
  x
}


jitter2.data.frame <- function(x, ...)
{
  as.data.frame(lapply(x,
                       function(z,...)
                       {
                         if (is.numeric(z))
                           jitter2.default(z,...)
                         else z
                       },
                       ...))
}


datadensity <- function(object, ...)
{
  if(!length(class(object)))
    class(object) <- data.class(object)
  
  UseMethod('datadensity')
}

datadensity.data.frame <-
  function(object, group,
           which=c('all','continuous','categorical'),
           method.cat=c('bar','freq'),
           col.group=1:10,
           n.unique=10, show.na=TRUE, nint=1, naxes,
           q, bottom.align=nint > 1,
           cex.axis=sc(.5,.3), cex.var=sc(.8,.3),
           lmgp=NULL,   tck=sc(-.009,-.002),
           ranges=NULL, labels=NULL, ...)
{
  which <- match.arg(which)
  method.cat <- match.arg(method.cat)
  maxna <- 0
  mgroup <- missing(group)  # before R changes it

  z <-
    sapply(object,
           function(x, n.unique)
           {
             xp <- x[!is.na(x)]
             nu <- if(length(xp)) length(unique(xp))
                   else 0

             if(nu < 2) c(0,0)
             else
               c(type=if(is.factor(x) || is.character(x) || nu < n.unique)
                        1
                      else 2,
                 na=sum(is.na(x)))
           },
           n.unique=n.unique)

  types <- c('nil','cat','cont')[z[1,]+1]
  numna <- z[2,]
  fnumna <- format(numna)
  maxna <- max(numna)

  w <- switch(which,
              all        = types != 'nil',
              continuous = types == 'cont',
              categorical= types == 'cat')

  if(missing(naxes)) naxes <- sum(w)

  ## Function to scale values such that when naxes<=3 get hi, >=50 get
  ## lo, otherwise linearly interpolate between 3 and 50
  sc <- function(hi,lo,naxes)
    approx(c(50,3),c(lo,hi),xout=naxes,rule=2)$y

  formals(sc) <- list(hi=NA,lo=NA,naxes=naxes)
  nams <- names(object)
  max.length.name <- max(nchar(nams))

  if(!length(lmgp))
    lmgp <- sc(0,0)

  oldpar <- oPar()  # in Hmisc Misc.s
  mgp  <- c(0,lmgp,0)

  mai  <- oldpar$mai
  plot.new();
  par(new=TRUE)
  ## enables strwidth

  mxlb <-  .1 + max(strwidth(nams, units='inches', cex=cex.var))
  mai[2] <- mxlb
  if(!show.na) maxna <- 0
  max.digits.na <- if(maxna == 0) 0
                   else trunc(log10(maxna)) + 1

  if(maxna > 0)
    mai[4] <- .1 + strwidth('Missing', units='inches', cex=cex.var)

  par(mgp=mgp, mai=mai,tck=tck)
  on.exit(setParNro(oldpar))

  if(!mgroup)
    group <- as.factor(group)
  else
    {
      group <- factor(rep(1,length(object[[1]])))
      ngroup <- 0
    }

  ngroup <- length(levels(group))
  col.group <- rep(col.group, length=ngroup)

  y <- 0
  for(i in (1:length(nams))[w])
    {
      if(y < 1)
        {
          plot(c(0,1),c(1,naxes),xlim=c(.02,.98),ylim=c(1,naxes),
               xlab='',ylab='',type='n',axes=FALSE)
          usr <- par('usr')
          y <- naxes + 1
          if(maxna > 0)
            {
              outerText('Missing',
                        y=naxes+strheight('Missing',units='user',cex=cex.var),
                        cex=cex.var)
            }
          
          charheight <- strheight('X',units='user',cex=.6)  ## par('cxy')[2]
        }
      
      y <- y - 1
      x <- object[[i]]
      if(types[i] == 'cont' )
        {  ## continuous variable
          x <- unclass(x)         ## handles dates
          isna <- is.na(x)
          nna  <- sum(isna)
          N <- length(x) - nna
          r <-
            if(length(ranges) && length(ranges[[nams[i]]]))
              ranges[[nams[i]]]
            else
              range(x, na.rm=TRUE)
          
          p <- pretty(r,
                      if(nint == 1)5
                      else nint)
          
          if(nint < 2)
            p <- c(p[1],p[length(p)]) ##bug in pretty for nint=1

          xmin <- p[1]
          xmax <- p[length(p)]
          cex <- par(cex=cex.axis)  # Bug in R: cex= ignored in
                                        # axis( )

          axis(side=1, at=(p-xmin)/(xmax-xmin), labels=format(p),
               pos=y, cex=cex.axis)

          par(cex=cex)
          if(mgroup)
            scat1d((x-xmin)/(xmax-xmin), y=y, bottom.align=bottom.align, 
                   minf=.075, frac=sc(.02,.005), ...)
          else for(g in 1:ngroup)
            {
              j <- group == levels(group)[g]
              scat1d((x[j]-xmin)/(xmax-xmin), y=y, bottom.align=bottom.align,
                     col=col.group[g], tfrac=if(N<125) 1 else max(.1, 125/N), 
                     minf=.075, frac=sc(.02,.005), ...)
            }
          
          if(!missing(q))
            {
              quant <- quantile(x, probs=q, na.rm=nna>0)
              points((quant-xmin)/(xmax-xmin),
                     rep(y-.5*charheight,length(q)),
                     pch=17, cex=.6)
            }
        } else {  ## character or categorical or discrete numeric
          if(is.character(x)) x <- as.factor(x)
          isna <- is.na(x)
          nna <- sum(isna)
          
          if(length(group) != length(x)) {
            if(is.data.frame(object))
              stop('length of group must equal length of variables in data frame')

            group <- rep(1, length(x))
          }

          tab <- table(group,x)
          lev <- dimnames(tab)[[2]]
          nl  <- length(lev)
      if(is.numeric(x))
        {
          xx <- as.numeric(lev)
          xx <- (xx - min(xx)) / (max(xx) - min(xx))
        } else {
          if(sum(nchar(lev)) > 200) 
            lev <- substring(lev, 1, max(1, round(200 / length(lev))))

          xx <- (0 : (nl - 1)) / (nl - 1)
        }

          cex <- par(cex=cex.axis)
          axis(side=1, at=xx, labels=lev, pos=y, cex=cex.axis, tick=FALSE)
          par(cex=cex)
          
          lines(c(0,1), c(y,y))
          maxfreq <- max(tab)
          for(g in if(ngroup == 0) 1 else 1 : ngroup)
            {
              tabg <- tab[g,]
              if(method.cat == 'bar')
                symbols(xx, y + .4 * tabg / maxfreq / 2, add=TRUE,
                        rectangles=cbind(.02, .4 * tabg / maxfreq),
                        inches=FALSE,
                        col=col.group[g])
              else text(xx, rep(y + .1, nl), format(tabg),
                        cex=cex.axis * sqrt(tab / maxfreq),
                        adj=.5)
            }
        }
      
      mtext(if(length(labels))labels[i]
      else nams[i],
            2, 0, at = y, srt = 0, cex = cex.var, adj = 1, las=1)
    ## las=1 for R (also 3 lines down)

    if(show.na && nna > 0)
      outerText(fnumna[i], y, cex=cex.var)
    }
  
  invisible()
}


histSpike <-
  function(x, side=1, nint=100, frac=.05, minf=NULL, mult.width=1,
           type=c('proportion','count','density'),
           xlim=range(x),
           ylim=c(0,max(f)), xlab=deparse(substitute(x)), 
           ylab=switch(type, proportion='Proportion',
             count     ='Frequency',
             density   ='Density'),
           y=NULL, curve=NULL, add=FALSE, 
           bottom.align=type == 'density', 
           col=par('col'), lwd=par('lwd'), grid=FALSE, ...)
{
  type <- match.arg(type)
  if(! add && side != 1)
    stop('side must be 1 if add=FALSE')

  if(add && type == 'count')
    warning('type="count" is ignored if add=TRUE')

  if(length(y) > 1) {
    if(length(y) != length(x))
      stop('lengths of x and y must match')
    
    if(length(curve))
      warning('curve ignored when y specified')
    
    i <- !is.na(x + y)
    curve <- list(x=x[i], y=y[i])
  }
  
  if(length(curve) && !missing(bottom.align) && bottom.align)
    warning('bottom.align=T specified with curve or y; ignoring bottom.align')

  gfun <- ordGridFun(grid)
  x <- x[!is.na(x)]
  x <- x[x >= xlim[1] & x <= xlim[2]]
  
  if(type != 'density') {
    if(is.character(nint) || length(x) <= 10) {
      f <- table(x)
      x <- as.numeric(names(f))
    } else {
      ncut <- nint+1
      bins <- seq(xlim[1], xlim[2], length = ncut)
      delta <- (bins[2]-bins[1]) / 2
      f <- table(cut(x, c(bins[1] - delta, bins)))
      
      x <- bins
      j <- f > 0
      x <- x[j]
      f <- f[j]
    }
    
    if(type == 'proportion') f <- f / sum(f)
  } else {
    nbar <- logb(length(x), base = 2) + 1
    width <- diff(range(x)) / nbar * .75 * mult.width
    den <- density(x, width=width, n=200, from=xlim[1], to=xlim[2])
    x <- den$x
    f <- den$y
  }
  
  if(!add) {
    if(grid)
      stop('add=FALSE not implemented for lattice')
    
    plot(0, 0, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type='n')
  }
  
  if(side == 1 || side == 3) {
    l <- 1:2;
    ax <- 1
  } else {
    l <- 3:4;
    ax <- 2
  }
  
  f <- f / max(f)
  if(length(minf)) f <- pmax(f, minf)

  pr <- parGrid(grid)
  usr <- pr$usr;
  pin <- pr$pin;
  uin <- pr$uin
  
  u <- usr[l]
  u.opp <- usr[- l]
  
  h <- min(pin) * frac / uin[- ax] * f
  h2 <- h / 2
  if(length(y) && inherits(y, 'unit')) {
    h  <- unit(frac,     'npc')
    h2 <- unit(frac / 2, 'npc')
  }
  if(length(curve) || length(y)) {
    if(length(curve))
      y <- approx(curve, xout=x, rule=2)$y

    a <- y - h2; b <- y + h2
  } else {
    a <- if(side < 3) u.opp[1]
    else u.opp[2] - h
    
    b <- if(side < 3) u.opp[1] + h
    else u.opp[2]
  }
  
  if(ax == 1 && bottom.align && type != 'density') {
    a <- a + h2
    b <- b + h2
  }
  
  if(type == 'density') {
    lll <- gfun$lines
    ## Problem in S+ getting right value of lwd
    if(ax == 1)
      do.call('lll',list(x,
                         if(side == 1)b
                         else a,
                         lwd=lwd,  col=col))
    else
      do.call('lll',list(if(side == 2)b
      else a,
                         x, lwd=lwd, col=col))
  } else {
    lll <- gfun$segments
    if(ax == 1)
      do.call('lll',list(x, a, x, b, lwd=lwd, xpd=frac < 0, col=col))
    else
      do.call('lll',list(a, x, b, x, lwd=lwd, xpd=frac < 0, col=col))
  }
  
  invisible(xlim)
}

if(FALSE)
  histSpikep <- function(p, x, y, z, group=NULL, color=NULL, hovertext=NULL,
                       colors=NULL,
                       bottom.align=TRUE, tracename='Proportion', ...) {

  d <- data.frame(x=rep(x, each=3),
                  y=rep(y, each=3),
                  z=rep(z, each=3))
  origcolor <- color

  if(length(group))     d$group     <- rep(group,     each=3)
  if(length(hovertext)) d$hovertext <- rep(hovertext, each=3)
  if(length(color))     d$color     <- rep(color,     each=3)
  
  n <- nrow(d)
  j <- seq(1, n, by=3)
  if(length(hovertext)) d$hovertext[j] <- ''
  if(! bottom.align)    d$y[j] <- d$y[j] - d$z[j] / 2
  
  j <- seq(3, n, by=3)
  d$x[j] <- NA
  if(length(hovertext)) d$hovertext[j] <- ''
  
  j <- seq(2, n, by=3)
  d$y[j] <- d$y[j] + d$z[j] / ifelse(bottom.align, 1, 2)

  plotly::plot_ly(d, x=~ x, y=~ y, mode='lines', type='scatter',
#                  color=d$color,# colors=colors,
                  line=list(color=d$color, width=1.4),  # ...
                  text=~ hovertext,
                  hoverinfo=if(length(hovertext)) 'text' else 'none')

#  plotly::add_trace(p, data=d, x=x, y=y, mode='lines',
#                    color=color, colors=colors,
#                    line=list(..., width=1.4),
#                    text=hovertext,
#                    hoverinfo=if(length(hovertext)) 'text' else 'none',
#                    evaluate=TRUE, name=tracename)
}

histboxp <- function(p=plotly::plot_ly(height=height),
                     x, group=NULL, xlab=NULL,
                     gmd=TRUE, sd=FALSE, bins=100, wmax=190, mult=7) {

  if(! length(xlab)) xlab <- label(x, html=TRUE, plot=TRUE,
                                   default=deparse(substitute(x)))

  if(! length(group)) group <- rep(1, length(x))
  if(length(x) != length(group)) stop('x and group must be same length')

  distinct <- unique(x)
  distinct <- distinct[! is.na(distinct)]
  xmin     <- min(distinct)
  xr       <- x

  ## Still do slight rounding if < bins distinct values because
  ## values extremely close to each other won't show otherwise
  if(length(distinct) > bins ||
     min(diff(sort(distinct))) < range(distinct) / (5 * bins)) {
    pret <- pretty(x, if(length(distinct) > bins) bins else 5 * bins)
    dist <- pret[2] - pret[1]
    r    <- range(pret)
    xr   <- r[1] + dist * round((x - r[1]) / dist)
  }

  mu <- markupSpecs$html
  
  y <- 0
  dh <- dm <- dq1 <- dq2 <- dq3 <- dgmd <- dsd <- levs <- NULL

  group <- as.factor(group)
  mis   <- is.na(x)
  for(g in levels(group)) {
    i <- group == g
    miss <- sum(mis[i])
    if(miss > 0) i <- i & ! mis
    if(! any(i)) next
    levs <- c(levs, g)
    u     <- x[i]
    ur    <- xr[i]
    tab   <- as.data.frame(table(ur))
    z     <- as.numeric(as.character(tab$ur))
    prop  <- tab$Freq / length(ur)
    y <- y - 1
    dh <- rbind(dh, data.frame(x=z, prop=prop, freq=tab$Freq,
                      txt=paste0(format(z), '<br>', round(prop, 3),
                                 '<br>n=', tab$Freq),
                      y=y))

    dm <- rbind(dm, data.frame(Mean=mean(u), n=length(u), miss=miss, y=y))

    if(gmd) {
      Gmd <- GiniMd(u)
      dgmd <- rbind(dgmd, data.frame(Gmd, x=xmin,
                                     txt=paste0('Gini mean difference:',
                                                format(Gmd, digits=5)),
                                     y=y))
    }

    if(sd) {
      Sd  <- sd(u)
      dsd <- rbind(dsd, data.frame(sd=Sd, x=xmin,
                                   txt=paste0('SD:', format(Sd, digits=5)),
                                   y=y))
      }

    probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
    qu  <- quantile(u, probs)
    nam <- paste0('Q', mu$sub(probs))
    txt <- paste0(nam, ':', format(qu, digits=5))
    dq1 <- rbind(dq1, data.frame(Median=qu[3], txt=txt[3], y=y))
    dq2 <- rbind(dq2, data.frame(quartiles=qu[c(2,4)], txt=txt[c(2,4)], y=y))
    dq3 <- rbind(dq3, data.frame(outer=qu[c(1,5)], txt=txt[c(1,5)], y=y))
  }

  ng <- length(levs)
  height <- plotlyParm$heightDotchart(ng) + 50 * (gmd & sd)

  dh$prop <- 0.8 * dh$prop / max(dh$prop)
  p <- plotly::add_segments(p, data=dh,
                            x = ~ x,
                            y = ~ y,
                            xend = ~ x,
                            yend = ~ y + prop,
                            text = ~ txt,
                            hoverinfo = 'text',
                            color = I('black'),
                            name  = 'Histogram')

  dm$txt <- with(dm, paste0('Mean:', format(Mean, digits=5), '<br>n=', n,
                            '<br>', miss, ' missing'))

  yoff <- 0.09

  p <- plotly::add_markers(p, data=dm, mode='markers', color=I('black'),
                           x = ~ Mean, y = ~ y - yoff,
                           text = ~ txt,
                           hoverinfo = 'text',
                           name = 'Mean')

  p <- plotly::add_markers(p, mode='markers', data=dq1,
                           x = ~ Median,
                           y = ~ y - yoff,
                           text = ~ txt,
                           hoverinfo = 'text',
                           marker = list(symbol='line-ns-open',
                                         color='black', size=8),
                           name = 'Median')
  
  p <- plotly::add_markers(p, mode='markers', data=dq2,
                           x = ~ quartiles,
                           y = ~ y - yoff,
                           text = ~ txt,
                           hoverinfo = 'text',
                           marker = list(symbol='line-ns-open',
                                         color='blue', size=6),
                           name = 'Quartiles')
  
  p <- plotly::add_markers(p, mode='markers', data=dq3,
                           x = ~ outer,
                           y = ~ y - yoff,
                           text = ~ txt,
                           hoverinfo = 'text',
                           marker = list(symbol='line-ns-open',
                                         color='red', size=4),
                           name = '0.05, 0.95<br>Quantiles')

  if(gmd)
    p <- plotly::add_segments(p, data=dgmd,
                              x = ~ x,
                              y = ~ y - 0.19,
                              xend = ~ x + Gmd,
                              yend = ~ y - 0.19,
                              text = ~ txt,
                              hoverinfo = 'text',
                              color = I('light gray'),
                              name = paste0('Gini ',
                                       mu$overbar(paste0('|',
                                                    htmlGreek('Delta'), '|'))),
                              visible='legendonly')
                              
  if(sd)
    p <- plotly::add_segments(p, data=dsd,
                              x = ~ x,
                              y = ~ y - 0.23,
                              xend = ~ x + sd,
                              yend = ~ y - 0.23,
                              text = ~ txt,
                              hoverinfo = 'text',
                              color = I('light blue'),
                              name = 'SD',
                              visible='legendonly')

  p <- plotly::layout(p,
                      margin = list(l=plotlyParm$lrmargin(levs,
                                                          wmax=wmax, mult=mult)),
                      xaxis = list(title=xlab, zeroline=FALSE),
                      yaxis = list(title='',
                                   tickvals= - (1 : ng),
                                   ticktext = levs))
  p
}
