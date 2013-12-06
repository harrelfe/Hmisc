## event.chart.q: eventchart program 1.0 (creates function event.chart)
##
## last edited: 9-27-97
## last edited: 10-20-98, add pty='m' for the default plotting;
##      one may change to pty='s' to get the 'square plot' for the Goldman's Event Chart
## FEH changes 9may02 for R

event.chart <-
  function(data, subset.r = 1:dim(data)[1], subset.c = 1:dim(data)[2],

           sort.by = NA, sort.ascending = TRUE,
           sort.na.last = TRUE, sort.after.subset = TRUE,
           y.var = NA, y.var.type = "n",
           y.jitter = FALSE, y.jitter.factor = 1,
           y.renum = FALSE, NA.rm = FALSE, x.reference = NA,
           now = max(data[, subset.c], na.rm = TRUE),
           now.line = FALSE, now.line.lty = 2,
           now.line.lwd = 1, now.line.col = 1, pty = "m",
           date.orig = c(1, 1, 1960), titl = "Event Chart",

           y.idlabels = NA, y.axis = "auto",
           y.axis.custom.at = NA, y.axis.custom.labels = NA,
           y.julian = FALSE, y.lim.extend = c(0, 0),
           y.lab = ifelse(is.na(y.idlabels), "", as.character(y.idlabels)),

           x.axis.all = TRUE, x.axis = "auto",
           x.axis.custom.at = NA, x.axis.custom.labels = NA,
           x.julian = FALSE, x.lim.extend = c(0, 0), x.scale = 1,
           x.lab = ifelse(x.julian, "Follow-up Time", "Study Date"),

           line.by = NA, line.lty = 1, line.lwd = 1, line.col = 1,
           line.add = NA, line.add.lty = NA,
           line.add.lwd = NA, line.add.col = NA,
           point.pch = 1:length(subset.c),
           point.cex = rep(0.6, length(subset.c)),
           point.col = rep(1, length(subset.c)),

           point.cex.mult = 1., point.cex.mult.var = NA,
           extra.points.no.mult = rep(NA, length(subset.c)),

           legend.plot = FALSE, legend.location = "o", legend.titl = titl,
           legend.titl.cex = 3, legend.titl.line = 1,
           legend.point.at = list(x = c(5, 95), y = c(95, 30)),
           legend.point.pch = point.pch,
           legend.point.text = ifelse(rep(is.data.frame(data), length(subset.c)),
                                      names(data[, subset.c]),
                                      subset.c),
           legend.cex = 2.5, legend.bty = "n",
           legend.line.at = list(x = c(5, 95), y = c(20, 5)),
           legend.line.text = names(table(as.character(data[, line.by]),
                                          exclude = c("", "NA"))),
           legend.line.lwd = line.lwd, legend.loc.num = 1,

           ...)
{
  legnd <- function(..., pch) {
    if(missing(pch)) legend(...)
    else legend(..., pch = pch)
  }

  month.day.year <- function(jul, origin.) {
    if (missing(origin.) || is.null(origin.)) {
      origin. <- .Options$chron.origin
      if (is.null(origin.))
        origin. <- c(month = 1, day = 1, year = 1960)
    }
    
    shift <- if (all(origin. == 0)) 0 else julian(origin = origin.)
    
    ## relative origin
    ## "absolute" origin
    j <- jul + shift
    j <- j - 1721119
    y <- (4 * j - 1) %/% 146097
    j <- 4 * j - 1 - 146097 * y
    d <- j %/% 4
    j <- (4 * d + 3) %/% 1461
    d <- 4 * d + 3 - 1461 * j
    d <- (d + 4) %/% 4
    m <- (5 * d - 3) %/% 153
    d <- 5 * d - 3 - 153 * m
    d <- (d + 5) %/% 5
    y <- 100 * y + j
    y <- y + ifelse(m < 10, 0, 1)
    m <- m + ifelse(m < 10, 3, -9)
    list(month = m, day = d, year = y)
  }

  ## julian.r
  ## Convert between Julian and Calendar Dates

  julian <- function(m, d, y, origin.) {
    only.origin <- all(missing(m), missing(d), missing(y))
    if (only.origin)
      m <- d <- y <- NULL
    
    ## return days since origin
    if (missing(origin.) || is.null(origin.)) {
      origin. <- .Options$chron.origin
      if (is.null(origin.))
        origin. <- c(month = 1, day = 1, year = 1960)
    }
    
    nms <- names(d)
    max.len <- max(length(m), length(d), length(y))
    
    ## prepend new origin value and rep out to common max. length:
    m <- c(origin.[1], rep(m, length = max.len))
    d <- c(origin.[2], rep(d, length = max.len))
    y <- c(origin.[3], rep(y, length = max.len))
    
    ## code from julian date in the S book (p.269)
    y <- y + ifelse(m > 2, 0, -1)
    m <- m + ifelse(m > 2, -3, 9)
    c <- y %/% 100
    ya <- y - 100 * c
    out <- (146097 * c) %/% 4 + (1461 * ya) %/% 4 + (153 * m + 2) %/% 5 +
      d + 1721119
    
    ## now subtract the new origin from all dates
    if (!only.origin) {
      if (all(origin. == 0))
        out <- out[-1]
      else
        out <- out[-1] - out[1]
    }
    
    names(out) <- nms
    out
  }

  ## stop function if unacceptable violations occur
  ##   (other stops may occur later)
  if (!is.matrix(data) && !is.data.frame(data))
    stop("argument data must be a matrix or a data frame\n")

  ## section 1: do necessary subsetting and sorting of data
  targodata <- apply(data[, subset.c, drop = FALSE], 2, as.numeric)

  ## targodata for target 'overall' data
  if (!is.na(x.reference))
    targodata <- apply(targodata - data[, x.reference], 2, as.numeric)

  ## start of sort routine
  if (!is.na(sort.by[1])) {
    if (sort.after.subset == TRUE)
      data <- data[subset.r, ]

    m <- dim(data)[1]
    keys <- 1:m
    rotate <- m:1
    length.sort.by <- length(sort.by)
    asc <- rep(sort.ascending, length.sort.by)
    for (i in length.sort.by:1) {
      if (asc[i])
        keys[] <- keys[sort.list(data[, sort.by[[i]]][keys],
                                 na.last = sort.na.last)]
      else
        keys[] <- keys[order(data[, sort.by[[i]]][keys],
                             rotate, na.last = sort.na.last)[rotate]]
    }

    data <- data[keys, ]

    if (sort.after.subset == FALSE) {
      subset.r <- (1:dim(data)[1])[subset.r]
      targdata <- apply(data[subset.r, subset.c, drop = FALSE], 2, as.numeric)
    } else if (sort.after.subset == TRUE) {
      targdata <- apply(data[, subset.c, drop = FALSE], 2, as.numeric)
      subset.ro <- (1:dim(data)[1])[subset.r]
      subset.r <- seq(length(subset.ro))
    }
  } else if (is.na(sort.by[1])) {
    subset.r <- (1:dim(data)[1])[subset.r]
    targdata <- apply(data[subset.r, subset.c, drop = FALSE], 2, as.numeric)
  }

  ## end of sort routine

  ## start to deal with missing values and renumbering y-axis
  if (NA.rm == TRUE) {
    whotoplot <- subset.r[!(apply(is.na(targdata), 1, all))]
    ## whotoplot is for use for data matrix(dataframe);
    ##  essentially who will be plotted from data

    t.whotoplot <- seq(dim(targdata)[1])[!(apply(is.na(targdata), 1, all))]
    ## t.whotoplot is for use for targdata matrix(dataframe);
    ##  essentially, who will be plotted from targdata

    if (y.renum == TRUE) {
      whattoplot <- seq(subset.r[!(apply(is.na(targdata), 1, all))])
      ## whattoplot is what will be plotted on y-axis of event chart
    } else if (y.renum == FALSE) {
      if ((!is.na(sort.by[1]) & sort.after.subset == FALSE) | (is.na(sort.by[1])))
        whattoplot <- subset.r[!(apply(is.na(targdata), 1, all))]
      else if (!is.na(sort.by[1]) & sort.after.subset == TRUE)
        whattoplot <- subset.ro[!(apply(is.na(targdata), 1, all))]
    }
  } else if (NA.rm == FALSE) {
    whotoplot <- subset.r
    t.whotoplot <- seq(dim(targdata)[1])
    if (y.renum == TRUE)
      whattoplot <- seq(subset.r)
    else if (y.renum == FALSE) {
      if ((!is.na(sort.by[1]) & sort.after.subset == FALSE) | (is.na(sort.by[1])))
        whattoplot <- subset.r
      else if (!is.na(sort.by[1]) & sort.after.subset == TRUE)
        whattoplot <- subset.ro
    }
  }
  ## end of dealing with missing values and renumbering of y-axis


  ## section 2:  perform necessary manipulations using x.reference and y.var

  ## deal with re-referencing x-axis with x.reference
  if (!is.na(x.reference)) {
    targdata <- apply(targdata - data[subset.r, x.reference], 2, as.numeric)
    if (NA.rm == TRUE) {
      x.referencew <- data[whotoplot, x.reference]
      whotoplot <- whotoplot[!is.na(x.referencew)]
      t.whotoplot <- t.whotoplot[!is.na(x.referencew)]
      whattoplot.ref <- whattoplot[!is.na(x.referencew)]
      if (y.renum == FALSE) {
        if ((!is.na(sort.by[1]) & sort.after.subset == FALSE) | (is.na(sort.by[1])))
          whattoplot <- seq(subset.r[1],
                            subset.r[1] + length(whattoplot.ref) - 1)
        else if (!is.na(sort.by[1]) & sort.after.subset == TRUE)
          whattoplot <- seq(subset.ro[1],
                            subset.ro[1] + length(whattoplot.ref) - 1)
      } else if (y.renum == TRUE)
        whattoplot <- seq(length(whattoplot.ref))
    }
  }

  ## deal with using a data frame variable to place lines on y-axis
  if (!is.na(y.var)) {
    if (!is.na(sort.by[1]))
      stop("cannot use sort.by and y.var simultaneously\n")

    y.varw <- as.numeric(data[whotoplot, y.var])
    whotoplot <- whotoplot[!is.na(y.varw)]
    t.whotoplot <- t.whotoplot[!is.na(y.varw)]
    whattoplot <- y.varw[!is.na(y.varw)]
    if (y.jitter == TRUE) {
      range.data <- diff(range(whattoplot))
      range.unif <- y.jitter.factor *
                    (range.data / (2 * (length(whattoplot) - 1)))
      whattoplot <- whattoplot +
                    runif(length(whattoplot), -(range.unif), range.unif)
    }
  }

  sort.what <- sort(whattoplot)
  length.what <- length(whattoplot)

  ## section 3:	perform all plotting

  ## first, make sure length of point.pch, cex, col is same as subset.c

  len.c <- length(subset.c)

  if (length(point.pch) < len.c) {
    warning("length(point.pch) < length(subset.c)")
    point.pch <- rep(point.pch, len.c)[1:len.c]
  }

  if (length(point.cex) < len.c) {
    warning("length(point.cex) < length(subset.c)")
    point.cex <- rep(point.cex, len.c)[1:len.c]
  }

  if (length(point.col) < len.c) {
    warning("length(point.col) < length(subset.c)")
    point.col <- rep(point.col, len.c)[1:len.c]
  }

  ## set default of par(new = FALSE)
  par(new = FALSE)

  ## plot external legend (if requested)
  if (legend.plot == TRUE && legend.location == "o") {
    plot(1, 1, type = "n", xlim = c(0, 100), ylim = c(0, 100),
         axes = FALSE, xlab = "", ylab = "")
    mtext(legend.titl, line = legend.titl.line, outer = FALSE,
          cex = legend.titl.cex)
    legnd(legend.point.at[[1]], legend.point.at[[2]],
          leg = legend.point.text,
          pch = legend.point.pch, cex = legend.cex,
          col = point.col, bty = legend.bty)
    if (!is.na(line.by)) {
      par(new = TRUE)
      legnd(legend.line.at[[1]], legend.line.at[[2]],
            leg = legend.line.text, cex = legend.cex,
            lty = line.lty, lwd = legend.line.lwd,
            col = line.col, bty = legend.bty)
    }

    invisible(par(ask = TRUE))
  }

  ## start creating objects to be used in determining plot region

  targdata <- targdata / x.scale
  targodata <- targodata / x.scale

  minvec <- apply(targdata[t.whotoplot,, drop = FALSE], 1, min, na.rm = TRUE)
  minotime <- ifelse(x.axis.all,
                     min(apply(targodata, 1, min, na.rm = TRUE), na.rm = TRUE),
                     min(minvec, na.rm = TRUE))
  maxvec <- apply(targdata[t.whotoplot,, drop = FALSE], 1, max, na.rm = TRUE)
  maxotime <- ifelse(x.axis.all,
                     max(apply(targodata, 1, max, na.rm = TRUE), na.rm = TRUE),
                     max(maxvec, na.rm = TRUE))

  ## determine par parameters and plot graphical region based
  ##  on request of y.var and, subsequently, y.var.type and now.line

  y.axis.top <- sort.what[length.what] + y.lim.extend[2]
  y.axis.bottom <- sort.what[1] - y.lim.extend[1]
  x.axis.right <- maxotime + x.lim.extend[2]
  x.axis.left <- minotime - x.lim.extend[1]

  if (!is.na(y.var) & y.var.type == "d") {
    oldpar <- par(omi = rep(0, 4), lwd = 0.6,
                  mgp = c(3.05, 1.1, 0), tck = -0.006, ...)
    ## set pty
    par(pty = pty)
    plot(whattoplot, type = "n",
         xlim = c(x.axis.left,
                  ifelse(now.line,
                         (now - (min(data[, subset.c], na.rm = TRUE))) / x.scale,
                         x.axis.right)),
         ylim = c(y.axis.bottom,
                  ifelse(pty == "s",
                         now,
                         y.axis.top)),
         xlab = x.lab, ylab = y.lab, axes = FALSE)
    if (now.line == TRUE)
      abline(now, ((sort.what[1] - now) /
                   (((now - min(data[, subset.c], na.rm = TRUE)) / x.scale) - minotime)),
             lty = now.line.lty, lwd = now.line.lwd, col = now.line.col)
  } else if (is.na(y.var) | (!is.na(y.var) & y.var.type == "n")) {
    if (now.line == TRUE)
      stop("with now.line == TRUE, y.var & y.var.type == 'd' must be specified\n")

    oldpar <- par(omi = rep(0, 4), lwd = 0.6,
                  mgp = c(2.8, 1.1, 0), tck = -0.006, ...)
    plot(whattoplot, type = "n",
         xlim = c(x.axis.left, x.axis.right),
         ylim = c(y.axis.bottom - 1, y.axis.top + 1),
         xlab = x.lab, ylab = y.lab, axes = FALSE)
  }

  ## plot y-axis labels

  if (!is.na(y.idlabels)) {
    if (!is.na(y.var)) {
      warning("y.idlabels not used when y.var has been specified\n")
      axis(side = 2)
    } else if (is.na(y.var))
      axis(side = 2, at = whattoplot,
           labels = as.vector(data[whotoplot, y.idlabels]))
  } else if (is.na(y.idlabels)) {
    if (y.axis == "auto") {
      if (is.na(y.var) | (!is.na(y.var) & y.var.type == "n"))
        axis(side = 2)
      else if (!is.na(y.var) & y.var.type == "d") {
        if (y.julian == FALSE) {
          y.axis.auto.now.bottom <- ifelse(now.line, sort.what[1],
                                           y.axis.bottom)
          ## marked by JJL, disable square plot
          ##y.axis.auto.now.top <- ifelse(now.line, now, y.axis.top)
          y.axis.auto.now.top <- ifelse(now.line, y.axis.top, y.axis.top)
          y.axis.auto.at <- round(seq(y.axis.auto.now.bottom,
                                      y.axis.auto.now.top, length = 5))
          y.axis.auto.labels <-
            paste(month.day.year(y.axis.auto.at, origin = date.orig)$month, "/",
                  month.day.year(y.axis.auto.at, origin = date.orig)$day, "/",
                  substring(month.day.year(y.axis.auto.at,
                                           origin = date.orig)$year, 3, 4),
                  sep = "")
          axis(side = 2, at = y.axis.auto.at, labels = y.axis.auto.labels)
        } else if (y.julian == TRUE)
          axis(side = 2)
      }
    } else if (y.axis == "custom") {
      if (is.na(y.axis.custom.at[1]) || is.na(y.axis.custom.labels[1]))
        stop("with y.axis == 'custom', must specify y.axis.custom.at and y.axis.custom.labels\n")

      axis(side = 2, at = y.axis.custom.at, labels = y.axis.custom.labels)
    }
  }

  ## plot x-axis labels

  if (x.axis == "auto") {
    if (x.julian == FALSE) {
      x.axis.auto.at <-
        round(seq(x.axis.left, x.axis.right, length = 5))
      x.axis.auto.labels <-
        paste(month.day.year(x.axis.auto.at, origin = date.orig)$month, "/",
              month.day.year(x.axis.auto.at, origin = date.orig)$day, "/",
              substring(month.day.year(x.axis.auto.at,
                                       origin = date.orig)$year, 3, 4),
              sep = "")
      axis(side = 1, at = x.axis.auto.at, labels = x.axis.auto.labels)
    } else if (x.julian == TRUE)
      axis(side = 1)
  } else if (x.axis == "custom") {
    if (is.na(x.axis.custom.at[1]) || is.na(x.axis.custom.labels[1]))
      stop("with x.axis = 'custom', user must specify x.axis.custom.at and x.axis.custom.labels\n")

    axis(side = 1, at = x.axis.custom.at, labels = x.axis.custom.labels)
  }

  if (!is.na(titl)) {
    title(titl)
  }

  ## plot lines and points

  if (!is.na(line.by)) {
    line.byw <- data[whotoplot, line.by]
    table.by <- table(as.character(line.byw), exclude = c("", "NA"))
    names.by <- names(table.by)
    len.by <- length(table.by)
    if (length(line.lty) < len.by)
      warning("user provided length(line.lty) < num. of line.by categories")

    if (length(line.lwd) < len.by)
      warning("user provided length(line.lwd) < num. of line.by categories")

    if (length(line.col) < len.by)
      warning("user provided length(line.col) < num. of line.by categories")

    line.lty <- rep(line.lty, len = len.by)
    line.lwd <- rep(line.lwd, len = len.by)
    line.col <- rep(line.col, len = len.by)
    lbt.whotoplot <-
      (1:(length(t.whotoplot)))[as.character(line.byw) != "" &
                                as.character(line.byw) != "NA"]
    for (i in lbt.whotoplot) {
      lines(c(minvec[i], maxvec[i]), rep(whattoplot[i], 2),
            lty = as.vector(line.lty[names.by == line.byw[i]]),
            lwd = as.vector(line.lwd[names.by == line.byw[i]]),
            col = as.vector(line.col[names.by == line.byw[i]]))
    }
  } else if (is.na(line.by)) {
    for (i in 1:length(t.whotoplot))
      lines(c(minvec[i], maxvec[i]), rep(whattoplot[i], 2),
            lty = line.lty[1], lwd = line.lwd[1], col = line.col[1])
  }

  if(is.na(point.cex.mult.var[1]))
    for(j in 1:dim(targdata)[2])
      points(as.vector(unlist(targdata[t.whotoplot, j])), whattoplot,
             pch = point.pch[j], cex = point.cex[j], col = point.col[j])
  else {
    ## loop only for extra points
    for(j in which(!is.na(extra.points.no.mult)))
      points(as.vector(unlist(targdata[t.whotoplot, j])), whattoplot,
             pch = point.pch[j], cex = point.cex[j], col = point.col[j])
    ## loop for points to magnify based on level of covariates
    for(i in 1:length(t.whotoplot)) {
      k <- 0
      for(j in which(is.na(extra.points.no.mult))) {
        k <- k + 1
        points(as.vector(unlist(targdata[t.whotoplot[i], j])),
               whattoplot[i], pch = point.pch[j],
               cex = ifelse(is.na(data[whotoplot[i], point.cex.mult.var[k]]),
                            point.cex[j],
                            point.cex.mult * data[whotoplot[i], point.cex.mult.var[k]] * point.cex[j]),
               col = point.col[j])
      }
    }
  }

  ## add line.add segments (if requested)

  if (!is.na(as.vector(line.add)[1])) {
    if (any(is.na(line.add.lty)))
      stop("line.add.lty can not have missing value(s) with non-missing line.add\n")

    if (any(is.na(line.add.lwd)))
      stop("line.add.lwd can not have missing value(s) with non-missing line.add\n")

    if (any(is.na(line.add.col)))
      stop("line.add.col can not have missing value(s) with non-missing line.add\n")

    line.add.m <- as.matrix(line.add)
    dim.m <- dim(line.add.m)

    if (dim.m[1] != 2)
      stop("line.add must be a matrix with two rows\n")

    if (length(line.add.lty) != dim.m[2])
      stop("length of line.add.lty must be the same as number of columns in line.add\n")

    if (length(line.add.lwd) != dim.m[2])
      stop("length of line.add.lwd must be the same as number of columns in line.add\n")

    if (length(line.add.col) != dim.m[2])
      stop("length of line.add.col must be the same as number of columns in line.add\n")

    for (j in (1:dim.m[2])) {
      for (i in (1:length(t.whotoplot))) {
        add.var1 <- subset.c == line.add.m[1, j]
        if (!any(add.var1))
          stop("variables chosen in line.add must also be in subset.c\n")
        
        add.var2 <- subset.c == line.add.m[2, j]
        if (!any(add.var2))
          stop("variables chosen in line.add must also be in subset.c\n")

        segments(targdata[i, (1:len.c)[add.var1]], whattoplot[i],
                 targdata[i, (1:len.c)[add.var2]], whattoplot[i],
                 lty = line.add.lty[j], lwd = line.add.lwd[j],
                 col = line.add.col[j])
      }
    }
  }

  ## plot internal legend (if requested)

  if (legend.plot == TRUE & legend.location != "o") {
    if (legend.location == "i") {
      legnd(legend.point.at[[1]], legend.point.at[[2]],
            leg = legend.point.text,
            pch = legend.point.pch, cex = legend.cex,
            col = point.col, bty = legend.bty)
      if (!is.na(line.by))
        legnd(legend.line.at[[1]], legend.line.at[[2]],
              leg = legend.line.text, cex = legend.cex,
              lty = line.lty, lwd = legend.line.lwd,
              col = line.col, bty = legend.bty)
    } else if (legend.location == "l") {
      cat("Please click at desired location to place legend for points.\n")
      legnd(locator(legend.loc.num), leg = legend.point.text,
            pch = legend.point.pch, cex = legend.cex,
            col = point.col, bty = legend.bty)
      if (!is.na(line.by)) {
        cat("Please click at desired location to place legend for lines.\n")
        legnd(locator(legend.loc.num), leg = legend.line.text,
              cex = legend.cex, lty = line.lty,
              lwd = legend.line.lwd, col = line.col, bty = legend.bty)
      }
    }
  }

  ## add box to main plot and clean up

  invisible(box())
  invisible(par(ask = FALSE))
  par(oldpar)
}


## event.convert.s
## convert 2-column coded events to multiple event time for event.chart()
## input: a matrix or dataframe with at least 2 columns
##        by default, the first column contains the event time and
##                    the second column contains the k event codes (e.g. 1=dead, 0=censord)
## ouput: a matrix of k columns, each column contains the time of kth coded event
##
event.convert <- function(data2, event.time = 1, event.code = 2)
{
  dim.d <- dim(data2)
  len.t <- length(event.time)
  if(len.t != length(event.code))
    stop("length of event.time and event.code must be the same")

  if(any(event.time > dim.d[2]))
    stop(paste("Column(s) in event.time cannot be greater than ", dim.d[2]))

  if(any(event.code > dim.d[2]))
    stop(paste("Column(s) in event.code cannot be greater than ",
    dim.d[2]))

  name.data <- names(data2)[event.time]
  if(is.null(name.data)) {
    name.data <- paste("V", event.time, sep = "")
  }

  n.level <- rep(NA, len.t)
  for (i in (1:len.t)) {
    n.level[i] <- length(table(data2[, event.code[i]]))
  }

  tot.col <- sum(n.level)
  data.out <- matrix(NA, dim.d[1], tot.col)
  name.col <- rep(NA, tot.col)
  n.col <- 1
  for (i in (1:len.t)) {
    tab.d <- table(data2[, event.code[i]])
    if(is.null(class(data2[, event.code[i]])))
      level.value <- as.numeric(names(tab.d))
    else
      level.value <- names(tab.d)

    for (j in (1:length(tab.d))) {
      data.out[, n.col] <- rep(NA, dim.d[1])
      check <- data2[, event.code[i]] == level.value[j]
      check[is.na(check)] <- FALSE
      data.out[, n.col][data2[, event.code[i]] == level.value[j]] <-
        data2[, event.time[i]][check]
      name.col[n.col] <-
        paste(name.data[i], ".", names(tab.d)[j], sep = "")
      n.col <- n.col + 1
    }
  }
  dimnames(data.out) <- list(1:dim.d[1], name.col)
  return(as.matrix(data.out))
}
