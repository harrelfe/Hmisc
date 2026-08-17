#' Enhanced Dot Chart Rendered as Typst Markup
#'
#' \code{typstDotchart} is a Typst-markup translation of
#' \code{\link{latexDotchart}}, itself a translation of
#' \code{\link{dotchart3}}. It produces a character string of Typst
#' markup (a single \verb{#box(...)} containing a sequence of
#' \verb{#place()} calls) that visually mimics \code{dotchart3}'s
#' output, for use in Typst/Quarto documents in place of a raster image.
#' As with \code{latexDotchart}, the \code{add} and \code{horizontal=FALSE}
#' options available in \code{dotchart3} are not supported here.
#'
#' \code{typstDotchart} reuses \code{latexDotchart}'s coordinate
#' computation, sorting, and margin logic essentially unchanged -- none
#' of that is LaTeX-specific. Only the drawing primitives differ:
#'
#' \itemize{
#'   \item Text (\code{\\put(x,y){\\makebox(...)[just]{s}}}) becomes a
#'     call to a small shared Typst helper, \code{justified-text}
#'     (expected to already be defined in the document via
#'     \code{typstFunctions$justifiedText}), which uses Typst's
#'     \code{measure()} function to compute the rendered width/height of
#'     \code{s} and offset the placement accordingly -- the Typst
#'     equivalent of \code{\\makebox}'s own text-width-aware
#'     justification.
#'   \item Lines (\code{\\put(x,y){\\line(dx,dy){len}}}) become
#'     \verb{#place(dx:, dy:, line(length:, angle:))} calls, with color
#'     passed directly as \code{line()}'s \code{stroke:} argument rather
#'     than LaTeX's stateful \verb{\\color{...} ... \\color{black}}
#'     wrapping -- Typst needs no such state to be opened and closed.
#'   \item Dots (\code{\\put(x,y){\\circle*{d}}}) become
#'     \verb{#place(dx:, dy:, circle(radius:, fill: black))} calls. Since
#'     \code{#place()} anchors the \emph{top-left} of a shape's bounding
#'     box rather than its center, both \code{dx} and \code{dy} are
#'     offset by \code{-radius} so the dot is actually centered on its
#'     target point.
#' }
#'
#' Because Typst's \code{#place()} measures \code{dy} from the
#' \emph{top} of its container downward, while LaTeX's \code{picture}
#' environment measures \code{y} from the \emph{bottom} upward, the
#' internal y-coordinate function is wrapped in a single top/bottom flip
#' (\code{yt <- function(y) h - yt0(y)}) so every other line of
#' coordinate logic below it can be used completely unchanged from
#' \code{latexDotchart}.
#'
#' The gap between x-axis tick marks and their labels was originally a
#' fixed \code{0.15} inch offset from the axis line, with tick marks
#' \code{0.05} inches long -- an effective gap of \code{0.10} inches
#' between the tick's far edge and the label. That gap has been reduced
#' by 40\% here (to \code{0.06} inches), giving a total label offset of
#' \code{0.05 + 0.06 = 0.11} inches, confirmed by direct visual
#' comparison against the original spacing.
#'
#' \code{size}, \code{size.labels}, and \code{size.group.labels} keep
#' \code{latexDotchart}'s original LaTeX font-size-command-name interface
#' (e.g. \code{'small'}, \code{'normalsize'}, \code{'large'}) for
#' familiarity, translated internally to approximate point sizes via a
#' fixed lookup table based on standard LaTeX class default point sizes.
#' This is an approximation -- actual LaTeX point sizes depend on the
#' base document font size -- not a byte-exact equivalence.
#'
#' \strong{This is a first-draft port.} The individual drawing
#' primitives (text justification via \code{measure()}, dot centering,
#' horizontal/vertical lines, direct color arguments, the tick-label gap)
#' have each been confirmed by standalone compile tests. The full
#' function, exercised end to end on real grouped/aux-data input the way
#' \code{latexDotchart}'s own examples do, has not yet been compile-tested
#' at that scale.
#'
#' @param data A numeric vector whose values are shown on the x-axis.
#' @param labels A vector of labels for each point, corresponding to
#'   \code{data}. If omitted, \code{names(data)} are used, and if there
#'   are no names, integers prefixed by \code{"#"} are used.
#' @param groups An optional categorical variable indicating how
#'   \code{data} values are grouped.
#' @param gdata Data values for groups, typically summaries such as
#'   group medians.
#' @param xlab X-axis title.
#' @param auxdata A vector of auxiliary data, the same length as
#'   \code{data}. If present, printed outside the right margin of the
#'   chart -- usually cell sizes.
#' @param auxgdata Similar to \code{auxdata} but corresponding to
#'   \code{gdata}.
#' @param auxtitle If \code{auxdata} is given, a column heading for it
#'   (e.g. \code{"N"}).
#' @param w Width of the chart, in inches.
#' @param h Height of the chart, in inches.
#' @param margin A 4-vector, in inches: margin to the left of the x-axis,
#'   below the y-axis, to the right of the x-axis, and above the y-axis.
#'   By default computed automatically based on label/auxdata widths.
#' @param lines Set to \code{FALSE} to suppress the horizontal reference
#'   lines.
#' @param dotsize Diameter of the filled dots, in inches.
#' @param size Text size for the main chart text, as a LaTeX font-size
#'   command name (see Details).
#' @param size.labels Text size for row labels.
#' @param size.group.labels Text size for group labels.
#' @param ttlabels Set to \code{TRUE} to render row labels in a
#'   monospace font.
#' @param sort. Set to \code{FALSE} to keep the input order rather than
#'   sorting by \code{data} value.
#' @param xaxis Set to \code{FALSE} to suppress the x-axis.
#' @param lcolor Color for the horizontal reference lines. Default
#'   \code{"gray"}.
#' @param ... Ignored.
#'
#' @return A single character string of Typst markup -- one
#'   \verb{#box(...)} containing the full sequence of placed drawing
#'   commands -- suitable for passing to \code{\link{typstAsis}} (if it
#'   is the sole/final content being emitted) or for concatenating into
#'   a larger character vector alongside other Typst content.
#'
#' @seealso \code{\link{latexDotchart}}, \code{\link[Hmisc]{dotchart3}},
#'   \code{\link{typstTranslate}}
#'
#' @examples
#' \dontrun{
#' z <- typstDotchart(c(.1, .2), c('a', 'bbAAb'), xlab = 'This Label',
#'                    auxdata = c(.1, .2), auxtitle = 'Zcriteria')
#' typstAsis(z)
#' }
#'
#' @export
typstDotchart <-
  function(data, labels, groups = NULL, gdata = NA,
           xlab = "", auxdata, auxgdata=NULL, auxtitle,
           w=4, h=4, margin, lines = TRUE, dotsize = .075, size='small',
           size.labels = 'small', size.group.labels = 'normalsize',
           ttlabels = FALSE, sort.=TRUE, xaxis=TRUE, lcolor='gray',
           ...)
{
  ## Approximate LaTeX font-size-command point sizes (10pt base class
  ## assumed) -- see Details.
  texSizePt <- c(tiny=6, scriptsize=7, footnotesize=8, small=9,
                normalsize=10, large=12, Large=14, LARGE=17,
                huge=20, Huge=25)
  toPt <- function(s) { p <- unname(texSizePt[s]); if(is.na(p)) 10 else p }

  ptsize      <- toPt(size)
  ptlabels    <- toPt(size.labels)
  ptgrouplabs <- toPt(size.group.labels)
  if(size.labels == size) ptlabels <- NULL
  if(size.group.labels == size) ptgrouplabs <- NULL

  ## Typst analog of the original txt() closure. Relies on
  ## justified-text already being defined in the document via
  ## typstFunctions$justifiedText.
  typst_text <- function(x, y, s, ptsz=NULL, just=c('c','l','r'), tt=FALSE) {
    just <- match.arg(just)
    s <- typstTranslate(s)
    n <- max(length(x), length(y), length(s))
    x <- rep(x, length.out=n); y <- rep(y, length.out=n)
    s <- rep(s, length.out=n)
    z <- character(n)
    for(i in 1:n) {
      body <- s[i]
      if(tt) body <- paste0('#raw("', gsub('"', '\\\\"', body, fixed=TRUE), '")')
      if(length(ptsz)) body <- paste0('#text(size:', ptsz, 'pt)[', body, ']')
      z[i] <- sprintf('#justified-text(%gin, %gin, [%s], justify: "%s")',
                      x[i], y[i], body, just)
    }
    z
  }

  ## Typst analog of the original ln() closure. Color is passed directly
  ## to line()'s stroke: argument -- no LaTeX-style \color{}...\color{}
  ## state needed.
  typst_line <- function(x1, y1, x2, y2, color='black') {
    n <- max(length(x1), length(x2), length(y1), length(y2))
    x1 <- rep(x1, length.out=n); y1 <- rep(y1, length.out=n)
    x2 <- rep(x2, length.out=n); y2 <- rep(y2, length.out=n)
    z <- character(n)
    for(i in 1:n) {
      stroke <- if(color == 'black') '' else
                paste0(', stroke: ', if(color == 'gray') 'gray' else color)
      z[i] <-
        if(x1[i] == x2[i])
          sprintf('#place(dx: %gin, dy: %gin, line(length: %gin, angle: 90deg%s))',
                  x1[i], min(y1[i], y2[i]), abs(y2[i]-y1[i]), stroke)
        else if(y1[i] == y2[i])
          sprintf('#place(dx: %gin, dy: %gin, line(length: %gin, angle: 0deg%s))',
                  min(x1[i], x2[i]), y1[i], abs(x2[i]-x1[i]), stroke)
        else
          ## Diagonal: not produced by any call site below, kept for
          ## completeness/future use -- NOT compile-tested
          sprintf('#place(dx: %gin, dy: %gin, line(end: (%gin, %gin)%s))',
                  x1[i], y1[i], x2[i]-x1[i], y2[i]-y1[i], stroke)
    }
    z
  }

  ## Typst analog of \circle*{d}. #place() anchors the top-left of the
  ## circle's bounding box, not its center, so both coordinates are
  ## offset by -radius -- confirmed necessary by direct compile test.
  typst_dot <- function(x, y, diam) {
    r <- diam / 2
    sprintf('#place(dx: %gin, dy: %gin, circle(radius: %gin, fill: black))',
            x - r, y - r, r)
  }

  acl <- function(s) 0.09 * max(nchar(s))

  z <- character(0)

  ndata <- length(data)
  if(missing(labels)) {
    if(length(names(data)))
      labels <- names(data)
    else labels <- paste("#", seq(along = ndata))
  }
  else labels <- rep(as.character(labels), length = ndata)

  if(missing(groups)) {
    glabels <- NULL
    gdata <- NULL
    if(sort.) {
      ord <- order(-data)
      data <- data[ord]
      labels  <- labels[ord]
      if(! missing(auxdata)) auxdata <- auxdata[ord]
    }
  } else {
    if(! sort.) {
      ug <- unique(as.character(groups))
      groups <- factor(as.character(groups), levels=ug)
    }
    groups  <- unclass(groups)
    glabels <- levels(groups)
    gdata   <- rep(gdata, length = length(glabels))
    ord     <- if(sort.) order(groups, -data) else
                         order(groups, seq(along = groups))
    groups  <- groups[ord]
    data    <- data[ord]
    labels  <- labels[ord]
    if(! missing(auxdata)) auxdata <- auxdata[ord]
  }

  alldat <- c(data, gdata)
  if(! missing(auxdata)) auxdata <- format(c(auxdata, auxgdata))

  alllab <- c(labels, glabels)

  xl <- range(p <- pretty(alldat))
  yl <- c(1, length(alldat))

  if(missing(margin))
    margin <- c(acl(alllab),
               ifelse(xlab == '', .2, .4),
               ifelse(missing(auxdata), 0, acl(auxdata)),
               ifelse(missing(auxtitle), 0, .1))

  xt  <- function(x) round((w - sum(margin[c(1,3)]))*(x - xl[1])/diff(xl) +
                           margin[1], 5)
  yt0 <- function(y) round((h - sum(margin[c(2,4)]))*(y - yl[1])/diff(yl) +
                           margin[2], 5)
  ## Flip: Typst's dy increases downward; LaTeX's picture y increases
  ## upward. Wrapping the flip here once means every line below can be
  ## used exactly as in latexDotchart, with no per-call adjustment.
  yt <- function(y) h - yt0(y)

  if(xaxis) {
    ## Tick labels: 0.11in offset from the axis (0.05in tick length +
    ## 0.06in gap, a 40% reduction from the original 0.10in gap) -- see
    ## Details.
    z <- c(z, typst_text(xt(p), yt(yl[1]) + 0.11, as.character(p),
                         ptsz=ptlabels, just='c'))
    z <- c(z, typst_line(xt(p), yt(yl[1]), xt(p), yt(yl[1]) + 0.05))
    if(xlab != '')
      z <- c(z, typst_text(xt(xl[1] + diff(xl)/2), h - 0.1, xlab, ptsz=ptsize))
  }

  z <- c(z, typst_line(xt(xl), yt(yl[1]), xt(xl), yt(yl[2])),
            typst_line(xt(xl[1]), yt(yl), xt(xl[2]), yt(yl)))

  den <- ndata + 2 * length(glabels) + 1
  delt <- ( - (yl[2] - yl[1]))/den
  ypos <- seq(yl[2], by = delt, length.out= ndata)

  if(! missing(groups)) {
    ypos1 <- ypos + 2 * delt * (if(length(groups)>1)
                                  cumsum(c(1, diff(groups) > 0))
                                else 1)
    diff2 <- c(3 * delt, diff(ypos1))
    ypos2 <- ypos1[abs(diff2 - 3 * delt) < abs(0.001 * delt)] -
      delt
    ypos <- c(ypos1, ypos2) - delt
  }

  ypos <- ypos + delt
  nongrp <- 1:ndata

  if(lines)
    z <- c(z, typst_line(xt(xl[1]), yt(ypos[nongrp]), xt(xl[2]), yt(ypos[nongrp]),
                         color=lcolor))

  for(i in seq(along = alldat))
    if(! is.na(alldat[i] + ypos[i]))
      z <- c(z, typst_dot(xt(alldat[i]), yt(ypos[i]), dotsize))

  if(! missing(auxdata)) {
    z <- c(z, typst_text(w - 0.02, yt(ypos[nongrp]), auxdata,
                         ptsz=ptlabels, just='r'))
    if(! missing(auxtitle))
      z <- c(z, typst_text(w - 0.02, yt(yl[2]) - 0.1, auxtitle,
                           ptsz=ptlabels, just='r'))
  }
  labng <- alllab[nongrp]
  yposng <- ypos[nongrp]

  z <- c(z, typst_text(margin[1] - 0.05, yt(yposng), labng,
                       ptsz=ptlabels, just='r', tt=ttlabels))
  if(! missing(groups))
    z <- c(z, typst_text(margin[1] - 0.05, yt(ypos[-nongrp]), alllab[-nongrp],
                         ptsz=ptgrouplabs, just='r'))

  paste0(
    typstFunctions$justifiedText, "\n",
    "#box(width: ", w, "in, height: ", h, "in)[\n",
    paste(z, collapse = "\n"),
    "\n]"
  )
}
