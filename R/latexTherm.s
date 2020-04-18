latexTherm <- function(y, name, w=.075, h=.15, spacefactor=1/2, extra=.07,
                       file='', append=TRUE) {
  ct <- function(..., append=TRUE) cat(..., file=file, append=append, sep='')
  ct('\\def\\', name, '{\n', append=append)
  tab <- attr(y, 'table')
  if(length(tab)) {
    ct('\\protect\\tooltipn{\n')
  }
  ct('\\setlength{\\unitlength}{.001in}\n')
  k <- length(y)
  W <- k * w + (k-1) * spacefactor * w
  z <- function(a) round(a * 1000)
  ct('\\begin{picture}(', z(W + extra), ',', z(h + extra), ')\n')
  x <- 0
  for(i in 1 : k) {
    b <- y[i]
    if(! is.na(b)) { 
      if(b < 1) {  # Draw frame if not completely filled
        ct('\\linethickness{.05pt}\n')
        ct('\\put(', z(x),     ', 0){\\line(1, 0){', z(w), '}}\n')
        ct('\\put(', z(x + w), ', 0){\\line(0, 1){', z(h), '}}\n')
        ct('\\put(', z(x + w), ',', z(h), '){\\line(-1, 0){', z(w), '}}\n')
        ct('\\put(', z(x),     ',', z(h), '){\\line(0, -1){', z(h), '}}\n')
      }
      if(b > 0) {
        ct('\\linethickness{', w, 'in}\n')
        ct('\\put(', z(x + w / 2), ', 0){\\line(0,1){', z(h * b), '}}\n')
      }
    }
    x <- x + w + spacefactor * w
  }
  
  ct('\\end{picture}',
     if(length(tab)) '}{\n',
     tab,
     if(length(tab)) '}',
     '}\n')
}

latexNeedle <- function(y, x=NULL, col='black', href=0.5, name, w=.05, h=.15,
                        extra=0, file='', append=TRUE) {
  ct <- function(..., append=TRUE) cat(..., file=file, append=append, sep='')
  ct('\\def\\', name, '{%\n', append=append)
  tab <- attr(y, 'table')
  if(length(tab)) {
    ct('\\protect\\tooltipn{%\n')
  }
  ct('\\setlength{\\unitlength}{.001in}%\n')
  k <- length(y)
  col <- rep(col, length.out=k)
  W <- max(k, 2) * w
  z <- function(a) round(a * 1000)
  ct('\\begin{picture}(', z(W + extra), ',', z(h), ')%\n')

  ## Draw grayscale frame
  ct('\\linethickness{.05pt}\\color[gray]{0.85}%\n')
  ct('\\put(0,0){\\line(1,0){', z(W), '}}%\n')
  # ct('\\put(', z(W), ',0){\\line(0,1){', z(h), '}}%\n')
  ct('\\put(', z(W), ',', z(h), '){\\line(-1,0){', z(W), '}}%\n')
  # ct('\\put(0,', z(h), '){\\line(0,-1){', z(h), '}}%\n')

  ## Draw horizontal reference lines
  if(length(href)) for(hr in href)
    ct('\\put(0,', z(h * hr), '){\\line(1,0){', z(W), '}}%\n')

  ## Draw vertical needles
  ## If x is given, scale to w / 2 to k * w / 2
  x <- if(length(x)) {
    r <- range(x)
    w / 2 + (k - 1) * w / 2 * (x - r[1]) / diff(r)
    } else seq(w / 2, k * w / 2, length.out=k)

  ct('\\linethickness{1.55pt}%\n')
  for(i in 1 : k) {
    b <- y[i]
    if(! is.na(b)) { 
      co <- paste(round(col2rgb(col[i]) / 255, 3), collapse=',')
      ct('\\color[rgb]{', co, '}')
      ct('\\put(', z(x[i]), ',0){\\line(0,1){', z(h * b), '}}%\n')
    }
  }

  ct('\\end{picture}',
     if(length(tab)) '}{%\n',
     tab,
     if(length(tab)) '}',
     '}%\n')
}

pngNeedle <- function(y, x=NULL, col='black', href=0.5, lwd=3.5, w=6, h=18,
                      file=tempfile(fileext='.png')) {

  k <- length(y)
  col <- rep(col, length.out=k)

  png(file, width=1 + k * w, height=h)
  par(mar=rep(0,4))
  plot.new()
  par(usr=c(0, 1, 0, 1))
  if(length(href)) {
    href <- c(0, href, 1)
    abline(h=href, col=gray(0.8))
    }

  ## If x is given, scale to [0.025, 0.975]
  x <- if(length(x)) {
    r <- range(x)
    0.025 + 0.95 * (x - r[1]) / diff(r)
    } else seq(0.025, 0.975, length.out=k)
  for(i in 1 : k) lines(c(x[i], x[i]), c(0, y[i]), col=col[i], lwd=lwd)
  dev.off()
  invisible(file)
  }
