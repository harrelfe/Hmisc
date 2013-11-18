latexTherm <- function(y, name, w=.075, h=.15, spacefactor=1/2, extra=.07,
                       file='', append=TRUE) {
  ct <- function(..., append=TRUE) cat(..., file=file, append=append, sep='')
  ct('\\def\\', name, '{\n', append=append)
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
        ct('\\put(', z(x + w/2), ', 0){\\line(0,1){', z(h) * b, '}}\n')
      }
    }
    x <- x + w + spacefactor * w
  }
  
  ct('\\end{picture}}\n')
}
