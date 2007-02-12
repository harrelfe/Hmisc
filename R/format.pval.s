## Use R function for S-Plus, just changed to .Options
format.pval <- function (x, pv=x, digits = max(1, .Options$digits - 2),
                         eps = .Machine$double.eps, 
                         na.form = "NA", ...) {
  if ((has.na <- any(ina <- is.na(pv)))) 
    pv <- pv[!ina]
    
  r <- character(length(is0 <- pv < eps))
  if (any(!is0)) {
    rr <- pv <- pv[!is0]
    expo <- floor(log10(ifelse(pv > 0, pv, 1e-50)))
    fixp <- expo >= -3 | (expo == -4 & digits > 1)
    if (any(fixp)) 
      rr[fixp] <- format(round(pv[fixp], digits = digits),
                         ...)
    if (any(!fixp)) 
      rr[!fixp] <- format(round(pv[!fixp], digits = digits),
                          ...)
    r[!is0] <- rr
  }
    
  if (any(is0)) {
    digits <- max(1, digits - 2)
    if (any(!is0)) {
      nc <- max(nchar(rr))
      if (digits > 1 && digits + 6 > nc) 
        digits <- max(1, nc - 7)
      sep <- if (digits == 1 && nc <= 6) 
        ""
      else " "
    }
    else sep <- if(digits == 1) 
      ""
    else " "
    
    r[is0] <- paste("<", format(eps, digits = digits, ...), sep = sep)
  }
  if (has.na) {
    rok <- r
    r <- character(length(ina))
    r[!ina] <- rok
    r[ina] <- na.form
  }
  r
}
