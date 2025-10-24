num.intercepts <- function(fit, type=c('fit', 'var', 'coef'))
{
  type <- match.arg(type)
  nrp <- fit$non.slopes
  if(! length(nrp))  {
    nm1 <- names(fit$coef)[1]
    nrp <- 1 * (nm1 == "Intercept" | nm1 == "(Intercept)")
  }
  if(type == 'fit') return(nrp)

  w <- if(type == 'coef') fit$coefficients else {
    v <- fit[['var']]
    if(! length(v)) {
      v <- fit$info.matrix
      # If there is only one intercept for lrm or orm, info matrix may be non-partitioned
      if(length(v) && ('ab' %in% names(v))) attr(v, 'intercepts') <- nrow(v$ab)
    }
    v
  }
  i  <- attr(w, 'intercepts')
  li <- length(i)
  if(! li) return(nrp)
  if(li == 1 && i == 0) 0 else li
}
