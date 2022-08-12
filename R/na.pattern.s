na.pattern <- function(x)
{
  k <- ncol(x)
  pattern <- ''
  idt <- is.data.table(x)
  for(i in 1 : k) {
    y <- if(idt) x[, ..i] else x[, i]
    pattern <- paste0(pattern, 1 * is.na(y))
    }
  table(pattern)
}
utils::globalVariables('..i')

