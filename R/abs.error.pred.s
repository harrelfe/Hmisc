abs.error.pred <- function(fit, lp=NULL, y=NULL)
{
  if(!length(y))  y  <- fit$y
  if(!length(lp)) lp <- fit$fitted.values
  if(!length(lp)) lp <- fit$linear.predictors
  if(!(length(y) && length(lp)))
    stop('must specify lp and y or specify y=T in the fit')
  
  s <- is.na(y + lp)
  
  if(any(s)) {
    y  <- y[!s]
    lp <- lp[!s]
  }
  
  my    <- median(y)
  mlp   <- median(lp)
  meanr <- mean(  abs( lp - mlp))
  meant <- mean(  abs(  y - my ))
  meane <- mean(  abs( lp -  y ))
  medr  <- median(abs( lp - mlp))
  medt  <- median(abs(  y - my ))
  mede  <- median(abs( lp -  y ))

  differences <- cbind(c(meanr,meane,meant),
                       c(medr ,mede ,medt ) )

  dimnames(differences) <- list(c('|Yi hat - median(Y hat)|',
                                  '|Yi hat - Yi|',
                                  '|Yi - median(Y)|'),
                                c('Mean','Median'))
  
  ratios <- cbind(c(meanr/meant, meane/meant),
                  c( medr/ medt,  mede/ medt))
  dimnames(ratios) <- list(c('|Yi hat - median(Y hat)|/|Yi - median(Y)|',
                             '|Yi hat - Yi|/|Yi - median(Y)|'),
                           c('Mean','Median'))
  structure(list(differences=differences,ratios=ratios),class='abs.error.pred')
}

print.abs.error.pred <- function(x, ...)
{
  cat('\nMean/Median |Differences|\n\n')
  print(x$differences)
  cat('\n\nRatios of Mean/Median |Differences|\n\n')
  print(x$ratios)
  invisible()
}
