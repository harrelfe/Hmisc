as.data.frame.Surv <- function(x, ...)
{
  rown <- if(length(dx1 <- dimnames(x)[[1]]))
            dx1
          else 
            as.character(1:nrow(x))
  ## Added names= 18Sep01
  structure(list(x), class="data.frame", names=deparse(substitute(x)),
            row.names=rown)
}
