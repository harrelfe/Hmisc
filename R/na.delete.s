## Enhancement of na.omit  F. Harrell 20 Oct 91
## Allows an element of the data frame to be another data frame
## Note: S does not invoke na.action if only a data frame variable is missing!

na.delete <- function(frame)
{
  y.detail <- na.detail.response(frame)
  n <- length(frame)
  omit <- FALSE
  vars <- seq(length = n)
  nmiss <- rep(0,n)
  storage.mode(nmiss) <- "integer"
  for(j in vars) {
    x <- frame[[j]]
    if(is.data.frame(x))
      x <- as.matrix(x)
    class(x) <- NULL	#so Surv object is.na ignored
    if(!is.atomic(x)) 
      stop("non-atomic, non-data frame variables not allowed")
    
    ## variables are assumed to be either some sort of matrix, numeric or cat'y
    isna <- is.na(x)	#Change from T. Therneau
    d <- dim(x)
    if(is.null(d) || length(d) != 2) {
      ##isna <- is.na(x)
      nmiss[j] <- sum(isna)
      omit <- omit | isna
    } else {
      ##isna <-is.na(x %*% rep(0,d[2]))
      isna <- (isna %*% rep(1,d[2])) > 0
      nmiss[j] <- sum(isna)
      omit <- omit | isna
    }
  }
  
  if(any(omit)) {
    rn <- row.names(frame)

    frame <- frame[!omit,,drop=FALSE]
    names(nmiss) <- names(frame)
    ## a %ia% b terms are included - delete them since main effects
    ## already counted  (next 2 stmts reinstated 27Oct93)

    i <- grep("%ia%", names(nmiss))
    if(length(i)>0)
      nmiss <- nmiss[-i]
    
    attr(frame,"nmiss") <- nmiss    # for backward compatibility
    temp <- seq(omit)[omit]
    names(temp) <- rn[omit]
    na.info <- list(nmiss=nmiss, omit=temp, 
                    na.detail.response=y.detail)
    
    class(na.info) <- "delete"
    attr(frame, "na.action") <- na.info
  }
  
  frame
}


naprint.delete <- function(x, ...)
{
  if(length(g <- x$nmiss)) {
    cat("Frequencies of Missing Values Due to Each Variable\n")
    print(g)
    cat("\n")
  }
  
  if(length(g <- x$na.detail.response)) {
    cat("\nStatistics on Response by Missing/Non-Missing Status of Predictors\n\n")
    print(unclass(g))
    cat("\n")		
  }
  
  invisible()
}
   
globalVariables("naresid.omit")
naresid.delete <- napredict.delete <- function(omit, x, ...)
{
  omit <- omit$omit
  if(exists('naresid.omit')) naresid.omit(omit, x)
  else {
    if(!existsFunction('naresid.exclude'))
      naresid.exclude <- getFromNamespace('naresid.exclude','stats')
    naresid.exclude(omit, x)
  }
}


nafitted.delete <- function(obj, x)
{
  omit <- obj$omit
  if(exists('naresid.omit'))
    naresid.omit(omit, x)
  else
    getFromNamespace('naresid.exclude','stats')(omit, x)
}
