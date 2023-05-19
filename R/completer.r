##' Create imputed dataset(s) using \code{transcan} and \code{aregImpute} objects
##'
##' Similar in function to `mice::complete`, this function uses `transcan` and `aregImpute` objects to impute missing data
##' and returns the completed dataset(s) as a dataframe or a list.
##' It assumes that `transcan` is used for single regression imputation.
##' @title completer
##' @param a          An object of class `transcan` or `aregImpute`
##' @param nimpute    A numeric vector between 1 and `a$n.impute`. For `transcan` object, this is set to 1. For `aregImpute` object, returns a list of `nimpute` datasets when `oneimpute` is set to `FALSE` (default).
##' @param oneimpute  A logical vector. When set to `TRUE`, returns a single completed dataset for the imputation number specified by `nimpute`
##' @param mydata     A data frame in which its missing values will be imputed.
##' @return           A single or a list of completed dataset(s).
##' @author           Yong-Hao Pua, Singapore General Hospital
##' @md
##' @examples
##' \dontrun{
##' mtcars$hp[1:5]    <- NA
##' mtcars$wt[1:10]   <- NA
##' myrform <- ~ wt + hp + I(carb)
##' mytranscan  <- transcan( myrform,  data = mtcars, imputed = TRUE,
##'   pl = FALSE, pr = FALSE, trantab = TRUE, long = TRUE)
##' myareg      <- aregImpute(myrform, data = mtcars, x=TRUE, n.impute = 5)
##' completer(mytranscan)                    # single completed dataset
##' completer(myareg, 3, oneimpute = TRUE)
##' # single completed dataset based on the `n.impute`th set of multiple imputation
##' completer(myareg, 3)
##' # list of completed datasets based on first `nimpute` sets of multiple imputation
##' completer(myareg)
##' # list of completed datasets based on all available sets of multiple imputation
##' # To get a stacked data frame of all completed datasets use
##' # do.call(rbind, completer(myareg, data=mydata))
##' # or use rbindlist in data.table
##' }
completer <- function (a, nimpute, oneimpute = FALSE, mydata) {


  trans <- inherits(a, 'transcan')
  areg  <- inherits(a, 'aregImpute')
  ni    <- a$n.impute

  if (trans)  nimpute <- 1
  if(missing(nimpute)) nimpute <- ni
  if(areg && nimpute > ni)
    stop ("'nimpute' cannot exceed ", ni, ".")

  if(missing(mydata)) mydata   <- eval(a$call$data)

  impute.transcan_args <- list(x = a, imputation = nimpute, data = mydata,  list.out = TRUE, pr = FALSE)
  if(trans) impute.transcan_args$imputation <- NULL   # remove `imputation` arg for transcan


  imputed_dflist <-
    if (trans | (areg & oneimpute) ) {
      # single completed dataset from transcan or aregImpute
      imputed <- do.call("impute.transcan", impute.transcan_args)
      mydata [ , names(imputed)] <- as.data.frame(imputed)
      return(mydata)
    } else {
      # list of completed datasets from aregImpute
      lapply(seq_len(nimpute), function(x){
        imputed <- do.call("impute.transcan", impute.transcan_args)
        mydata [ , names(imputed)] <- as.data.frame(imputed)
        return(mydata)
      })
    }

  return(imputed_dflist)
  }
