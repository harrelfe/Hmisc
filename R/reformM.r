# Function to automatically reorder variables for use with aregImpute
# Modification of function written by Yong Hao Pua
# Specify nperm=integer to obtain that many formulae with random
# permutation of order of variables; omit nperm to obtain formula
# with variables sorted by decreasing number of NAs

reformM <- function(formula, data, nperm) {
  cs <- all.vars(formula)
  data <- data[, cs]
  ismiss <- function(x) if(is.character(x)) is.na(x) | x==''  else is.na(x) 
  m <- sapply(data, ismiss)
  miss.per.obs <- apply(m, 1, sum)
  miss.per.var <- apply(m, 2, sum)
  # Percent of observations with any missings:
  pm <- ceiling(100 * sum(miss.per.obs > 0) / length(miss.per.obs))
  nimpute = max(5, pm)
  cat("Recommended number of imputations:", nimpute, "\n")

  if(missing(nperm)) {
    j <- order(miss.per.var, decreasing=TRUE) # var with highest NA freq first
    formula <- as.formula(paste('~', paste(cs[j], collapse=' + ')))
    }
  else {
    formula <- list()
    prev <- character(0)
    # Could add logic to sample() until permutation is different from
    # all previous permutations
    for(i in 1 : nperm) {
      ## Try up to 10 times to get a unique permutation
      for(j in 1 : 10) {
        f <- paste('~', paste(sample(cs), collapse=' + '))
        if(f %nin% prev) {
          prev <- c(prev, f)
          formula[[i]] <- as.formula(f)
          break
        }
      }
    }
    
    if(nperm == 1) formula <- formula[[1]]
  }
formula
}
