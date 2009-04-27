## survfit.km <- function(...) {
##   if(!.R.) stop('this function should not override an S-Plus function')
##   require(survival)
##   getFromNamespace(ifelse(packageDescription('survival',fields='Version') >= "2.35-3",
##                           'survfitKM', 'survfit.km'),
##                    'survival')(...)
## }

if(.R.) {
  if(packageDescription('survival',fields='Version') >= "2.35-3")
    survfit.km <- survival:::survfitKM
}
