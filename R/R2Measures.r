##' Generalized R^2 Measures
##'
##' Computes various generalized R^2 measures related to the Maddala-Cox-Snell (MCS) R^2 for regression models fitted with maximum likelihood.  The original MCS R^2 is labeled `R2` in the result.  This measure uses the raw sample size `n` and does not penalize for the number of free parameters, so it can be rewarded for overfitting.  A measure adjusted for the number of fitted regression coefficients `p` uses the analogy to R^2 in linear models by computing `1 - exp(- lr / n) * (n-1)/(n-p-1)` if `padj=2`, which is approximately `1 - exp(- (lr - p) / n)`, the version used if `padj=1` (the default).  The latter measure is appealing because the expected value of the likelihood ratio chi-square statistic `lr` is `p` under the global null hypothesis of no predictors being associated with the response variable.  See <https://hbiostat.org/bib/r2.html> for more details.
##'
##' It is well known that in logistic regression the MCS R^2 cannot achieve a value of 1.0 even with a perfect model, which prompted Nagelkerke to divide the R^2 measure by its maximum attainable value.  This is not necessarily the best recalibration of R^2 throughout its range.  An alternative is to use the formulas above but to replace the raw sample size `n` with the effective sample size, which for data with many ties can be significantly lower than the number of observations.  As used in the `popower()` and `describe()` functions, in the context of a Wilcoxon test or the proportional odds model, the effective sample size is `n * (1 - f)` where `f` is the sums of cubes of the proportion of observations at each distict value of the response variable.  Whitehead derived this from an approximation to the variance of a log odds ratio in a proportional odds model.  To obtain R^2 measures using the effective sample size, either provide `ess` as a single number specifying the effective sample size, or specify a vector of frequencies of distinct Y values from which the effective sample size will be computed.  In the context of survival analysis, the single number effective sample size you may wish to specify is the number of uncensored observations.  This is exactly correct when estimating the hazard rate from a simple exponential distribution or when using the Cox PH/log-rank test.  For failure time distributions with a very high early hazard, censored observations contain enough information that the effective sample size is greater than the number of events.  See Benedetti et al, 1982.
##'
##' If the effective sample size equals the raw sample size, measures involving the effective sample size are set to \code{NA}.
##' @title R2Measures
##' @param lr likelihoood ratio chi-square statistic
##' @param p number of non-intercepts in the model that achieved `lr`
##' @param n raw number of observations
##' @param ess if a single number, is the effective sample size.  If a vector of numbers is assumed to be the frequency tabulation of all distinct values of the outcome variable, from which the effective sample size is computed.
##' @param padj set to 2 to use the classical adjusted R^2 penalty, 1 (the default) to subtract `p` from `lr`
##' @return named vector of R2 measures.  The notation for results is `R^2(p, n)` where the `p` component is empty for unadjusted estimates and `n` is the sample size used (actual sample size for first measures, effective sample size for remaining ones).  For indexes that are not adjusted, only `n` appears.
##' @author Frank Harrell
##' @md
##' @export
##' @references
##' Smith TJ and McKenna CM (2013): A comparison of logistic regression pseudo R^2 indices.  Multiple Linear Regression Viewpoints 39:17-26.  <https://www.glmj.org/archives/articles/Smith_v39n2.pdf>
##' 
##' Benedetti JK, et al (1982): Effective sample size for tests of censored survival data.  Biometrika 69:343--349.
##' 
##' Mittlbock M, Schemper M (1996): Explained variation for logistic regression.  Stat in Med 15:1987-1997.
##' 
##' Date, S: R-squared, adjusted R-squared and pseudo R-squared. <https://timeseriesreasoning.com/contents/r-squared-adjusted-r-squared-pseudo-r-squared/>
##' 
##' UCLA: What are pseudo R-squareds? <https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/>
##' 
##' Allison P (2013): What's the beset R-squared for logistic regression? <https://statisticalhorizons.com/r2logistic/>
##' 
##' Menard S (2000): Coefficients of determination for multiple logistic regression analysis.  The Am Statistician 54:17-24.
##'
##' Whitehead J (1993): Sample size calculations for ordered categorical data.  Stat in Med 12:2257-2271.  See errata (1994) 13:871 and letter to the editor by Julious SA, Campbell MJ (1996) 15:1065-1066 showing that for 2-category Y the Whitehead sample size formula agrees closely with the usual formula for comparing two proportions.
##' @examples
##' x <- c(rep(0, 50), rep(1, 50))
##' y <- x
##' # f <- lrm(y ~ x)
##' # f   # Nagelkerke R^2=1.0
##' # lr <- f$stats['Model L.R.']
##' # 1 - exp(- lr / 100)  # Maddala-Cox-Snell (MCS) 0.75
##' lr <- 138.6267  # manually so don't need rms package
##'
##' R2Measures(lr, 1, 100, c(50, 50))  # 0.84 Effective n=75
##' R2Measures(lr, 1, 100, 50)         # 0.94
##' # MCS requires unreasonable effective sample size = minimum outcome
##' # frequency to get close to the 1.0 that Nagelkerke R^2 achieves

R2Measures <- function(lr, p, n, ess=NULL, padj=1) {
  R     <- numeric(0)
  r2    <- 1. - exp(- lr / n)
  adj <- function() if(padj == 1) 1. - exp(- max(lr - p, 0) / n)
           else
             1. - exp(- lr / n) * (n - 1.) / (n - p - 1.)
  r2adj <- adj()
  R <- c(R2=r2, R2adj=r2adj)
  g <- function(p, n) {
    n <- as.character(round(n, 1))
    sub <- ifelse(p == 0, n, paste0(p, ',', n))
    paste0('R2(', sub, ')')
  }
  name    <- g(c(0, p), c(n, n))

  if(length(ess)) {
    ## Replace n with effective sample size
    nr <- n
    n <- if(length(ess) == 1) ess else {
      P <- ess / sum(ess)
      n * (1. - sum(P ^ 3))
        }
    r2    <- 1. - exp(- lr / n)
    r2adj <- adj()
    if(n == nr) r2 <- r2adj <- NA
    name  <- c(name, g(c(0, p), c(n, n)))
    R     <- c(R, r2, r2adj)
  }
  names(R) <- name
  R
}
