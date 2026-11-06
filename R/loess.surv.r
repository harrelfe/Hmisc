#' Loess/Lowess Smoother for Censored Data
#'
#' Estimates the probability of surviving past time `u` (i.e. the
#' probability that the event has not occurred by `u`) as a smooth function of
#' a continuous predictor `x`, when the response is right-censored.  This is a
#' modification of the `loess.surv` function accompanying
#' van Klaveren et al. (2026); see References.
#'
#' @details
#' Observations are reweighted by inverse probability of censoring weighting
#' (IPCW).  Let \eqn{Y_u = \min(Y, u)}.  A Cox model for the \emph{censoring}
#' distribution is fitted to \eqn{(Y_u, \text{censored before } u)}, yielding
#' for each subject the probability `K` that the event time was observed
#' (estimated as the exponential of minus the expected number of censoring
#' events).  The indicator of an observed event before `u` is then divided by
#' `K`, and this inflated indicator is smoothed against `x`.  The smooth
#' estimates the cumulative incidence at `u`, and the function returns
#' \eqn{1 - } that estimate, curtailed to \eqn{[0, 1]}.
#'
#' With the default `smoother = "lowess"`, [stats::lowess()] is used with
#' iterations turned off (`iter = 0`); this appears to perform better than
#' [stats::loess()] and is much faster.  The lowess fit is interpolated to
#' `xout` with [stats::approx()].  Note that `xout` values outside the range of
#' `x` give `NA` with `smoother = "lowess"`.
#'
#' Relative to the original function, this version returns estimated survival
#' probabilities rather than cumulative incidences, and curtails estimates to
#' lie in \eqn{[0, 1]}.
#'
#' `NA`s are excluded from `x` and `S` before calculations commence.
#'
#' @param x Numeric vector, the predictor to be smoothed against.
#' @param S A [survival::Surv()] object (right-censored) containing the follow-up
#'   times and event indicators, with the same length as `x`.
#' @param u Numeric scalar, the time point at which the survival probability is
#'   to be estimated.
#' @param xout Numeric vector of `x` values at which to evaluate the smooth.
#'   Defaults to 200 equally spaced points over the range of `x`.
#' @param span Smoothing span: the `f` argument of [stats::lowess()] or the
#'   `span` argument of [stats::loess()].  Default 0.75.
#' @param formula.cens One-sided formula giving the right-hand side of the Cox
#'   model used for the censoring distribution, e.g. `~ x` or `~ strata(g)`.
#'   Default `~ 1` assumes censoring is independent of covariates.  Variables
#'   must be found in the calling environment.
#' @param smoother Which smoother to use: `"lowess"` (default, with
#'   `iter = 0`) or `"loess"`.
#' @param pl Logical.  If `TRUE`, plot the IPCW-inflated event indicators
#'   (unless `add = TRUE`) and draw the estimated survival curve.
#' @param add Logical.  If `TRUE` and `pl = TRUE`, add the curve (in red) to an
#'   existing plot instead of starting a new one.
#'
#' @return A list with components
#'   \item{x}{the values `xout` at which the smooth was evaluated}
#'   \item{y}{estimated probability of surviving past `u` at each `x`, in
#'     \eqn{[0, 1]}}
#'
#' @references
#' van Klaveren D, Austin PC, Serruys PW, van Calster B, Harrell FE (2026).
#' Locally estimated scatterplot smoothing for censored survival data.
#' *Statistics in Medicine*. \doi{10.1002/sim.70783}
#'
#' @seealso [stats::lowess()], [stats::loess()], [survival::coxph()]
#'
#' @examples
#' \dontrun{
#' require(survival)
#' set.seed(1)
#' n  <- 500
#' x  <- runif(n)
#' tt <- rexp(n, rate = exp(x - 0.5))
#' cc <- rexp(n, rate = 0.3)
#' S  <- Surv(pmin(tt, cc), tt <= cc)
#' r  <- loess.surv(x, S, u = 1, pl = TRUE)
#' r2 <- loess.surv(x, S, u = 1, smoother = "loess", formula.cens = ~ x)
#' }
#'
#' @export
loess.surv <- function(x, S, u,
                       xout = seq(min(x), max(x), length.out = 200),
                       span = 0.75, formula.cens = ~ 1,
                       smoother = c('lowess', 'loess'), pl = FALSE, add = FALSE) {
  smoother <- match.arg(smoother)
  isna <- is.na(x) | is.na(S)
  if(any(isna)) {
    x <- x[! isna]
    S <- S[! isna,, drop=FALSE]
  }
  y <- S[, 1]                               # Obtain event times
  e <- S[, 2]                               # Obtain event indicators
  y.u <- pmin(y, u)                         # Observed event times at time u
  e.u <- e * (y < u)                        # Event indicators at time u
  e.cens.u <- (1 - e) * (y < u)             # Indicators of censoring at time u
  form <- as.formula(paste('survival::Surv(y.u, e.cens.u)', deparse(formula.cens)))
  G <- survival::coxph(form)                # Fit censoring model G
  K <- exp(- predict(G, type = 'expected')) # Probabilities that event times are observed
  e.loess <- e.u * 1 / K                    # The event indicators are inflated with 1/K
  r <- switch(smoother,
              lowess = {f <- lowess(x, e.loess, f = span, iter = 0)
                        approx(f$x, f$y, xout = xout) },
              loess  = {f <- loess(e.loess ~ x, span = span)
                        list(x = xout, y = predict(f, data.frame(x = xout))) } )
  if (pl) {
    if (! add) plot(x, e.loess)
    lines(r$x, 1 - r$y, col = if (add) 'red' else 'black')
  }
  list(x = r$x, y = pmax(0, pmin(1, 1 - r$y)))
}
