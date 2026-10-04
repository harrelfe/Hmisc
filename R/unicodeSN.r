#' Format Numbers in Scientific Notation Using Unicode Superscripts
#'
#' Converts numbers to compact scientific notation such as `1.5×10⁻⁴`, using
#' Unicode superscript characters for the exponent so that the result needs no
#' LaTeX or markdown math and occupies little space in a Quarto (or R Markdown)
#' report.  Trailing zeros in the coefficient are removed (`2.00×10³` becomes
#' `2×10³`).  Numbers of moderate magnitude are not put in scientific notation;
#' they are simply rounded and formatted.
#'
#' @param x numeric vector.
#' @param digits maximum number of digits to the right of the decimal point in
#'   the coefficient (default 2).  Numbers that are not converted to scientific
#'   notation are shown with `digits + 1` significant digits, i.e., the same
#'   precision as the coefficient.
#' @param bounds two-element numeric vector `c(lower, upper)` (default
#'   `c(0.001, 100)`).  Numbers with `lower <= abs(x) < upper`, and zero, are
#'   shown in ordinary notation instead of scientific notation.
#'
#' @details
#' If `x` is zero, or if `bounds[1] <= abs(x) < bounds[2]`, the result is the
#' number rounded and formatted with `format()` instead of scientific
#' notation.  All
#' other finite values are written as `coefficient×10^exponent` with a
#' coefficient whose absolute value is at least 1 and less than 10.  `NA` is
#' returned as `NA`, and `Inf`/`-Inf`/`NaN` are returned as their usual text.
#'
#' Rounding in the non-scientific range uses significant digits
#' (`signif(x, digits + 1)`) rather than `round(x, digits)` so that values such
#' as 0.0012 are not displayed as 0.
#'
#' @return a character vector of the same length as `x`.
#'
#' @author Frank Harrell
#' @examples
#' unicodeSN(c(123456, 0.000123, 1500, 2e10, -0.00045, 3.14159, 0, 0.05, 42.678))
#' unicodeSN(299792458, digits = 4)
#' unicodeSN(9.999e5)        # rounds up to 1×10⁶
#' unicodeSN(c(0.5, 50, 5000), bounds = c(1, 1000))
#' # In Quarto, inline code such as `r unicodeSN(p)` needs no special chunk options
unicodeSN <- function(x, digits = 2, bounds = c(0.001, 100)) {
  stopifnot(length(bounds) == 2, bounds[1] <= bounds[2])
  one <- function(v) {
    if (is.na(v))       return(NA_character_)
    if (! is.finite(v)) return(as.character(v))
    a <- abs(v)
    if (a == 0 || (a >= bounds[1] && a < bounds[2]))
      return(format(signif(v, digits + 1)))

    e <- floor(log10(a))
    m <- round(v / 10^e, digits)
    if (abs(m) >= 10) {              # e.g. 9.999 rounds to 10.00
      m <- m / 10
      e <- e + 1
    }
    coef <- formatC(m, format = 'f', digits = digits)
    if (grepl('.', coef, fixed = TRUE))
      coef <- sub('\\.?0+$', '', coef)
    paste0(coef, '×10',
           chartr('-0123456789', '⁻⁰¹²³⁴⁵⁶⁷⁸⁹',
                  as.character(e)))
  }
  vapply(x, one, character(1), USE.NAMES = FALSE)
}
