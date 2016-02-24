#' Add a lowess smoother without counfidence bands.
#'
#' Automatically selects \code{iter=0} for \code{lowess} if \code{y} is binary, otherwise uses \code{iter=3}.
#'
#'
#' @param span see \code{f} argument to \code{lowess}
#' @param fun a function to transform smoothed \code{y}
#' @param fullrange should the fit span the full range of the plot, or just
#'   the data
#' @param n number of points to evaluate smoother at
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param ... other arguments are passed to smoothing function
#' @inheritParams stat_identity
#' @return a data.frame with additional columns
#'   \item{y}{predicted value}
#' @seealso
#'   \code{\link{lowess}} for \code{loess} smoother.
#' @export
#' @examples
#' \donttest{
#' c <- ggplot(mtcars, aes(qsec, wt))
#' c + stat_plsmo()
#' c + stat_plsmo() + geom_point()
#'
#' c + stat_plsmo(span = 0.1) + geom_point()
#'
#' # Smoothers for subsets
#' c <- ggplot(mtcars, aes(y=wt, x=mpg)) + facet_grid(. ~ cyl)
#' c + stat_plsmo() + geom_point()
#' c + stat_plsmo(fullrange = TRUE) + geom_point()
#'
#' # Geoms and stats are automatically split by aesthetics that are factors
#' c <- ggplot(mtcars, aes(y=wt, x=mpg, colour=factor(cyl)))
#' c + stat_plsmo() + geom_point()
#' c + stat_plsmo(aes(fill = factor(cyl))) + geom_point()
#' c + stat_plsmo(fullrange=TRUE) + geom_point()
#'
#' # Example with logistic regression
#' data("kyphosis", package="rpart")
#' qplot(Age, as.numeric(Kyphosis) - 1, data = kyphosis) + stat_plsmo()
#' }

stat_plsmo <- function (mapping = NULL, data = NULL, geom = "smooth",
                        position = "identity",
                        n = 80, fullrange = FALSE, span=2/3, fun=function(x) x,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                        ...) {

  layer(
    stat = StatPlsmo, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      n = n,
      fullrange = fullrange,
      span = span,
      fun = fun,
      na.rm = na.rm,
      ...)
  )
}

StatPlsmo <- ggplot2::ggproto("StatPlsmo", ggplot2::Stat,
  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    rows <- plyr::daply(data, "group", function(df) length(unique(df$x)))

    if (all(rows == 1) && length(rows) > 1) {
      message("geom_plsmo: Only one unique x value each group.",
        "Maybe you want aes(group = 1)?")
      return(data.frame())
    }

    data
  },

  compute_group = function(., data, scales, n=80, span=2/3, fun=function(x) x,
                        fullrange=FALSE, xseq = NULL, na.rm = FALSE) {
    data <- remove_missing(data, na.rm, c("x", "y"), name="stat_plsmo")
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(data.frame())
    }

    if (is.null(xseq)) {
      if (is.integer(data$x)) {
        if (fullrange) {
          xseq <- scales$x$dimension()
        } else {
          xseq <- sort(unique(data$x))
        }
      } else {
        if (fullrange) {
          range <- scales$x$dimension()
        } else {
          range <- range(data$x, na.rm = TRUE)
        }
        xseq <- seq(range[1], range[2], length.out = n)
      }
    }

    n_y <- length(unique(data$y[!is.na(data$y)]))

    z <- lowess(data$x, data$y,
      iter = if (n_y < 3) 0 else 3,
      f = span
    )
    z <- approx(z, xout = xseq)
    z$y <- fun(z$y)
    as.data.frame(z)
  }
)
