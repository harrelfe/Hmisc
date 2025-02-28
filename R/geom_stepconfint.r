#' @title Step function confidence intervals for ggplot2
#' @description Produces a step function confidence interval for survival curves.  This function is taken from
#' the \code{utile.visuals} package by Eric Finnesgard.  That package is not used because of its
#' strong dependencies.
#' @param mapping Aesthetic mappings with aes() function. Like geom_ribbon(), you must provide
#' columns for x, ymin (lower limit), ymax (upper limit).
#' @param data The data to be displayed in this layer. Can inherit from ggplot parent.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' Defaults to 'identity'.
#' @param position Position adjustment, either as a string, or the result of a call to a
#' position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE,
#' missing values are silently removed.
#' @param ... Optional. Any other ggplot geom_ribbon() arguments.
#' @note Originally adapted from the survminer package <https://github.com/kassambara/survminer>.
#' @examples
#' require(survival)
#' require(ggplot2)
#'
#' f <- survfit(Surv(time, status) ~ trt, data = diabetic)
#' d <- with(f, data.frame(time, surv, lower, upper, trt=rep(names(f$strata), f$strata)))
#' ggplot(d, aes(x = time, y=surv)) +
#'   geom_step(aes(color = trt)) +
#'   geom_stepconfint(aes(ymin = lower, ymax = upper, fill = trt), alpha = 0.3) +
#'   coord_cartesian(c(0, 50)) +
#'   scale_x_continuous(expand = c(0.02,0)) +
#'   labs(x = 'Time', y = 'Freedom From Event') +
#'   scale_color_manual(
#'     values = c('#d83641', '#1A45A7'),
#'     name = 'Treatment',
#'     labels = c('None', 'Laser'),
#'     aesthetics = c('colour', 'fill'))
#' @export
#' @author Eric Finnesgard
geom_stepconfint <- function (
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, ...) {
  ggplot2::layer(
    mapping = mapping,
    data = data,
    stat = stat,
    geom = ggplot2::ggproto(
      `_class` = 'GeomConfint',
      `_inherit` = ggplot2::GeomRibbon,
      required_aes = c("x", "ymin", "ymax"),
      draw_group = function (self, data, panel_scales, coord, na.rm = FALSE) {
        if (na.rm) data <- data[stats::complete.cases(self$required_aes), ]
        data <- data[order(data$group, data$x), ]
        data <- self$stairstep_confint(data)
        ggplot2::GeomRibbon$draw_group(data, panel_scales, coord, na.rm = FALSE)
      },
      stairstep_confint = function (data) {
        data <- as.data.frame(data)[order(data$x), ]
        n <- nrow(data)
        ys <- rep(1:n, each = 2)[-2 * n]
        xs <- c(1, rep(2:n, each = 2))
        data.frame(
          x = data$x[xs],
          ymin = data$ymin[ys],
          ymax = data$ymax[ys],
          data[xs, setdiff(names(data), c("x", "ymin", "ymax"))]
        )
      }
    ),
    position = position,
    params = list(na.rm = na.rm, ...)
  )
}
