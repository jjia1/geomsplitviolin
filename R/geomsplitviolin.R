#' @import ggplot2
#' @export
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1, "group"]
  newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
      1))
    quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
  }
})

#' Split Violin Plot
#'
#' Creates a split violin plot, allowing for the visualization of the distribution
#' of a variable for two groups side-by-side in the same violin plot.
#'
#' @param mapping Set of aesthetic mappings created by `aes` or `aes_`.
#' @param data The data to be displayed in this layer. It can be omitted, in which case
#' the default data defined in the ggplot object will be used.
#' @param ... Additional arguments passed to `layer`. These are often aesthetics,
#' used to set an aesthetic to a fixed value, like `color = "red"` or `size = 3`.
#' They may also be parameters to the paired geom/stat.
#' @param draw_quantiles Numeric vector of quantiles to draw as horizontal lines
#' inside the violins.
#' @param trim If `FALSE`, doesn't trim the tails of the violins to the range of the data.
#' @param scale Scale parameter for the violin plot.
#' @param na.rm If `FALSE`, the default, missing values are removed with a warning.
#' If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' `NA`, the default, includes if any aesthetics are mapped.
#' `FALSE` never includes, and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#' rather than combining with them.
#' @return A ggplot layer
#' @examples
#' ggplot(data = YourDataFrame, aes(x = factorVariable, y = numericVariable)) +
#'   geom_split_violin()
#' @export
geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}
