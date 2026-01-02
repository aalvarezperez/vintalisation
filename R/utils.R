#' Round to nearest 'accuracy'
#'
#' @param x Numeric vector to round.
#' @param accuracy Granularity to round to.
#' @param f Rounding function (defaults to [base::round()]).
#' @return A numeric vector with rounded values.
#' @export
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' Save a ggplot into ./visualisations
#' @param filename Destination file name (without path).
#' @param plot Plot object to save.
#' @param device Graphics device to use (e.g. `"png"`).
#' @param height Height of the output file (default pixels).
#' @param scale Aspect ratio multiplier (defaults to 16/9).
#' @param unit Units for width/height, defaults to `"px"`.
#' @export
make_plot <- function(filename,
                      plot = ggplot2::last_plot(),
                      device = "png",
                      height = 2500,
                      scale  = 16 / 9,
                      unit   = "px") {
  path <- file.path(getwd(), "visualisations")
  dir.create(path, showWarnings = FALSE)
  ggplot2::ggsave(
    plot = plot,
    filename = filename,
    path = path,
    dpi  = "retina",
    device = device,
    height = height,
    width  = height * scale,
    units  = unit
  )
  message("Plot saved in ./visualisations")
}

#' Watermark layer
#'
#' @param text Label to display as a watermark.
#' @param xmin,xmax,ymin,ymax Numeric bounds for the annotation.
#' @param scale Scaling factor applied to the watermark text size.
#' @param rot Rotation angle in degrees.
#' @param col Watermark colour.
#' @param alpha Alpha transparency value.
#' @return A ggplot2 annotation layer.
#' @export
watermark <- function(text = "Not validated",
                      xmin, xmax, ymin, ymax,
                      scale = 1, rot = 0,
                      col = "grey", alpha = .5) {
  ggplot2::annotation_custom(
    grid::textGrob(
      text,
      gp = grid::gpar(fontsize = 80 * scale, col = col, alpha = alpha),
      rot = rot
    ),
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax
  )
}
