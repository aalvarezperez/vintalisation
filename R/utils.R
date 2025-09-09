#' Round to nearest 'accuracy'
#' @export
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' Save a ggplot into ./visualisations
#' @inheritParams ggplot2::ggsave
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