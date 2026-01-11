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

#' Save a ggplot with sensible defaults
#'
#' @param filename Destination file name (without path).
#' @param plot Plot object to save.
#' @param path Output directory (default: "./visualisations"). Created if needed.
#' @param device Graphics device to use (e.g. `"png"`).
#' @param height Height of the output file (default pixels).
#' @param aspect_ratio Width/height ratio (defaults to 16/9).
#' @param unit Units for width/height, defaults to `"px"`.
#' @param dpi Resolution: "retina" (320), "print" (300), "screen" (72), or numeric.
#' @export
make_plot <- function(filename,
                      plot = ggplot2::last_plot(),
                      path = file.path(getwd(), "visualisations"),
                      device = "png",
                      height = 2500,
                      aspect_ratio = 16 / 9,
                      unit = "px",
                      dpi = "retina") {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  dpi_numeric <- switch(
    as.character(dpi),
    "retina" = 320,
    "print" = 300,
    "screen" = 72,
    as.numeric(dpi)
  )
  showtext::showtext_opts(dpi = dpi_numeric)

  ggplot2::ggsave(
    plot = plot,
    filename = filename,
    path = path,
    dpi = dpi,
    device = device,
    height = height,
    width = height * aspect_ratio,
    units = unit
  )
  message("Plot saved to ", file.path(path, filename))
}

#' Watermark layer
#'
#' Adds a text watermark annotation to a ggplot. By default spans the entire
#' plot area.
#'
#' @param text Label to display as a watermark.
#' @param xmin,xmax,ymin,ymax Numeric bounds for the annotation. Defaults to
#'   `-Inf`/`Inf` to span entire plot.
#' @param scale Scaling factor applied to the watermark text size.
#' @param rot Rotation angle in degrees.
#' @param col Watermark colour.
#' @param alpha Alpha transparency value.
#' @return A ggplot2 annotation layer.
#' @export
watermark <- function(text = "Not validated",
                      xmin = -Inf, xmax = Inf,
                      ymin = -Inf, ymax = Inf,
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
