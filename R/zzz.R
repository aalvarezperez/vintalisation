.onLoad <- function(libname, pkgname) {
  .load_palettes(force = TRUE)
}

#' Apply vintalisation geom defaults
#'
#' Sets recommended ggplot2 geom defaults for line width and point size.
#' Call this function explicitly to opt-in to these defaults, as they affect
#' all ggplot2 plots in your session.
#'
#' @param line_width Line width for geom_line (default 1).
#' @param point_size Point size for geom_point (default 2.75).
#' @return Invisibly returns NULL. Called for side effects.
#' @export
#' @examples
#' \dontrun{
#' vintalisation_defaults()
#' }
vintalisation_defaults <- function(line_width = 1, point_size = 2.75) {
  ggplot2::update_geom_defaults("line",  list(linewidth = line_width))
  ggplot2::update_geom_defaults("point", list(size = point_size))
  invisible(NULL)
}
