.onLoad <- function(libname, pkgname) {
  .load_palettes(force = TRUE)
  ggplot2::update_geom_defaults("line",  list(linewidth = 1))
  ggplot2::update_geom_defaults("point", list(size = 2.75))
}
