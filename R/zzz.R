.onLoad <- function(libname, pkgname) {
  yaml_file <- system.file("palettes.yml", package = pkgname)
  .palettes <- yaml::read_yaml(yaml_file)
  
  # Store as internal object
  assign(".palettes", .palettes, envir = parent.env(environment()))
  
  ggplot2::update_geom_defaults("line",  list(linewidth = 1))
  ggplot2::update_geom_defaults("point", list(size = 2.75))
}