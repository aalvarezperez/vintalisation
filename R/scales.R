#' Discrete/continuous colour scale using Adevinta palettes
#' @inheritParams ggplot2::scale_colour_manual
#' @export
scale_color_adevinta <- function(palette = "main", discrete = TRUE,
                                 invert = FALSE, reverse = FALSE, ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("adevinta_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale (wrapper around \code{scale_color_adevinta})
#' @inheritParams scale_color_adevinta
#' @export
scale_fill_adevinta <- function(palette = "main", discrete = TRUE,
                                invert = FALSE, reverse = FALSE, ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("adevinta_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}