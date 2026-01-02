#' Discrete/continuous colour scale using custom palettes
#'
#' @inheritParams get_palette
#' @param discrete Logical; if `TRUE`, build a discrete scale, otherwise a
#'   continuous gradient.
#' @param ... Additional arguments passed to the underlying ggplot2 scale.
#' @export
scale_color_my_palette <- function(palette = "adevinta_brand_1", discrete = TRUE,
                                   invert = FALSE, reverse = FALSE, ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("custom_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale (wrapper around \code{scale_color_my_palette})
#' @inheritParams scale_color_my_palette
#' @export
scale_fill_my_palette <- function(palette = "adevinta_brand_1", discrete = TRUE,
                                  invert = FALSE, reverse = FALSE, ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("custom_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' Manychat branded colour scale
#'
#' @inheritParams scale_color_my_palette
#' @param palette Palette name; defaults to the most versatile Manychat palette.
#' @export
scale_color_manychat <- function(palette = "manychat_diverging_1",
                                 discrete = TRUE,
                                 invert = FALSE,
                                 reverse = FALSE,
                                 ...) {
  scale_color_my_palette(
    palette = palette,
    discrete = discrete,
    invert = invert,
    reverse = reverse,
    ...
  )
}

#' @rdname scale_color_manychat
#' @export
scale_fill_manychat <- function(palette = "manychat_diverging_1",
                                discrete = TRUE,
                                invert = FALSE,
                                reverse = FALSE,
                                ...) {
  scale_fill_my_palette(
    palette = palette,
    discrete = discrete,
    invert = invert,
    reverse = reverse,
    ...
  )
}
