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
    ggplot2::discrete_scale("colour", palette = pal, ...)
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
    ggplot2::discrete_scale("fill", palette = pal, ...)
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

#' Manychat gradient colour scale
#'
#' Continuous gradient scale using Manychat palettes, analogous to
#' \code{\link[ggplot2]{scale_color_gradient}}.
#'
#' @inheritParams get_palette
#' @param palette Palette name; defaults to a Manychat gradient palette.
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{scale_color_gradientn}}.
#' @export
scale_color_gradient_manychat <- function(palette = "manychat_diverging_1",
                                          invert = FALSE,
                                          reverse = FALSE,
                                          ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  ggplot2::scale_color_gradientn(colours = pal(256), ...)
}

#' @rdname scale_color_gradient_manychat
#' @export
scale_fill_gradient_manychat <- function(palette = "manychat_diverging_1",
                                         invert = FALSE,
                                         reverse = FALSE,
                                         ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}

#' Manychat diverging gradient colour scale
#'
#' Diverging gradient scale with a midpoint, analogous to
#' \code{\link[ggplot2]{scale_color_gradient2}}.
#'
#' @inheritParams get_palette
#' @param palette Palette name; defaults to a Manychat diverging palette.
#' @param midpoint The midpoint value of the scale (default 0).
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{scale_color_gradient2}}.
#' @export
scale_color_gradient2_manychat <- function(palette = "manychat_diverging_1",
                                           midpoint = 0,
                                           invert = FALSE,
                                           reverse = FALSE,
                                           ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  cols <- pal(3)
  ggplot2::scale_color_gradient2(
    low = cols[1],
    mid = cols[2],
    high = cols[3],
    midpoint = midpoint,
    ...
  )
}

#' @rdname scale_color_gradient2_manychat
#' @export
scale_fill_gradient2_manychat <- function(palette = "manychat_diverging_1",
                                          midpoint = 0,
                                          invert = FALSE,
                                          reverse = FALSE,
                                          ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  cols <- pal(3)
  ggplot2::scale_fill_gradient2(
    low = cols[1],
    mid = cols[2],
    high = cols[3],
    midpoint = midpoint,
    ...
  )
}

#' Manychat n-colour gradient scale
#'
#' Continuous gradient scale using all colours from a palette, analogous to
#' \code{\link[ggplot2]{scale_color_gradientn}}.
#'
#' @inheritParams get_palette
#' @param palette Palette name; defaults to a Manychat palette.
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{scale_color_gradientn}}.
#' @export
scale_color_gradientn_manychat <- function(palette = "manychat_diverging_1",
                                           invert = FALSE,
                                           reverse = FALSE,
                                           ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  ggplot2::scale_color_gradientn(colours = pal(256), ...)
}

#' @rdname scale_color_gradientn_manychat
#' @export
scale_fill_gradientn_manychat <- function(palette = "manychat_diverging_1",
                                          invert = FALSE,
                                          reverse = FALSE,
                                          ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  ggplot2::scale_fill_gradientn(colours = pal(256), ...)
}

# -------------------------------------------------------------------------
# Binned scales
# -------------------------------------------------------------------------

#' Manychat binned colour scale
#'
#' Binned colour scale for continuous data displayed in discrete bins,
#' analogous to \code{\link[ggplot2]{scale_color_binned}}.
#'
#' @inheritParams get_palette
#' @param palette Palette name; defaults to a Manychat palette.
#' @param n_bins Number of bins (default 6).
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{scale_color_stepsn}}.
#' @export
scale_color_binned_manychat <- function(palette = "manychat_diverging_1",
                                        n_bins = 6,
                                        invert = FALSE,
                                        reverse = FALSE,
                                        ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  ggplot2::scale_color_stepsn(colours = pal(n_bins), ...)
}

#' @rdname scale_color_binned_manychat
#' @export
scale_fill_binned_manychat <- function(palette = "manychat_diverging_1",
                                       n_bins = 6,
                                       invert = FALSE,
                                       reverse = FALSE,
                                       ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  ggplot2::scale_fill_stepsn(colours = pal(n_bins), ...)
}

# -------------------------------------------------------------------------
# Steps scales
# -------------------------------------------------------------------------

#' Manychat stepped colour scale
#'
#' Stepped colour scale (two colours), analogous to
#' \code{\link[ggplot2]{scale_color_steps}}.
#'
#' @inheritParams get_palette
#' @param palette Palette name; defaults to a Manychat palette.
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{scale_color_steps}}.
#' @export
scale_color_steps_manychat <- function(palette = "manychat_diverging_1",
                                       invert = FALSE,
                                       reverse = FALSE,
                                       ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  cols <- pal(2)
  ggplot2::scale_color_steps(low = cols[1], high = cols[2], ...)
}

#' @rdname scale_color_steps_manychat
#' @export
scale_fill_steps_manychat <- function(palette = "manychat_diverging_1",
                                      invert = FALSE,
                                      reverse = FALSE,
                                      ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  cols <- pal(2)
  ggplot2::scale_fill_steps(low = cols[1], high = cols[2], ...)
}

#' Manychat diverging stepped colour scale
#'
#' Diverging stepped colour scale with a midpoint, analogous to
#' \code{\link[ggplot2]{scale_color_steps2}}.
#'
#' @inheritParams get_palette
#' @param palette Palette name; defaults to a Manychat diverging palette.
#' @param midpoint The midpoint value of the scale (default 0).
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{scale_color_steps2}}.
#' @export
scale_color_steps2_manychat <- function(palette = "manychat_diverging_1",
                                        midpoint = 0,
                                        invert = FALSE,
                                        reverse = FALSE,
                                        ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  cols <- pal(3)
  ggplot2::scale_color_steps2(
    low = cols[1],
    mid = cols[2],
    high = cols[3],
    midpoint = midpoint,
    ...
  )
}

#' @rdname scale_color_steps2_manychat
#' @export
scale_fill_steps2_manychat <- function(palette = "manychat_diverging_1",
                                       midpoint = 0,
                                       invert = FALSE,
                                       reverse = FALSE,
                                       ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  cols <- pal(3)
  ggplot2::scale_fill_steps2(
    low = cols[1],
    mid = cols[2],
    high = cols[3],
    midpoint = midpoint,
    ...
  )
}

#' Manychat n-colour stepped scale
#'
#' Stepped colour scale using multiple colours from a palette, analogous to
#' \code{\link[ggplot2]{scale_color_stepsn}}.
#'
#' @inheritParams get_palette
#' @param palette Palette name; defaults to a Manychat palette.
#' @param n_bins Number of colour steps (default 6).
#' @param ... Additional arguments passed to
#'   \code{\link[ggplot2]{scale_color_stepsn}}.
#' @export
scale_color_stepsn_manychat <- function(palette = "manychat_diverging_1",
                                        n_bins = 6,
                                        invert = FALSE,
                                        reverse = FALSE,
                                        ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  ggplot2::scale_color_stepsn(colours = pal(n_bins), ...)
}

#' @rdname scale_color_stepsn_manychat
#' @export
scale_fill_stepsn_manychat <- function(palette = "manychat_diverging_1",
                                       n_bins = 6,
                                       invert = FALSE,
                                       reverse = FALSE,
                                       ...) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)
  ggplot2::scale_fill_stepsn(colours = pal(n_bins), ...)
}

# -------------------------------------------------------------------------
# Clever label functions (human-readable abbreviations)
# -------------------------------------------------------------------------

#' Human-readable number labels with K/M/B suffixes
#'
#' Label function for use with ggplot2 scales. Formats large numbers with
#' K/M/B suffixes (e.g., 1000 -> 1K, 1500000 -> 1.5M).
#'
#' @param accuracy Number to round to (e.g., 0.1 for one decimal place).
#' @param ... Additional arguments passed to \code{\link[scales]{label_number}}.
#' @return A labeling function for use with \code{labels} argument in scales.
#' @export
#' @examples
#' \dontrun{
#' ggplot(data, aes(x, y)) +
#'   geom_point() +
#'   scale_y_continuous(labels = label_clever_number())
#' }
label_clever_number <- function(accuracy = 1, ...) {

  scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = accuracy, ...)
}

#' Human-readable dollar labels with K/M/B suffixes
#'
#' Label function for use with ggplot2 scales. Formats large dollar amounts
#' with K/M/B suffixes (e.g., 1000 -> $1K, 1500000 -> $1.5M).
#'
#' @param accuracy Number to round to (e.g., 0.1 for one decimal place).
#' @param prefix Currency symbol (default "$").
#' @param suffix Optional suffix after the number.
#' @param ... Additional arguments passed to \code{\link[scales]{label_dollar}}.
#' @return A labeling function for use with \code{labels} argument in scales.
#' @export
#' @examples
#' \dontrun{
#' ggplot(data, aes(x, y)) +
#'   geom_point() +
#'   scale_y_continuous(labels = label_clever_dollar())
#' }
label_clever_dollar <- function(accuracy = 1, prefix = "$", suffix = "", ...) {
  scales::label_dollar(
    scale_cut = scales::cut_short_scale(),
    accuracy = accuracy,
    prefix = prefix,
    suffix = suffix,
    ...
  )
}

# -------------------------------------------------------------------------
# Manual helper
# -------------------------------------------------------------------------

#' Extract specific colours from a Manychat palette
#'
#' Helper function to pick specific colours by index for use with
#' \code{\link[ggplot2]{scale_color_manual}} or
#' \code{\link[ggplot2]{scale_fill_manual}}.
#'
#' @inheritParams get_palette
#' @param palette Palette name; defaults to a Manychat palette.
#' @param indices Integer vector of colour indices to extract. If \code{NULL},
#'   returns all colours from the palette.
#' @param n If \code{indices} is \code{NULL}, interpolate this many colours
#'   from the palette (default uses the palette's native colours).
#' @return A character vector of hex colour codes.
#' @export
#' @examples
#' \dontrun{
#' # Pick specific colors by index
#' manychat_colors(c(1, 3, 5))
#'
#' # Use with scale_*_manual
#' ggplot(data, aes(x, y, fill = group)) +
#'   geom_col() +
#'   scale_fill_manual(values = manychat_colors(c(1, 3)))
#'
#' # Get 4 interpolated colors
#' manychat_colors(n = 4)
#' }
manychat_colors <- function(indices = NULL,
                            palette = "manychat_diverging_1",
                            n = NULL,
                            invert = FALSE,
                            reverse = FALSE) {
  pal <- get_palette(palette, invert = invert, reverse = reverse)


  if (!is.null(indices)) {
    # Get enough colors to cover the max index requested
    max_idx <- max(indices)
    cols <- pal(max_idx)
    return(cols[indices])
  }

  if (!is.null(n)) {
    return(pal(n))
  }


  # Return native palette colors

  palettes <- .load_palettes()
  native_cols <- palettes[[palette]]$colours
  if (reverse) native_cols <- rev(native_cols)
  if (invert) native_cols <- grDevices::rgb(1 - grDevices::col2rgb(native_cols) / 255)
  native_cols
}
