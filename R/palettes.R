#' Built-in palettes list
#' @keywords internal
palettes_list <- list(
  main   = get_colors(c("sky_blue","jungle","lavender","orange","lime","pink","purple")),
  bol    = get_colors(pattern = "^bol_"),
  marktplaats = get_colors(pattern = "^marktplaats_"),
  colorblind  = get_colors(pattern = "^cb_")
)

# -------------------------------------------------------------------------

#' Create a palette function
#' @inheritParams get_palette
#' @export
get_palette <- function(palette = "main", invert = FALSE, reverse = FALSE, ...) {
  stopifnot(palette %in% names(palettes_list))
  pal <- palettes_list[[palette]]
  if (reverse) pal <- rev(pal)
  if (invert)  pal <- grDevices::rgb(1-col2rgb(pal)/255)
  grDevices::colorRampPalette(pal, ...)
}

# -------------------------------------------------------------------------

#' Show a palette in the viewer
#' @inheritParams get_palette
#' @param plot Logical; draw palette?
#' @export
show_palette <- function(palette = NULL, reverse = FALSE, plot = TRUE) {
  if (is.null(palette)) return(names(palettes_list))
  pal <- get_palette(palette, reverse = reverse)(length(palettes_list[[palette]]))
  if (plot) scales::show_col(pal)
  invisible(pal)
}