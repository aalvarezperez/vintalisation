#' Cached palette metadata
#' @keywords internal
.palettes_cache <- NULL

.palettes_file <- function() {
  path <- system.file("palettes.yml", package = "vintalisation")
  if (!nzchar(path) || !file.exists(path)) {
    candidates <- c(
      file.path(getwd(), "inst", "palettes.yml"),
      file.path(getwd(), "R", "palettes.yml")
    )
    path <- ""
    for (candidate in candidates) {
      if (file.exists(candidate)) {
        path <- candidate
        break
      }
    }
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("palettes.yml file not found. Please ensure it is bundled with the package.", call. = FALSE)
  }
  path
}

.load_palettes <- function(force = FALSE) {
  if (force || is.null(.palettes_cache)) {
    .palettes_cache <<- yaml::read_yaml(.palettes_file())
  }
  .palettes_cache
}

#' Retrieve palette colours by name or pattern
#' @param palette_names Character vector with explicit palette identifiers.
#' @param pattern Optional regular expression that selects palettes by name.
#' @return A character vector with concatenated hex colours.
#' @export
get_colors <- function(palette_names = NULL, pattern = NULL) {
  palettes <- .load_palettes()
  available <- names(palettes)
  selected <- character(0)

  if (!is.null(palette_names)) {
    missing <- setdiff(palette_names, available)
    if (length(missing)) {
      stop(
        "Unknown palette(s): ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    selected <- palette_names
  }

  if (!is.null(pattern)) {
    selected <- union(selected, grep(pattern, available, value = TRUE))
  }

  if (!length(selected)) {
    selected <- available
  }

  unlist(
    lapply(palettes[selected], function(entry) entry$colours),
    use.names = FALSE
  )
}

#' Built-in palettes list
#' @keywords internal
palettes_list <- local({
  palettes <- .load_palettes()
  lapply(palettes, function(entry) entry$colours)
})

# -------------------------------------------------------------------------

#' List available palettes with optional filtering
#'
#' Returns palette names filtered by metadata attributes (source, category,
#' variant, or tags).
#'
#' @param source Filter by source (e.g., "adevinta", "manychat", "studio").
#' @param category Filter by category (e.g., "brand", "editorial", "accessibility").
#' @param variant Filter by variant (e.g., "core", "neutral", "accent").
#' @param tags Filter by tags (palettes must have at least one matching tag).
#' @return Character vector of palette names matching the filters.
#' @export
#' @examples
#' list_palettes()
#' list_palettes(source = "manychat")
#' list_palettes(category = "accessibility")
#' list_palettes(tags = "colorblind_safe")
list_palettes <- function(source = NULL, category = NULL, variant = NULL, tags = NULL) {
  palettes <- .load_palettes()

  matches <- vapply(names(palettes), function(name) {
    meta <- palettes[[name]]$meta
    if (is.null(meta)) return(TRUE)

    if (!is.null(source) && !identical(meta$source, source)) return(FALSE)
    if (!is.null(category) && !identical(meta$category, category)) return(FALSE)
    if (!is.null(variant) && !identical(meta$variant, variant)) return(FALSE)
    if (!is.null(tags)) {
      palette_tags <- meta$tags
      if (is.null(palette_tags) || !any(tags %in% palette_tags)) return(FALSE)
    }
    TRUE
  }, logical(1))

  names(palettes)[matches]
}

#' Create a palette function
#'
#' @param palette Character scalar naming one of the palettes defined in
#'   `palettes.yml`.
#' @param invert Logical; invert colours via `1 - R/G/B`.
#' @param reverse Logical; reverse the colour order.
#' @param ... Additional arguments passed to `grDevices::colorRampPalette()`.
#' @export
get_palette <- function(palette = "adevinta_brand_1", invert = FALSE, reverse = FALSE, ...) {
  stopifnot(palette %in% names(palettes_list))
  pal <- palettes_list[[palette]]
  if (reverse) pal <- rev(pal)
  if (invert)  pal <- grDevices::rgb(1-col2rgb(pal)/255)
  grDevices::colorRampPalette(pal, ...)
}

# -------------------------------------------------------------------------

#' Show a palette in the viewer
#'
#' Displays a palette as colored swatches. When `palette` is NULL, returns
#' available palette names. Returns a ggplot object for better RStudio
#' integration.
#'
#' @inheritParams get_palette
#' @param plot Logical; if TRUE, print the plot immediately. If FALSE, just
#'   return the ggplot object without printing.
#' @return If `palette` is NULL, returns palette names. Otherwise returns a
#'   ggplot object (invisibly if `plot = TRUE`).
#' @export
#' @examples
#' show_palette("adevinta_brand_1")
#' p <- show_palette("manychat_diverging_1", plot = FALSE)
show_palette <- function(palette = NULL, reverse = FALSE, plot = TRUE) {
  if (is.null(palette)) return(names(palettes_list))

  pal <- get_palette(palette, reverse = reverse)(length(palettes_list[[palette]]))
  n <- length(pal)

  df <- data.frame(
    x = seq_len(n),
    colour = factor(pal, levels = pal)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = 1, fill = .data$colour)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::labs(title = palette) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "none"
    )

  if (plot) {
    print(p)
    invisible(p)
  } else {
    p
  }
}
