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
#' @inheritParams get_palette
#' @param plot Logical; draw palette?
#' @export
show_palette <- function(palette = NULL, reverse = FALSE, plot = TRUE) {
  if (is.null(palette)) return(names(palettes_list))
  pal <- get_palette(palette, reverse = reverse)(length(palettes_list[[palette]]))
  if (plot) scales::show_col(pal)
  invisible(pal)
}
