#' Custom minimal theme
#'
#' @param market  One of "adevinta","marktplaats","gumtree","kijiji","2dehands","manychat"
#' @param base_size  Base font size
#' @param base_family Base font family (defaults to "DM Sans")
#' @param heading_family Font family for titles/subtitles (defaults to
#'   `base_family`).
#' @param bg_fill Plot background colour
#' @param blank_plot_bg Logical; if `TRUE`, draw the plot background using
#'   `bg_fill`, otherwise keep it blank.
#' @param legend_position Position of the legend: "bottom", "top", "left",
#'   "right", or "none".
#' @param show_y_axis Logical; if `TRUE`, display the Y-axis line.
#' @param y_axis_angle Angle for y-axis title (default 0 for horizontal,
#'   use 90 for vertical when labels are long).
#' @export
theme_custom <- function(market = "adevinta",
                         base_size = 24,
                         base_family = "DM Sans",
                         heading_family = base_family,
                         bg_fill = "#FFFFFF",
                         blank_plot_bg = TRUE,
                         legend_position = "bottom",
                         show_y_axis = FALSE,
                         y_axis_angle = 0) {

  .check_font_available(base_family)
  if (heading_family != base_family) .check_font_available(heading_family)

  textcol <- .market_text_colour(market)
  plot_bg <- .plot_background(blank_plot_bg, bg_fill)
  .apply_base_theme(base_family, heading_family, base_size, textcol, plot_bg,
                    legend_position, show_y_axis, y_axis_angle)
}

#' Check if font is available
#' @keywords internal
.check_font_available <- function(font_family) {
  # Basic R fonts and sysfonts-registered fonts
  basic_fonts <- c("sans", "serif", "mono")
  sysfonts_fonts <- tryCatch(sysfonts::font_families(), error = function(e) character(0))


  # Check system fonts if systemfonts package is available

  system_fonts <- tryCatch({
    if (requireNamespace("systemfonts", quietly = TRUE)) {
      unique(systemfonts::system_fonts()$family)
    } else {
      character(0)
    }
  }, error = function(e) character(0))

  available <- font_family %in% c(basic_fonts, sysfonts_fonts, system_fonts)

  if (!available) {
    warning(
      "Font '", font_family, "' not found. ",
      "Text may render with a fallback font.",
      call. = FALSE
    )
  }
  invisible(available)
}

.market_text_colour <- function(market) {
  text_cols <- c(
    adevinta    = "#1d1f2a",
    marktplaats = "#2D3C4D",
    gumtree     = "#3c3241",
    kijiji      = "#3E4153",
    `2dehands`  = "#00285A",
    manychat    = "#1F1F1F"
  )

  if (!market %in% names(text_cols)) stop("Unknown market: ", market, call. = FALSE)
  text_cols[[market]]
}

.plot_background <- function(blank_plot_bg, bg_fill) {
  if (blank_plot_bg) {
    ggplot2::element_rect(fill = bg_fill, colour = bg_fill)
  } else {
    ggplot2::element_blank()
  }
}

#' Derive a lighter gridline colour from text colour
#' @keywords internal
.gridline_colour <- function(textcol) {
  rgb_vals <- grDevices::col2rgb(textcol) / 255
  lighter <- rgb_vals + (1 - rgb_vals) * 0.7
  grDevices::rgb(lighter[1], lighter[2], lighter[3])
}

.apply_base_theme <- function(base_family, heading_family, base_size, textcol, plot_bg,
                              legend_position = "bottom", show_y_axis = FALSE,
                              y_axis_angle = 0) {
  y_axis_line <- if (show_y_axis) {
    ggplot2::element_line(colour = textcol, linewidth = .35)
  } else {
    ggplot2::element_blank()
  }

  y_title_hjust <- if (y_axis_angle == 0) 1 else 0.5
  y_title_vjust <- if (y_axis_angle == 0) 1 else 0.5

  legend_dir <- if (legend_position %in% c("bottom", "top")) "horizontal" else "vertical"
  gridline_col <- .gridline_colour(textcol)

  ggthemes::theme_tufte(base_family = base_family, base_size = base_size) %+replace%
    ggplot2::theme(
      text = ggplot2::element_text(colour = textcol, size = base_size),
      plot.title = ggplot2::element_text(
        family = heading_family,
        face = "bold",
        size = base_size * 1.6,
        lineheight = 1.1,
        margin = ggplot2::margin(b = base_size * 0.5)
      ),
      plot.subtitle = ggplot2::element_text(
        family = heading_family,
        size = base_size * 1.2,
        lineheight = 1.1,
        margin = ggplot2::margin(b = base_size * 0.4)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.75,
        hjust = 1,
        colour = "gray40",
        lineheight = 1.1,
        margin = ggplot2::margin(t = base_size * 0.5)
      ),
      plot.tag = ggplot2::element_text(
        size = base_size * 1.2,
        face = "bold",
        margin = ggplot2::margin(b = base_size * 0.3, r = base_size * 0.3)
      ),
      axis.title = ggplot2::element_text(
        size = base_size * 1.05,
        lineheight = 1.0
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = base_size * 0.4)
      ),
      axis.title.y = ggplot2::element_text(
        angle = y_axis_angle,
        hjust = y_title_hjust,
        vjust = y_title_vjust,
        margin = ggplot2::margin(r = base_size * 0.4)
      ),
      axis.text = ggplot2::element_text(size = base_size * 0.9),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(t = base_size * 0.2)
      ),
      axis.text.y = ggplot2::element_text(
        hjust = 0,
        margin = ggplot2::margin(r = base_size * 0.2)
      ),
      legend.title = ggplot2::element_text(
        size = base_size,
        margin = ggplot2::margin(b = base_size * 0.15)
      ),
      legend.text = ggplot2::element_text(size = base_size * 0.85),
      legend.position = legend_position,
      legend.direction = legend_dir,
      legend.key.size = grid::unit(base_size * 0.8, "pt"),
      legend.key.spacing = grid::unit(base_size * 0.25, "pt"),
      legend.key.spacing.x = grid::unit(base_size * 0.35, "pt"),
      legend.margin = ggplot2::margin(
        t = base_size * 0.4,
        r = base_size * 0.1,
        b = base_size * 0.1,
        l = base_size * 0.1
      ),
      legend.box.spacing = grid::unit(base_size * 0.5, "pt"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = gridline_col, linewidth = .25),
      axis.ticks = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = textcol, linewidth = .35),
      axis.line.y = y_axis_line,
      plot.background = plot_bg,
      strip.text = ggplot2::element_text(
        size = base_size * 0.95,
        face = "bold",
        margin = ggplot2::margin(b = base_size * 0.3)
      ),
      strip.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(
        t = base_size,
        r = base_size,
        b = base_size,
        l = base_size
      ),
      panel.spacing = grid::unit(base_size * 0.7, "pt")
    )
}

#' Register bundled Manychat fonts
#' @keywords internal
register_manychat_fonts <- function() {
  registered <- sysfonts::font_families()
  if ("Manychat Gravity" %in% registered && "Rooftop" %in% registered) {
    return(invisible(NULL))
  }

  fonts_dir <- system.file("fonts", package = "vintalisation")
  regular <- file.path(fonts_dir, "ManychatGravity.otf")
  heading_regular <- file.path(fonts_dir, "Rooftop-Regular.otf")
  heading_bold <- file.path(fonts_dir, "Rooftop-Bold.otf")

  if (file.exists(regular) && !"Manychat Gravity" %in% registered) {
    sysfonts::font_add(family = "Manychat Gravity", regular = regular)
  }
  if (file.exists(heading_regular) && !"Rooftop" %in% registered) {
    sysfonts::font_add(
      family = "Rooftop",
      regular = heading_regular,
      bold = if (file.exists(heading_bold)) heading_bold else heading_regular
    )
  }
  showtext::showtext_auto()
  invisible(NULL)
}

#' Manychat branded theme
#'
#' @inheritParams theme_custom
#' @export
theme_manychat <- function(market = "manychat",
                           base_size = 24,
                           base_family = "Rooftop",
                           heading_family = "Manychat Gravity",
                           bg_fill = "#FFFFFF",
                           blank_plot_bg = TRUE,
                           legend_position = "bottom",
                           show_y_axis = FALSE,
                           y_axis_angle = 0) {
  register_manychat_fonts()
  theme_custom(
    market = market,
    base_size = base_size,
    base_family = base_family,
    heading_family = heading_family,
    bg_fill = bg_fill,
    blank_plot_bg = blank_plot_bg,
    legend_position = legend_position,
    show_y_axis = show_y_axis,
    y_axis_angle = y_axis_angle
  )
}

#' Dark-mode wrapper for themes
#'
#' @param .theme A ggplot2 theme to convert to dark mode.
#' @param verbose Logical; print messages produced by `ggdark::dark_mode()`.
#' @param force_geom_invert Logical; passed to `ggdark::dark_mode()`.
#' @param black_bg Logical; if `TRUE`, keep the background black.
#' @return A ggplot2 theme with dark-mode adjustments.
#' @export
my_dark_mode <- function(.theme = ggplot2::theme_get(),
                         verbose = TRUE,
                         force_geom_invert = FALSE,
                         black_bg = FALSE) {
  th <- ggdark::dark_mode(.theme, verbose = verbose,
                          force_geom_invert = force_geom_invert)
  if (!black_bg) th <- th + ggplot2::theme(plot.background = ggplot2::element_blank())
  th
}
