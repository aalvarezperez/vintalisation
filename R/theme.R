#' Adevinta minimal theme
#'
#' @param market  One of "adevinta","marktplaats","gumtree","kijiji","2dehands"
#' @param base_size  Base font size
#' @param base_family Base font family
#' @param bg_fill Plot background colour
#' @export
theme_adevinta <- function(market = "adevinta",
                           base_size = 18 / .pt,
                           base_family = "DM Sans",
                           bg_fill = "#FFFFFF",
                           blank_plot_bg = TRUE) {
  
  text_cols <- c(
    adevinta    = "#1d1f2a",
    marktplaats = "#2D3C4D",
    gumtree     = "#3c3241",
    kijiji      = "#3E4153",
    `2dehands`  = "#00285A"
  )
  textcol <- text_cols[[market]]
  
  PLOT_BG <- if (blank_plot_bg) ggplot2::element_rect(fill = bg_fill, colour = bg_fill)
  else ggplot2::element_blank()
  
  ggthemes::theme_tufte(base_family = base_family, base_size = base_size) %+replace%
    ggplot2::theme(
      text            = ggplot2::element_text(colour = textcol, size = base_size),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey80", linewidth = .25),
      axis.ticks      = ggplot2::element_blank(),
      axis.line.x     = ggplot2::element_line(colour = textcol, linewidth = .35),
      axis.line.y     = ggplot2::element_blank(),
      plot.background = PLOT_BG,
      legend.key.size = grid::unit(7, "pt")
    )
}

#' Dark-mode wrapper (thin wrapper around {ggdark})
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