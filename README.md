# vintalisation

Custom palette, scale, and theme helpers for building consistent ggplot2 visuals.

## Installation

```r
# install.packages("devtools") # if needed
devtools::install_github("aalvarezperez/vintalisation")
```

For local testing before pushing to GitHub:

```r
devtools::install_local("/path/to/vintalisation", build = TRUE, upgrade = "never")
```

## Quick Walkthrough

```r
library(ggplot2)
library(vintalisation)

# 1. Inspect palettes
show_palette("adevinta_brand_1")    # plots to the viewer
get_palette("bol_brand_1")(5)       # returns raw hex codes

# 2. Use colour/fill scales
ggplot(mtcars, aes(factor(cyl), fill = factor(gear))) +
  geom_bar() +
  scale_fill_my_palette("studio_accessibility_1") +
  theme_custom("marktplaats")

# 3. Switch to dark mode
last_plot() + my_dark_mode()

# 4. Use Manychat presets
ggplot(mtcars, aes(factor(cyl), fill = factor(gear))) +
  geom_bar() +
  scale_fill_manychat() +
  theme_manychat()

# 5. Save production-ready files
make_plot("gears.png")
```

### Feature Highlights

- **Palettes** – YAML-driven definitions in `inst/palettes.yml` with helpers:
  - `get_palette()`/`show_palette()` for inspection.
  - `get_colors()` for regex/name-based extraction.
- **ggplot2 scales** – `scale_*_my_palette()` provide generic access; `scale_*_manychat()` pre-sets the partner palette defaults for quick use.
- **Themes** – `theme_custom()` (market aware), `theme_manychat()` for branded typography, plus `my_dark_mode()` for dark UIs.
- **Utilities** – `make_plot()` for reproducible exports, `watermark()` for annotation, `round_any()` for quick numeric rounding.

## Palette Naming Conventions

Every palette follows `<source>_<type>_<index>` (e.g., `manychat_diverging_1`):

- **Brand palettes**: `adevinta_brand_1`, `bol_brand_1`, etc. Use sequential indices for variations (core, neutral, heat, accent).
- **Campaign/editorial**: `adevinta_campaign_1`, `adevinta_editorial_2`.
- **Accessibility**: `studio_accessibility_1` (bright), `_2` (soft) for colorblind-safe defaults.
- **Partner data**: `manychat_diverging_1`–`3` reflect the Manychat combinations.
- **Studio diverging**: `studio_diverging_1`–`3` cover internal palettes for ordered/diverging data.

Each `meta` block in `inst/palettes.yml` stores `category`, `source`, `variant`, and `tags` to allow filtering or automation based on role.

## Adding New Palettes

1. **Edit `inst/palettes.yml`**
   ```yaml
   new_palette_name:
     colours: ["#111111", "#222222", "#333333"]
     meta: {brand: custom}
   ```
   Keep keys lowercase and supply valid hex codes.

2. **Reload/test locally**
   ```r
   devtools::load_all()
   show_palette("new_palette_name")
   ```

3. **Use immediately**
   ```r
   ggplot(df, aes(x, y, colour = group)) +
     geom_point() +
     scale_color_my_palette("new_palette_name")
   ```

Manychat typography requires the bundled fonts. The package automatically registers them via `sysfonts` + `showtext` when you call `theme_manychat()`.

No additional R code changes are needed—the package automatically loads every palette defined in the YAML file during `.onLoad()`.

## License

MIT
