# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**vintalisation** is an R package providing custom palette, scale, and theme helpers for ggplot2 visualizations. It uses a YAML-driven palette system with support for multiple brands (Adevinta, Marktplaats, Gumtree, Kijiji, Manychat).

## Development Commands

```r
# Load package for development (run from project root)
devtools::load_all()

# Rebuild documentation after editing Roxygen comments
devtools::document()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-palettes.R")

# Build and check package
devtools::check()

# Build vignettes
devtools::build_vignettes()
```

## Architecture

### YAML-Driven Palette System
- All palettes are defined in `inst/palettes.yml` with metadata (category, source, variant, tags)
- Palettes are loaded once via `.load_palettes()` and cached in `.palettes_cache` (see `R/palettes.R`)
- `get_palette()` returns a `colorRampPalette()` function for interpolation

### Module Organization

| File | Purpose |
|------|---------|
| `R/palettes.R` | Palette loading, inspection (`get_palette()`, `show_palette()`, `get_colors()`) |
| `R/scales.R` | ggplot2 scale functions (`scale_color_my_palette()`, `scale_fill_manychat()`) |
| `R/theme.R` | Theme definitions built on `ggthemes::theme_tufte()` with `%+replace%` pattern |
| `R/colors.R` | Color wheel utilities and harmony functions (complementary, tetradic, etc.) |
| `R/utils.R` | Export helpers (`make_plot()`) and utilities (`round_any()`, `watermark()`) |
| `R/zzz.R` | `.onLoad()` hook for initialization |

### Key Patterns
- **Theme composition**: Themes extend `theme_tufte()` using `%+replace%` operator
- **Font registration**: Custom fonts in `inst/fonts/` are registered via sysfonts/showtext at load time
- **Factory functions**: `get_palette()` returns palette functions, not raw color vectors

### Adding New Palettes
Add entries to `inst/palettes.yml` following the existing schema:
```yaml
palette_name:
  colours: ["#hex1", "#hex2", ...]
  meta:
    category: brand|editorial|accessibility
    source: adevinta|marktplaats|manychat|...
    variant: core|neutral|heat|accent
    tags: [tag1, tag2]
```

No R code changes required - palettes are loaded dynamically from YAML.
