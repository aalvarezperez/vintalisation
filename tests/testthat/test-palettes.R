test_that("get_palette returns a function", {
  pal <- get_palette("adevinta_brand_1")
  expect_type(pal, "closure")
})

test_that("get_palette function returns correct number of colors", {
  pal <- get_palette("adevinta_brand_1")
  expect_length(pal(5), 5)
  expect_length(pal(10), 10)
})

test_that("get_palette returns valid hex colors", {
  pal <- get_palette("adevinta_brand_1")
  colors <- pal(5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
})

test_that("get_palette reverse argument reverses colors", {
  pal_normal <- get_palette("adevinta_brand_1")
  pal_reversed <- get_palette("adevinta_brand_1", reverse = TRUE)
  colors_normal <- pal_normal(3)
  colors_reversed <- pal_reversed(3)
  expect_equal(colors_normal[1], colors_reversed[3])
  expect_equal(colors_normal[3], colors_reversed[1])
})

test_that("get_palette invert argument inverts colors", {
  pal_normal <- get_palette("adevinta_brand_1")
  pal_inverted <- get_palette("adevinta_brand_1", invert = TRUE)
  colors_normal <- pal_normal(1)
  colors_inverted <- pal_inverted(1)
  expect_false(colors_normal == colors_inverted)
})
test_that("get_palette errors on unknown palette", {
  expect_error(get_palette("nonexistent_palette"))
})

test_that("show_palette returns palette names when palette is NULL", {
  result <- show_palette(palette = NULL)
  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_true("adevinta_brand_1" %in% result)
})

test_that("show_palette returns ggplot object", {
  result <- show_palette("adevinta_brand_1", plot = FALSE)
  expect_s3_class(result, "ggplot")
})

test_that("get_colors returns colors for valid palette names", {
  colors <- get_colors(palette_names = "adevinta_brand_1")
  expect_type(colors, "character")
  expect_true(length(colors) > 0)
})

test_that("get_colors returns colors matching pattern", {
  colors <- get_colors(pattern = "^adevinta")
  expect_type(colors, "character")
  expect_true(length(colors) > 0)
})

test_that("get_colors errors on unknown palette", {
  expect_error(get_colors(palette_names = "nonexistent"))
})

test_that("get_colors returns all colors when no args provided", {
  all_colors <- get_colors()
  expect_type(all_colors, "character")
  expect_true(length(all_colors) > 10)
})

test_that("list_palettes returns all palettes by default", {
  result <- list_palettes()
  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_true("adevinta_brand_1" %in% result)
})

test_that("list_palettes filters by source", {
  result <- list_palettes(source = "manychat")
  expect_type(result, "character")
  expect_true(all(grepl("manychat", result)))
})

test_that("list_palettes filters by category", {
  result <- list_palettes(category = "accessibility")
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("list_palettes filters by tags", {
  result <- list_palettes(tags = "colorblind_safe")
  expect_type(result, "character")
  expect_true(length(result) > 0)
})
