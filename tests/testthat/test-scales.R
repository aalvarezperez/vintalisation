# Test discrete scales
test_that("scale_color_my_palette returns a ggplot scale", {
  scale <- scale_color_my_palette("adevinta_brand_1", discrete = TRUE)
  expect_s3_class(scale, "Scale")
  expect_s3_class(scale, "ggproto")
})

test_that("scale_fill_my_palette returns a ggplot scale", {
  scale <- scale_fill_my_palette("adevinta_brand_1", discrete = TRUE)
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_my_palette continuous returns gradientn scale", {
  scale <- scale_color_my_palette("adevinta_brand_1", discrete = FALSE)
  expect_s3_class(scale, "Scale")
})

test_that("scale_fill_my_palette continuous returns gradientn scale", {
  scale <- scale_fill_my_palette("adevinta_brand_1", discrete = FALSE)
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_my_palette accepts reverse argument", {
  scale <- scale_color_my_palette("adevinta_brand_1", reverse = TRUE)
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_my_palette accepts invert argument", {
  scale <- scale_color_my_palette("adevinta_brand_1", invert = TRUE)
  expect_s3_class(scale, "Scale")
})

# Test Manychat scales
test_that("scale_color_manychat returns a ggplot scale", {
  scale <- scale_color_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_fill_manychat returns a ggplot scale", {
  scale <- scale_fill_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_manychat accepts custom palette", {
  scale <- scale_color_manychat(palette = "manychat_diverging_1")
  expect_s3_class(scale, "Scale")
})

# Test gradient scales
test_that("scale_color_gradient_manychat returns a continuous scale", {
  scale <- scale_color_gradient_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_fill_gradient_manychat returns a continuous scale", {
  scale <- scale_fill_gradient_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_gradient2_manychat returns a diverging scale", {
  scale <- scale_color_gradient2_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_fill_gradient2_manychat returns a diverging scale", {
  scale <- scale_fill_gradient2_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_gradient2_manychat accepts midpoint", {
  scale <- scale_color_gradient2_manychat(midpoint = 0.5)
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_gradientn_manychat returns a gradientn scale", {
  scale <- scale_color_gradientn_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_fill_gradientn_manychat returns a gradientn scale", {
  scale <- scale_fill_gradientn_manychat()
  expect_s3_class(scale, "Scale")
})

# Test binned scales
test_that("scale_color_binned_manychat returns a binned scale", {
  scale <- scale_color_binned_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_fill_binned_manychat returns a binned scale", {
  scale <- scale_fill_binned_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_binned_manychat accepts n_bins argument", {
  scale <- scale_color_binned_manychat(n_bins = 4)
  expect_s3_class(scale, "Scale")
})

# Test steps scales
test_that("scale_color_steps_manychat returns a steps scale", {
  scale <- scale_color_steps_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_fill_steps_manychat returns a steps scale", {
  scale <- scale_fill_steps_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_steps2_manychat returns a diverging steps scale", {
  scale <- scale_color_steps2_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_fill_steps2_manychat returns a diverging steps scale", {
  scale <- scale_fill_steps2_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_steps2_manychat accepts midpoint", {
  scale <- scale_color_steps2_manychat(midpoint = 100)
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_stepsn_manychat returns a stepsn scale", {
  scale <- scale_color_stepsn_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_fill_stepsn_manychat returns a stepsn scale", {
  scale <- scale_fill_stepsn_manychat()
  expect_s3_class(scale, "Scale")
})

test_that("scale_color_stepsn_manychat accepts n_bins argument", {
  scale <- scale_color_stepsn_manychat(n_bins = 8)
  expect_s3_class(scale, "Scale")
})

# Test manychat_colors helper
test_that("manychat_colors returns hex colors", {
  colors <- manychat_colors()
  expect_type(colors, "character")
  expect_true(all(grepl("^#", colors)))
})

test_that("manychat_colors with indices returns specific colors", {
  colors <- manychat_colors(indices = c(1, 3))
  expect_length(colors, 2)
})

test_that("manychat_colors with n returns interpolated colors", {
  colors <- manychat_colors(n = 5)
  expect_length(colors, 5)
})

test_that("manychat_colors accepts palette argument", {
  colors <- manychat_colors(palette = "manychat_diverging_1")
  expect_type(colors, "character")
})

test_that("manychat_colors accepts reverse argument", {
  colors_normal <- manychat_colors(n = 3)
  colors_reversed <- manychat_colors(n = 3, reverse = TRUE)
  expect_equal(colors_normal[1], colors_reversed[3])
})

test_that("manychat_colors accepts invert argument", {
  colors_normal <- manychat_colors(n = 1)
  colors_inverted <- manychat_colors(n = 1, invert = TRUE)
  expect_false(colors_normal == colors_inverted)
})
