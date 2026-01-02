library(ggplot2)

test_that("get_palette returns a palette function", {
  pal <- get_palette("main")

  expect_true(is.function(pal))
  expect_length(pal(3), 3)
})

test_that("scale_color_adevinta returns ggplot2 scales", {
  discrete_scale <- scale_color_adevinta("main", discrete = TRUE)
  continuous_scale <- scale_color_adevinta("main", discrete = FALSE)

  expect_true(inherits(discrete_scale, "ScaleDiscrete"))
  expect_true(inherits(continuous_scale, "ScaleContinuous"))
})

test_that("theme_adevinta returns a ggplot2 theme", {
  theme <- theme_adevinta()

  expect_true(inherits(theme, "theme"))
  expect_true(inherits(theme$plot.background, "element"))
})
