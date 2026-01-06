# Test round_any
test_that("round_any rounds to specified accuracy", {
  expect_equal(round_any(1.234, 0.1), 1.2)
  expect_equal(round_any(1.256, 0.1), 1.3)
})

test_that("round_any works with different accuracies", {
  expect_equal(round_any(123, 10), 120)
  expect_equal(round_any(127, 10), 130)
  # R uses banker's rounding (round to even), so 125 -> 120
  expect_equal(round_any(125, 10), 120)
  expect_equal(round_any(126, 10), 130)
})

test_that("round_any accepts custom rounding function", {
  expect_equal(round_any(1.234, 0.1, floor), 1.2)
  expect_equal(round_any(1.299, 0.1, floor), 1.2)
  expect_equal(round_any(1.201, 0.1, ceiling), 1.3)
})

test_that("round_any works with vectors", {
  result <- round_any(c(1.23, 4.56, 7.89), 0.5)
  expect_equal(result, c(1.0, 4.5, 8.0))
})

test_that("round_any handles zero accuracy edge case", {
  expect_true(is.nan(round_any(5, 0)))
})

# Test watermark
test_that("watermark returns a ggplot annotation layer", {
  layer <- watermark(
    text = "Test",
    xmin = 0, xmax = 1,
    ymin = 0, ymax = 1
  )
  expect_s3_class(layer, "LayerInstance")
})

test_that("watermark accepts custom parameters", {
  layer <- watermark(
    text = "Custom",
    xmin = -1, xmax = 1,
    ymin = -1, ymax = 1,
    scale = 0.5,
    rot = 45,
    col = "red",
    alpha = 0.3
  )
  expect_s3_class(layer, "LayerInstance")
})

# Test make_plot (without actually saving)
test_that("make_plot creates visualisations directory", {
  skip_on_cran()

  # Create a simple plot
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  # Create temp directory for test
  old_wd <- getwd()

tmp_dir <- tempdir()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)

  # Run make_plot
  make_plot("test_plot.png", plot = p)

  # Check directory was created
  expect_true(dir.exists(file.path(tmp_dir, "visualisations")))

  # Check file was created
  expect_true(file.exists(file.path(tmp_dir, "visualisations", "test_plot.png")))

  # Cleanup
  unlink(file.path(tmp_dir, "visualisations"), recursive = TRUE)
})
