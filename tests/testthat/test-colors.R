# Test setColors
test_that("setColors returns correct number of colors", {
  colors <- setColors("tomato", 12)
  expect_length(colors, 12)
})

test_that("setColors returns valid hex colors", {
  colors <- setColors("red", 6)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
})

test_that("setColors works with hex input", {
  colors <- setColors("#FF6347", 8)
  expect_length(colors, 8)
  expect_true(all(grepl("^#", colors)))
})

test_that("setColors preserves transparency", {
  colors <- setColors("#FF6347AA", 4)
  expect_length(colors, 4)
  expect_true(all(nchar(colors) == 9))
})

# Test col2HSV
test_that("col2HSV returns hex color", {
  result <- col2HSV("tomato")
  expect_type(result, "character")
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", result))
})

test_that("col2HSV works with multiple colors", {
  result <- col2HSV(c("red", "green", "blue"))
  expect_length(result, 3)
})

test_that("col2HSV works with hex input", {
  result <- col2HSV("#FF6347")
  expect_type(result, "character")
})

# Test adjacent/analogous
test_that("adjacent returns 3 colors", {
  colors <- adjacent("tomato", plot = FALSE)
  expect_length(colors, 3)
})

test_that("adjacent returns valid hex colors", {
  colors <- adjacent("red", plot = FALSE)
  expect_true(all(grepl("^#", colors)))
})

test_that("analogous is alias for adjacent", {
  adj_colors <- adjacent("blue", plot = FALSE)
  ana_colors <- analogous("blue", plot = FALSE)
  expect_equal(adj_colors, ana_colors)
})

# Test complementary/opposite
test_that("complementary returns 2 colors", {
  colors <- complementary("tomato", plot = FALSE)
  expect_length(colors, 2)
})

test_that("complementary returns valid hex colors", {
  colors <- complementary("red", plot = FALSE)
  expect_true(all(grepl("^#", colors)))
})

test_that("opposite is alias for complementary", {
  comp_colors <- complementary("green", plot = FALSE)
  opp_colors <- opposite("green", plot = FALSE)
  expect_equal(comp_colors, opp_colors)
})

# Test tetradic
test_that("tetradic returns 4 colors", {
  colors <- tetradic("tomato", plot = FALSE)
  expect_length(colors, 4)
})

test_that("tetradic returns valid hex colors", {
  colors <- tetradic("red", plot = FALSE)
  expect_true(all(grepl("^#", colors)))
})

# Test pizza (basic functionality without plotting)
test_that("pizza runs without error", {
  expect_no_error(pizza(c("#FF0000", "#00FF00", "#0000FF")))
})
