test_that("min_max normalizes values correctly", {
  # Basic functionality
  x <- c(10, 20, 30, 40, 50)
  result <- min_max(x)
  expect_equal(result, c(0, 25, 50, 75, 100))

  # Single value
  expect_equal(min_max(5), NaN)

  # Identical values
  expect_equal(min_max(c(5, 5, 5)), c(NaN, NaN, NaN))

  # With NA values
  x_na <- c(1, 5, NA, 10, 15)
  result_na <- min_max(x_na)
  expect_equal(result_na[1], 0)
  expect_equal(result_na[5], 100)
  expect_true(is.na(result_na[3]))

  # Negative values
  x_neg <- c(-10, 0, 10)
  result_neg <- min_max(x_neg)
  expect_equal(result_neg, c(0, 50, 100))
})

test_that("min_max handles edge cases", {
  # Empty vector
  expect_equal(min_max(numeric(0)), numeric(0))

  # Very large numbers
  x_large <- c(1e10, 2e10, 3e10)
  result_large <- min_max(x_large)
  expect_equal(result_large, c(0, 50, 100))

  # Very small differences
  x_small <- c(1.0000001, 1.0000002, 1.0000003)
  result_small <- min_max(x_small)
  expect_equal(result_small, c(0, 50, 100))
})
