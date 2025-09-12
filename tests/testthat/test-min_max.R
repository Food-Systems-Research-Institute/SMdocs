test_that("min_max normalizes values correctly", {
  # Basic functionality
  x <- c(10, 20, 30, 40, 50)
  result <- min_max(x)
  expect_equal(result, c(0, 25, 50, 75, 100))

  # Identical values
  expect_equal(min_max(c(5, 5, 5)), c(NaN, NaN, NaN))

  # With NA values
  x_na <- c(1, 5, NA, 10, 15)
  expect_warning(result_na <- min_max(x_na))
  expect_equal(result_na[1], 0)
  expect_equal(result_na[5], 100)
  expect_true(is.na(result_na[3]))

  # Negative values
  x_neg <- c(-10, 0, 10)
  result_neg <- min_max(x_neg)
  expect_equal(result_neg, c(0, 50, 100))
})

test_that("min_max handles edge cases", {
  # Error with empty vector
  expect_error(min_max(numeric(0)))

  # Error with single value
  expect_error(min_max(5))

  # Warning with NAs
  expect_warning(min_max(c(1, 2, NA)))
  expect_warning(min_max(c(1, 2, NaN)))

  # Works with NULLs
  expect_no_error(min_max(c(1, 2, NULL)))
})
