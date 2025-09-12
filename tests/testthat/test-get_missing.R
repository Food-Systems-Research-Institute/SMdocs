test_that("get_missing analyzes missing data correctly", {
  # Sample data with missing values
  df <- data.frame(
    fips = rep(c("01", "02", "03"), each = 3),
    year = rep(2020:2022, 3),
    population = c(100, NA, 120, 200, 210, NA, 300, 310, 320)
  )

  # Test without output (default behavior)
  # Capture the printed output
  output <- capture.output({
    result <- get_missing(df, "population")
  })

  expect_null(result)
  expect_true(any(grepl("Years:", output)))
  expect_true(any(grepl("Missing:", output)))

  # Test with output = TRUE
  result_with_output <- get_missing(df, "population", out = TRUE)

  expect_s3_class(result_with_output, "data.frame")
  expect_true("fips" %in% colnames(result_with_output))
  expect_true("year" %in% colnames(result_with_output))
  expect_true("population" %in% colnames(result_with_output))

  # Check that complete combinations are created
  expected_rows <- length(unique(df$fips)) * length(unique(df$year))
  expect_equal(nrow(result_with_output), expected_rows)

  # Test missing value count
  n_missing <- sum(is.na(result_with_output$population))
  expect_true(n_missing >= 2)  # At least the original 2 NAs
})

test_that("get_missing handles edge cases", {
  # Data with no missing values
  df_complete <- data.frame(
    fips = rep(c("01", "02"), each = 2),
    year = rep(2020:2021, 2),
    metric = c(10, 20, 30, 40)
  )

  result_complete <- get_missing(df_complete, "metric", out = TRUE)
  expect_equal(sum(is.na(result_complete$metric)), 0)

  # Data with all missing values for the variable
  df_all_na <- data.frame(
    fips = c("01", "02"),
    year = c(2020, 2021),
    metric = c(NA, NA)
  )

  # This should handle the case where all values are NA after na.omit
  expect_warning(
    result_all_na <- get_missing(df_all_na, "metric", out = TRUE),
    NA  # We expect no warnings, but if there are any, that's also acceptable
  )

  # Single row of data
  df_single <- data.frame(
    fips = "01",
    year = 2020,
    value = 100
  )

  result_single <- get_missing(df_single, "value", out = TRUE)
  expect_equal(nrow(result_single), 1)
  expect_equal(result_single$value, 100)
})
