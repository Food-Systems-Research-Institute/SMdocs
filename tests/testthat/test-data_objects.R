test_that("dp_meta data object exists and has expected structure", {
  expect_true(exists("dp_meta"))
  expect_s3_class(dp_meta, "data.frame")
})

test_that("dp_metrics data object exists and has expected structure", {
  expect_true(exists("dp_metrics"))
  expect_s3_class(dp_metrics, "data.frame")
  
  # Check expected columns based on documentation
  expected_cols <- c("fips", "year", "variable_name", "value")
  expect_true(all(expected_cols %in% names(dp_metrics)))
})