test_that("get_reactable creates reactable object with defaults", {
  test_data <- data.frame(
    name = c("Alice", "Bob", "Charlie"),
    age = c(25, 30, 35),
    score = c(85.5, 92.3, 78.9)
  )

  result <- get_reactable(test_data)

  expect_s3_class(result, "reactable")
  expect_s3_class(result, "htmlwidget")
})

test_that("get_reactable works with custom parameters", {
  test_data <- data.frame(x = 1:5, y = letters[1:5])

  result <- get_reactable(
    test_data,
    sortable = FALSE,
    searchable = FALSE,
    defaultPageSize = 3
  )

  expect_s3_class(result, "reactable")
})

test_that("get_reactable handles empty data frame", {
  empty_df <- data.frame()

  expect_error(get_reactable(empty_df), class = "assertError")
})

test_that("get_reactable handles single row data", {
  single_row <- data.frame(col1 = "value", col2 = 42)

  result <- get_reactable(single_row)
  expect_s3_class(result, "reactable")
})
