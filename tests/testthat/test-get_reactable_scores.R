test_that("get_reactable_scores extracts and formats scores correctly", {
  scores_list <- list(
    method1 = list(
      dimension_scores = data.frame(
        state = c("CA", "TX", "NY"),
        economic = c(0.12345, 0.67890, 0.11111),
        social = c(0.98765, 0.43210, 0.55555),
        stringsAsFactors = FALSE
      )
    )
  )
  
  result <- get_reactable_scores(scores_list, "method1")
  
  expect_s3_class(result, "reactable")
  expect_s3_class(result, "htmlwidget")
})

test_that("get_reactable_scores handles method name correctly", {
  scores_list <- list(
    pca = list(
      dimension_scores = data.frame(
        state = c("CA", "TX"),
        value1 = c(1.234567, 2.345678),
        value2 = c(3.456789, 4.567890)
      )
    ),
    factor_analysis = list(
      dimension_scores = data.frame(
        state = c("FL", "GA"),
        score = c(0.123456, 0.987654)
      )
    )
  )
  
  result_pca <- get_reactable_scores(scores_list, "pca")
  result_fa <- get_reactable_scores(scores_list, "factor_analysis")
  
  expect_s3_class(result_pca, "reactable")
  expect_s3_class(result_fa, "reactable")
})

test_that("get_reactable_scores handles single state data", {
  scores_list <- list(
    test_method = list(
      dimension_scores = data.frame(
        state = "CA",
        economic = 0.5,
        social = 0.7
      )
    )
  )
  
  result <- get_reactable_scores(scores_list, "test_method")
  expect_s3_class(result, "reactable")
})