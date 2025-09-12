test_that("get_indicator_distributions creates plot grid", {
  skip_if_not_installed("psych")
  skip_if_not_installed("ggpubr")
  
  scores_list <- list(
    test_transform = list(
      indicator_scores = data.frame(
        state = c("CA", "TX", "NY", "FL"),
        indicator1 = c(0.1, 0.5, 0.8, 0.3),
        indicator2 = c(0.9, 0.2, 0.7, 0.4),
        indicator3 = c(0.3, 0.6, 0.1, 0.9),
        stringsAsFactors = FALSE
      )
    )
  )
  
  result <- get_indicator_distributions(
    scores_list,
    "test_transform",
    rows = 2,
    columns = 2
  )
  
  expect_s3_class(result, "ggplot")
})

test_that("get_indicator_distributions filters aggregates correctly", {
  skip_if_not_installed("psych")
  skip_if_not_installed("ggpubr")
  
  scores_list <- list(
    method1 = list(
      indicator_scores = data.frame(
        state = c("CA", "TX", "US_mean", "NE_median", "FL"),
        metric1 = c(1, 2, 1.5, 1.2, 3),
        metric2 = c(5, 6, 5.5, 5.1, 7)
      )
    )
  )
  
  expect_no_error({
    result <- get_indicator_distributions(scores_list, "method1", rows = 1, columns = 2)
  })
})

test_that("get_indicator_distributions handles single indicator", {
  skip_if_not_installed("psych")
  skip_if_not_installed("ggpubr")
  
  scores_list <- list(
    simple = list(
      indicator_scores = data.frame(
        state = c("CA", "TX", "NY"),
        single_indicator = c(0.2, 0.5, 0.8)
      )
    )
  )
  
  result <- get_indicator_distributions(scores_list, "simple", rows = 1, columns = 1)
  expect_s3_class(result, "ggplot")
})