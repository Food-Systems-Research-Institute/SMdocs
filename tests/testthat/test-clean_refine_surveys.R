test_that("clean_refine_surveys processes survey data correctly", {
  survey_tables <- list(
    must_include = data.frame(
      indicator = c("GDP", "Employment", "Education"),
      freq = c(15, 10, 20),
      score = c(45, 30, 60)
    ),
    probably_include = data.frame(
      indicator = c("GDP", "Employment", "Education"),
      freq = c(5, 15, 5),
      score = c(10, 30, 10)
    ),
    maybe_include = data.frame(
      indicator = c("GDP", "Employment", "Education"),
      freq = c(5, 0, 0),
      score = c(5, 0, 0)
    )
  )
  
  result <- clean_refine_surveys(
    tables = survey_tables,
    scope = "indicator",
    cat_must = "must_include",
    cat_probably = "probably_include",
    n_votes = 25
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true("Indicator" %in% names(result))
  expect_true("Score" %in% names(result))
  expect_true("Proportion Must Include" %in% names(result))
  expect_true("Proportion Must OR Probably Include" %in% names(result))
})

test_that("clean_refine_surveys works with index scope", {
  survey_tables <- list(
    cat1 = data.frame(
      index = c("Economic", "Social"),
      freq = c(10, 15),
      score = c(30, 45)
    ),
    cat2 = data.frame(
      index = c("Economic", "Social"),
      freq = c(5, 10),
      score = c(10, 20)
    )
  )
  
  result <- clean_refine_surveys(
    tables = survey_tables,
    scope = "index",
    cat_must = "cat1",
    cat_probably = "cat2",
    n_votes = 25
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("Index" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("clean_refine_surveys handles missing values correctly", {
  survey_tables <- list(
    high = data.frame(
      indicator = c("A", "B"),
      freq = c(10, 5),
      score = c(30, 15)
    ),
    med = data.frame(
      indicator = c("A"),  # B is missing
      freq = c(5),
      score = c(10)
    )
  )
  
  result <- clean_refine_surveys(
    tables = survey_tables,
    scope = "indicator",
    cat_must = "high",
    cat_probably = "med",
    n_votes = 15
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})