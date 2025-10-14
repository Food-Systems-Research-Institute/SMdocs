test_that("get_dimension_ggraph creates ggraph with data frame input", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("igraph")
  skip_if_not_installed("paletteer")

  framework_df <- data.frame(
    dimension = c("economic", "economic", "social", "social"),
    index = c("index1", "index1", "index2", "index2"),
    indicator = c("indicator1", "indicator2", "indicator3", "indicator4"),
    stringsAsFactors = FALSE
  )

  result <- get_dimension_ggraph(
    framework_df = framework_df,
    dimension_in = "economic"
  )

  expect_s3_class(result, "ggplot")
})

test_that("get_dimension_ggraph works with metrics included", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("igraph")
  skip_if_not_installed("paletteer")

  framework_df <- data.frame(
    dimension = c("social", "social"),
    index = c("health", "health"),
    indicator = c("mortality", "morbidity"),
    metric = c("infant_mortality", "disease_rate"),
    stringsAsFactors = FALSE
  )

  result <- get_dimension_ggraph(
    framework_df = framework_df,
    dimension_in = "social",
    include_metrics = TRUE
  )

  expect_s3_class(result, "ggplot")
})

test_that("get_dimension_ggraph handles custom parameters", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("igraph")
  skip_if_not_installed("paletteer")

  framework_df <- data.frame(
    dimension = "economic",
    index = "productivity",
    indicator = "gdp",
    stringsAsFactors = FALSE
  )

  expect_no_error({
    result <- get_dimension_ggraph(
      framework_df = framework_df,
      dimension_in = "economic",
      leaf_font_size = 3
    )
  })
})

test_that("get_dimension_ggraph_slim creates slim version", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("igraph")
  skip_if_not_installed("paletteer")

  framework_df <- data.frame(
    Dimension = c("environmental", "environmental"),
    Index = c("quality", "quality"),
    Indicator = c("air_quality", "water_quality"),
    stringsAsFactors = FALSE
  )

  result <- get_dimension_ggraph_slim(
    framework_df = framework_df,
    dimension_in = "environmental"
  )

  expect_s3_class(result, "ggplot")
})
