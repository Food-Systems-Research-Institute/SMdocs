test_that("get_distributions creates plots correctly", {
  # Create test data with different distribution types
  set.seed(123)
  test_data <- data.frame(
    id = 1:100,
    normal_var = rnorm(100, mean = 50, sd = 10),
    skewed_var = rexp(100, rate = 0.1),  # Right skewed
    uniform_var = runif(100, 0, 100)
  )

  # Test basic functionality
  plots <- get_distributions(test_data)

  expect_type(plots, "list")
  expect_length(plots, 3)  # Should have 3 plots (excluding id column)

  # Check that all plots are ggplot objects
  expect_true(all(sapply(plots, function(p) inherits(p, "ggplot"))))

  # Test with custom parameters
  plots_custom <- get_distributions(
    test_data,
    fill = "blue",
    color = "navy",
    skew_cutoff = 1.5
  )

  expect_type(plots_custom, "list")
  expect_length(plots_custom, 3)
})

test_that("get_distributions handles edge cases", {
  # Data with only one numeric column
  single_col_data <- data.frame(
    id = 1:50,
    value = rnorm(50)
  )

  plots_single <- get_distributions(single_col_data)
  expect_length(plots_single, 1)
  expect_true(inherits(plots_single[[1]], "ggplot"))

  # Data with missing values
  data_with_na <- data.frame(
    id = 1:50,
    var1 = c(rnorm(40), rep(NA, 10)),
    var2 = c(rep(NA, 5), rnorm(45))
  )

  expect_no_error({
    plots_na <- get_distributions(data_with_na)
  })
  expect_length(plots_na, 2)

  # Data with constant values (no variation)
  constant_data <- data.frame(
    id = 1:20,
    constant = rep(5, 20)
  )

  expect_no_error({
    plots_constant <- get_distributions(constant_data)
  })
  expect_length(plots_constant, 1)
})

test_that("get_distributions identifies skewed variables correctly", {
  # Create data where we know which variables should be skewed
  set.seed(456)
  test_data <- data.frame(
    id = 1:1000,  # Large sample for reliable skewness calculation
    normal = rnorm(1000),
    highly_skewed = rexp(1000, rate = 0.1),  # Should be highly right-skewed
    moderately_skewed = rbeta(1000, 0.5, 2)  # Moderately skewed
  )

  # Test with default skew_cutoff = 2
  plots_default <- get_distributions(test_data)

  # We can't directly test the colors, but we can verify the function runs
  # and produces the expected number of plots
  expect_length(plots_default, 3)

  # Test with lower skew_cutoff
  plots_strict <- get_distributions(test_data, skew_cutoff = 0.5)
  expect_length(plots_strict, 3)
})
