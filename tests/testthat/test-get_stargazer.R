test_that("get_stargazer creates stargazer output with basic model", {
  skip_if_not_installed("stargazer")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  # Create test data and assign to global environment so update() can find it
  test_data <<- data.frame(
    outcome = rnorm(100),
    dim1 = rnorm(100),
    dim2 = rnorm(100),
    gdp_per_cap = rnorm(100, 50000, 10000),
    population = runif(100, 10000, 1000000)
  )

  model <- lm(outcome ~ dim1 + dim2, data = test_data)

  expect_no_error({
    result <- get_stargazer(model, dep_var = "Test Outcome", type = "text")
  })
  
  # Clean up
  rm(test_data, envir = .GlobalEnv)
})

test_that("get_stargazer works with robust standard errors", {
  skip_if_not_installed("stargazer")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  # Create test data and assign to global environment so update() can find it
  test_data <<- data.frame(
    outcome = rnorm(50),
    dim1 = rnorm(50),
    gdp_per_cap = rnorm(50, 50000, 10000),
    population = runif(50, 10000, 1000000)
  )

  model <- lm(outcome ~ dim1, data = test_data)

  expect_no_error({
    result <- get_stargazer(model, dep_var = "Test", robust = TRUE, type = "text")
  })
  
  # Clean up
  rm(test_data, envir = .GlobalEnv)
})

test_that("get_stargazer handles custom column labels", {
  skip_if_not_installed("stargazer")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  # Create test data and assign to global environment so update() can find it
  test_data <<- data.frame(
    outcome = rnorm(30),
    dim1 = rnorm(30),
    gdp_per_cap = rnorm(30, 50000, 10000),
    population = runif(30, 10000, 1000000)
  )

  model <- lm(outcome ~ dim1, data = test_data)
  custom_labels <- c("Model A", "Model B", "Model C")

  expect_no_error({
    result <- get_stargazer(
      model,
      dep_var = "Test",
      column_labels = custom_labels,
      type = "text"
    )
  })
  
  # Clean up
  rm(test_data, envir = .GlobalEnv)
})
