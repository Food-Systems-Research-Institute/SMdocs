test_that("get_res_plots works with linear model", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  
  expect_no_error({
    get_res_plots(model)
  })
})

test_that("get_res_plots works with glm model", {
  model <- glm(am ~ mpg + hp, data = mtcars, family = binomial)
  
  expect_no_error({
    get_res_plots(model)
  })
})

test_that("get_res_plots resets par correctly", {
  model <- lm(mpg ~ wt, data = mtcars)
  
  # Check that par is reset to single plot after function
  get_res_plots(model)
  current_par <- par("mfrow")
  
  expect_equal(current_par, c(1, 1))
})