test_that("get_latest_year filters to latest year correctly", {
  # Sample data
  df <- data.frame(
    variable_name = c("var1", "var1", "var2", "var2", "var3"),
    year = c(2020, 2021, 2019, 2021, 2022),
    value = c(10, 15, 20, 25, 30),
    other_col = letters[1:5]
  )
  
  # Test with suffix (default)
  result_suffix <- get_latest_year(df)
  
  expect_equal(nrow(result_suffix), 3)
  expect_true(all(c("var1_2021", "var2_2021", "var3_2022") %in% result_suffix$variable_name))
  expect_false("year" %in% colnames(result_suffix))
  expect_equal(result_suffix$value[result_suffix$variable_name == "var1_2021"], 15)
  expect_equal(result_suffix$value[result_suffix$variable_name == "var2_2021"], 25)
  
  # Test without suffix
  result_no_suffix <- get_latest_year(df, add_suffix = FALSE)
  
  expect_equal(nrow(result_no_suffix), 3)
  expect_true(all(c("var1", "var2", "var3") %in% result_no_suffix$variable_name))
  expect_false("year" %in% colnames(result_no_suffix))
  expect_equal(result_no_suffix$value[result_no_suffix$variable_name == "var1"], 15)
  
  # Test with custom column names
  df_custom <- data.frame(
    metric = c("metric1", "metric1", "metric2"),
    yr = c(2020, 2021, 2020),
    score = c(100, 200, 300)
  )
  
  result_custom <- get_latest_year(df_custom, var_col = "metric", year_col = "yr")
  expect_equal(nrow(result_custom), 2)
  expect_true(all(c("metric1_2021", "metric2_2020") %in% result_custom$metric))
})

test_that("get_latest_year handles edge cases", {
  # Single row
  df_single <- data.frame(
    variable_name = "var1",
    year = 2020,
    value = 100
  )
  
  result_single <- get_latest_year(df_single)
  expect_equal(nrow(result_single), 1)
  expect_equal(result_single$variable_name, "var1_2020")
  
  # Multiple years same variable
  df_multi <- data.frame(
    variable_name = rep("var1", 5),
    year = 2018:2022,
    value = 1:5
  )
  
  result_multi <- get_latest_year(df_multi)
  expect_equal(nrow(result_multi), 1)
  expect_equal(result_multi$variable_name, "var1_2022")
  expect_equal(result_multi$value, 5)
  
  # Character years (should be converted)
  df_char_year <- data.frame(
    variable_name = c("var1", "var1"),
    year = c("2020", "2021"),
    value = c(10, 20)
  )
  
  result_char <- get_latest_year(df_char_year)
  expect_equal(result_char$variable_name, "var1_2021")
  expect_equal(result_char$value, 20)
})