test_that("get_vt_spiders creates spider charts", {
  skip_if_not_installed("fmsb")
  
  df_list <- list(
    minmax_arithmetic = list(
      dimension_scores = data.frame(
        economic = c(50, 45, 55),
        social = c(50, 60, 40),
        environmental = c(50, 70, 35),
        governance = c(50, 55, 45),
        infrastructure = c(50, 40, 65),
        state = c("US_median", "VT", "CA")
      )
    ),
    minmax_geometric = list(
      dimension_scores = data.frame(
        economic = c(40, 35, 45),
        social = c(40, 50, 30),
        environmental = c(40, 60, 25),
        governance = c(40, 45, 35),
        infrastructure = c(40, 30, 55),
        state = c("US_median", "VT", "CA")
      )
    )
  )
  
  expect_no_error({
    get_vt_spiders(df_list, "minmax")
  })
})

test_that("get_single_spider creates single chart", {
  skip_if_not_installed("fmsb")
  
  df <- data.frame(
    economic = c(50, 45, 55),
    social = c(50, 60, 40),
    environmental = c(50, 70, 35),
    governance = c(50, 55, 45),
    infrastructure = c(50, 40, 65),
    state = c("US_median", "VT", "CA")
  )
  
  expect_no_error({
    get_single_spider(df, "Test Vermont Chart")
  })
})

test_that("spider functions reset par correctly", {
  skip_if_not_installed("fmsb")
  
  df_list <- list(
    test_arithmetic = list(
      dimension_scores = data.frame(
        dim1 = c(50, 45), dim2 = c(50, 60), dim3 = c(50, 70), 
        dim4 = c(50, 55), dim5 = c(50, 40),
        state = c("US_median", "VT")
      )
    ),
    test_geometric = list(
      dimension_scores = data.frame(
        dim1 = c(40, 35), dim2 = c(40, 50), dim3 = c(40, 60), 
        dim4 = c(40, 45), dim5 = c(40, 30),
        state = c("US_median", "VT")
      )
    )
  )
  
  get_vt_spiders(df_list, "test")
  current_par <- par("mfrow")
  
  expect_equal(current_par, c(1, 1))
})