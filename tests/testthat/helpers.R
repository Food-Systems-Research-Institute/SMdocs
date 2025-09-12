# Helper functions and sample data for testing

# Sample sustainability metrics data
sample_metrics <- data.frame(
  fips = rep(c("50001", "50003", "50005"), each = 5),
  year = rep(2018:2022, 3),
  variable_name = rep(c("economic_health", "environmental_quality", "social_wellbeing"), each = 5),
  value = c(
    # Economic health: increasing trend
    45, 50, 55, 60, 65,
    # Environmental quality: decreasing trend  
    80, 75, 70, 65, NA,
    # Social wellbeing: mixed with missing
    60, NA, 65, 70, 72
  ),
  stringsAsFactors = FALSE
)

# Sample wide-format data for testing transformations
sample_wide <- data.frame(
  fips = c("50001", "50003", "50005"),
  county = c("Addison", "Bennington", "Caledonia"),
  pop_2020 = c(37363, 37347, 30233),
  pop_2021 = c(37200, 37100, 30000),
  income_2020 = c(65000, 58000, 52000),
  income_2019 = c(63000, 56000, 50000),
  stringsAsFactors = FALSE
)

# Sample data with different year patterns
sample_mixed_years <- data.frame(
  fips = rep(c("50001", "50003"), each = 8),
  year = rep(c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), 2),
  variable_name = rep(c("annual_metric", "five_year_metric"), each = 8),
  value = c(
    # Annual metric - complete
    10, 11, 12, 13, 14, 15, 16, 17,
    # Five year metric - every 5 years
    100, NA, NA, NA, NA, 105, NA, NA
  ),
  stringsAsFactors = FALSE
)

# Function to create test data with specified missingness pattern
create_test_data <- function(n_counties = 3, n_years = 5, missing_prob = 0.1) {
  counties <- paste0("50", sprintf("%03d", 1:n_counties))
  years <- (2023 - n_years + 1):2023
  
  expand.grid(
    fips = counties,
    year = years,
    stringsAsFactors = FALSE
  ) |>
    within({
      value <- rnorm(length(fips), mean = 50, sd = 15)
      # Introduce missing values
      value[sample(length(value), size = floor(length(value) * missing_prob))] <- NA
    })
}