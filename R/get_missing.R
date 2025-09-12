#' Check missing data patterns for individual variables
#'
#' This function analyzes missing data for a specific variable from a wide
#' data frame. It's designed to handle variables with different update
#' frequencies (annual vs every 5 years) and varying time coverage ranges.
#'
#' @param df A data frame containing the data to analyze. Must have columns
#'   named 'fips', 'year', and the variable specified in the \code{var} parameter.
#' @param var Character string specifying the name of the variable to analyze
#'   for missing data patterns.
#' @param out Logical indicating whether to return the processed data frame.
#'   If FALSE (default), only prints summary statistics. If TRUE, returns
#'   the completed data frame.
#'
#' @return If \code{out = FALSE}, returns NULL invisibly after printing
#'   missing data statistics. If \code{out = TRUE}, returns a data frame
#'   with complete combinations of fips and year, showing missing patterns
#'   for the specified variable.
#'
#' @details The function performs the following steps:
#'   \itemize{
#'     \item Selects only fips, year, and the specified variable
#'     \item Removes rows where all selected variables are NA
#'     \item Creates complete combinations of fips and year (fills in missing combinations)
#'     \item Calculates and prints summary statistics:
#'       \itemize{
#'         \item Available years (sorted and comma-separated)
#'         \item Number and percentage of missing values
#'         \item Total number of observations
#'       }
#'   }
#'
#'   This approach is particularly useful for temporal data where different
#'   variables may be collected at different intervals or time periods.
#'
#' @importFrom dplyr select all_of
#' @importFrom tidyr complete
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#' # Sample data with fips codes, years, and a variable
#' sample_data <- data.frame(
#'   fips = rep(c("01", "02", "03"), each = 5),
#'   year = rep(2018:2022, 3),
#'   population = c(100, 110, NA, 130, 140,
#'                  200, NA, 220, 230, 240,
#'                  300, 310, 320, NA, 340)
#' )
#'
#' # Check missing data patterns for population variable
#' get_missing(sample_data, "population")
#'
#' # Get the completed data frame back
#' completed_data <- get_missing(sample_data, "population", out = TRUE)
#' }
#'
#' @export
get_missing <- function(df, var, out = FALSE) {
  dat <- df %>%
    dplyr::select(fips, year, !!var) %>%
    na.omit() %>%
    tidyr::complete(fips, year)
  years <- paste0(sort(unique(dat$year)), collapse = ', ')
  n_miss <- sum(is.na(dat[[var]]))
  total <- length(dat[[var]])
  perc_miss <- round((n_miss/total) * 100, 3)
  cat('\n\nYears:', years)
  cat('\nMissing: ', n_miss, ' out of ', total, ' (', perc_miss, '%)', sep = '')
  if (out) return(dat)
}
