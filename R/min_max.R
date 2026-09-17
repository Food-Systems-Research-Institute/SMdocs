#' Min-max normalization from 0 to 100
#'
#' This function performs min-max normalization on a numeric vector, scaling
#' values to range from 0 to 100. The transformation preserves the relative
#' relationships between values while standardizing the scale.
#'
#' @param x A numeric vector to be normalized. Missing values (NA) are handled
#'   appropriately and will remain as NA in the output.
#'
#' @return A numeric vector of the same length as \code{x}, with values
#'   normalized to the range 0 to 100. The minimum value in \code{x} becomes 0,
#'   the maximum becomes 100, and all other values are scaled proportionally.
#'   NA values in the input remain as NA in the output.
#'
#' @details The min-max normalization formula used is:
#'   \deqn{normalized = \frac{x - \min(x)}{\max(x) - \min(x)} \times 100}
#'
#'   The function handles missing values by excluding them from min/max
#'   calculations using \code{na.rm = TRUE}. If all values are NA or if
#'   min equals max, the function will still work appropriately.
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' # Basic usage
#' values <- c(10, 20, 30, 40, 50)
#' min_max(values)
#' # Returns: [1]  0 25 50 75 100
#'
#' # With missing values
#' values_na <- c(1, 5, NA, 10, 15)
#' min_max(values_na)
#' # Returns: [1]  0.00 28.57    NA 64.29 100.00
#'
#' # With identical values
#' identical_values <- c(5, 5, 5, 5)
#' min_max(identical_values)
#' # Returns: [1] NaN NaN NaN NaN (since max - min = 0)
#'
#' @export
min_max <- function(x) {
  assertthat::assert_that(length(x) > 1, msg = 'Vector must have length > 1')
  if (any(is.na(x))) {
    n_nas <- sum(is.na(x))
    warning(paste('Input contains', n_nas, 'NAs. These will be removed.'))
  }

  normed <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  normed * 100
}
