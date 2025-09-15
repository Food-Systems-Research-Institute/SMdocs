#' Filter data to retain only the latest year for each variable
#'
#' This function filters a data frame so that each unique variable only has
#' data from its latest available year. Optionally appends the year as a suffix
#' to the variable name.
#'
#' @param df A data frame containing the data to filter
#' @param var_col Character string specifying the column name containing variable names.
#'   Default is 'variable_name'
#' @param year_col Character string specifying the column name containing years.
#'   Default is 'year'
#' @param add_suffix Logical indicating whether to add the year as a suffix to the
#'   variable name. Default is TRUE
#'
#' @return A data frame filtered to contain only the latest year for each variable.
#'   If \code{add_suffix = TRUE}, variable names will have the year appended.
#'   The year column is removed from the output.
#'
#' @details The function converts the year column to numeric, identifies the maximum
#'   year for each variable, filters to retain only those rows, and optionally
#'   modifies variable names to include the year suffix before removing the year column.
#'
#' @importFrom dplyr filter mutate select bind_rows pull
#' @importFrom purrr map
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Sample data
#' df <- data.frame(
#'   variable_name = c("var1", "var1", "var2", "var2"),
#'   year = c(2020, 2021, 2019, 2021),
#'   value = c(10, 15, 20, 25)
#' )
#'
#' # Get latest year with suffix
#' get_latest_year(df)
#'
#' # Get latest year without suffix
#' get_latest_year(df, add_suffix = FALSE)
#' }
#'
#' @export
get_latest_year <- function(df,
                           var_col = 'variable_name',
                           year_col = 'year',
                           add_suffix = TRUE
                           ){

  # Make sure that year is numeric
  df <- dplyr::mutate(df, {{ year_col }} := as.numeric(.data[[year_col]]))

  # Get unique variable names
  vars <- unique(df[[var_col]])

  # Filter to latest year for each variable
  filtered_df <- purrr::map(vars, \(var) {
    unique_years <- df %>%
      dplyr::filter(.data[[var_col]] == var) %>%
      pull({{ year_col }}) %>%
      unique()
    out <- df %>%
      dplyr::filter(
        .data[[var_col]] == var,
        .data[[year_col]] == max(unique_years)
      )
    if (add_suffix == TRUE) {
      out <- out %>%
        dplyr::mutate({{ var_col }} := paste0(.data[[var_col]], '_', .data[[year_col]]))
    }
    out <- out %>%
      dplyr::select(-{{ year_col }})
    return(out)
  }) %>%
    dplyr::bind_rows()

  return(filtered_df)
}
