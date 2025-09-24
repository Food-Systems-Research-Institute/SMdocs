#' Prepare data for regression figures
#'
#' This function takes the DF of model outputs (variable_name, coefficient, p
#' values, outcomes...) and formats it for regression graphs. Returns formatted
#' data as well as a lookup to match "indicator-metric" label to dimension to
#' add colors.
#'
#' @param model_output_df A data frame of model outputs, containing dimension,
#'   indicator, and metric columns.
#'
#' @return A data frame with title-case combined "indicator-metric" labels,
#'   dimensions ordered alphabetically, and estimates arranged within each
#'   dimension. Also a label-dimension lookup to add colors.
#'
#' @details The function converts indicator and metric column values to title
#'   case and creates a combined label for indicator and metrics, orders
#'   dimensions alphabetically, makes the ordered dimensions a factor, arranges
#'   by dimension and then by estimate, and creates a lookup to later add
#'   dimension colors to labels.
#'
#' @importFrom dplyr mutate arrange distinct ungroup %>%
#' @importFrom snakecase to_title_case
#'
#' @examples
#' \dontrun{
#' # Sample data frame
#' df <- data.frame(
#'   dimension = c("d1", "d2", "d3", "d4"),
#'   indicator = c("earth", "wind", "fire", "music"),
#'   metric = c("soil", "air", "heat", "notes"),
#'   estimate = c(0.01, 0.05, -0.2, 0.07)
#' )
#'
#' # Prepare data for regression figure
#' prepared_data <- prepare_figure_data(df)
#' }
#'
#' @export
prepare_figure_data <- function(model_output_df) {
  # Title case and combine labels
  figure_data <- model_output_df %>%
    mutate(
      indicator = snakecase::to_title_case(indicator),
      metric = snakecase::to_title_case(metric),
      combined_label = paste(indicator, "-", metric)
    )

  # Order dimensions alphabetically
  ordered_dims <- sort(unique(figure_data$dimension))

  # Arrange and filter
  figure_data <- figure_data %>%
    mutate(
      dimension = factor(dimension, levels = ordered_dims),
      estimate = as.numeric(estimate)
    ) %>%
    arrange(dimension, estimate) %>%
    mutate(
      combined_label = factor(combined_label, levels = rev(unique(combined_label)))
    ) %>%
    ungroup()

  # Create label-dimension lookup
  label_dimension_lookup <- figure_data %>%
    distinct(combined_label, dimension)

  # Return both processed data and label-dimension
  list(
    figure_data = figure_data,
    label_dimension_lookup = label_dimension_lookup
  )
}
