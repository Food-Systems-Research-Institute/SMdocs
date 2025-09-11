#' Create a reactable table for dimension scores
#'
#' This function creates a specialized reactable table for displaying dimension
#' scores from a scores list. It extracts the dimension scores for a specific
#' method, formats numeric columns to 3 decimal places, and creates an
#' interactive table with predefined styling.
#'
#' @param scores_list A named list containing score data, where each element
#'   corresponds to a method and contains a `dimension_scores` data frame.
#' @param method_name Character string specifying the name of the method
#'   to extract from the scores_list. This should correspond to a named
#'   element in scores_list that contains dimension_scores.
#'
#' @return A reactable htmlwidget object displaying the formatted dimension
#'   scores with interactive features including sorting, filtering, searching,
#'   and pagination.
#'
#' @details The function performs the following operations:
#'   \itemize{
#'     \item Extracts dimension_scores from the specified method in scores_list
#'     \item Reorders columns to put 'state' first
#'     \item Formats all numeric columns to display 3 decimal places
#'     \item Creates an interactive reactable table with compact styling
#'     \item Sets default page size to 5 rows
#'   }
#'
#'   The resulting table includes standard interactive features like sorting,
#'   filtering, searching, and pagination, with a compact layout optimized
#'   for displaying numerical score data.
#'
#' @importFrom dplyr select everything mutate across where
#' @importFrom reactable reactable
#'
#' @examples
#' \dontrun{
#' # Assuming you have a scores_list with method results
#' scores_list <- list(
#'   pca = list(
#'     dimension_scores = data.frame(
#'       state = c("CA", "TX", "NY"),
#'       economic = c(0.456, 0.123, 0.789),
#'       social = c(0.234, 0.567, 0.890)
#'     )
#'   )
#' )
#'
#' # Create reactable for PCA method scores
#' get_reactable_scores(scores_list, "pca")
#' }
#'
#' @export
get_reactable_scores <- function(scores_list, method_name) {
 
  # Get DF to display
  df <- scores_list[[method_name]]$dimension_scores %>% 
    dplyr::select(state, dplyr::everything()) %>% 
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ format(round(.x, 3), nsmall = 3)))
  
  reactable::reactable(
    df,
    sortable = TRUE,
    resizable = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    pagination = TRUE,
    bordered = TRUE,
    wrap = TRUE,
    rownames = FALSE,
    onClick = 'select',
    striped = TRUE,
    defaultPageSize = 5,
    showPageSizeOptions = TRUE,
    highlight = TRUE,
    style = list(fontSize = "14px"),
    compact = TRUE,
    fullWidth = TRUE
  )
}