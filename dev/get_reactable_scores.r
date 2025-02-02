pacman::p_load(
  dplyr,
  reactable,
  stringr,
  htmltools
)

get_reactable_scores <- function(scores_list, method_name) {
  
  # Get DF to display
  df <- scores_list[[method_name]]$dimension_scores %>% 
    select(state, everything()) %>% 
    mutate(across(where(is.numeric), ~ format(round(.x, 3), nsmall = 3)))
  
  reactable(
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
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    highlight = TRUE,
    style = list(fontSize = "14px"),
    compact = TRUE,
    fullWidth = TRUE
  )
}