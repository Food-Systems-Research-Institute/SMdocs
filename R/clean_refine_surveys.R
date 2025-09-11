#' Clean and refine survey data to create scoring tables
#'
#' This function takes a list of survey tables with frequencies and scores,
#' combines them into a comprehensive scoring table with proportions of
#' different response categories and weighted scores.
#'
#' @param tables A named list of data frames, where each data frame contains
#'   survey response frequencies and scores. Each data frame should have
#'   columns for the scope variable (indicator or index), frequencies, and scores.
#' @param scope Character vector of length 1 specifying the scope level.
#'   Must be either 'indicator' or 'index'. This determines which column
#'   to group by in the analysis.
#' @param cat_must Character string specifying the category name for responses
#'   that indicate "must include" (highest priority responses).
#' @param cat_probably Character string specifying the category name for responses
#'   that indicate "probably include" (moderate priority responses).
#' @param n_votes Numeric specifying the total number of votes/responses,
#'   used to calculate proportions.
#'
#' @return A data frame with the following columns:
#'   \itemize{
#'     \item Scope column (Indicator/Index): Names of indicators or indices
#'     \item Score: Weighted sum of scores across all categories
#'     \item Proportion Must Include: Proportion of "must include" responses
#'     \item Proportion Must OR Probably Include: Combined proportion of
#'       "must include" and "probably include" responses
#'   }
#'   
#'   The data frame is sorted in descending order by score, with numeric
#'   proportions formatted to 2 decimal places.
#'
#' @details The function performs several key operations:
#'   \itemize{
#'     \item Combines all survey tables and adds category identifiers
#'     \item Calculates proportion of "must include" responses for each item
#'     \item Calculates combined proportion of "must" + "probably include"
#'     \item Computes weighted scores by summing across all categories
#'     \item Joins all metrics together and formats the output
#'     \item Handles missing values by setting them to 0
#'     \item Formats proportions to 2 decimal places for readability
#'   }
#'
#' @importFrom purrr imap
#' @importFrom dplyr bind_rows mutate select filter group_by summarize
#'   arrange full_join across where
#' @importFrom stringr str_to_sentence
#'
#' @examples
#' \dontrun{
#' # Sample survey tables
#' survey_tables <- list(
#'   category1 = data.frame(
#'     indicator = c("GDP", "Employment", "Education"),
#'     freq = c(15, 10, 20),
#'     score = c(45, 30, 60)
#'   ),
#'   category2 = data.frame(
#'     indicator = c("GDP", "Employment", "Education"),
#'     freq = c(5, 15, 5),
#'     score = c(10, 30, 10)
#'   )
#' )
#'
#' # Clean and refine the survey data
#' results <- clean_refine_surveys(
#'   tables = survey_tables,
#'   scope = "indicator",
#'   cat_must = "category1",
#'   cat_probably = "category2",
#'   n_votes = 25
#' )
#' }
#'
#' @export
clean_refine_surveys <- function(
    tables, # list
    scope = c('indicator', 'index'),
    cat_must,
    cat_probably,
    n_votes
) {

  # Add category to tables
  props <- tables %>% 
    purrr::imap(~ .x %>% dplyr::mutate(cat = .y)) %>% 
    dplyr::bind_rows() %>% 
    dplyr::select(-score)

  # Get proportion of probably include OR must include
  prop_prob_or_must_include <- props %>% 
    dplyr::filter(cat %in% c(cat_must, cat_probably)) %>% 
    # group_by(scope) %>% 
    dplyr::group_by(.[[scope]]) %>%
    dplyr::summarize(prop_include = sum(freq) / n_votes) %>% 
    dplyr::arrange(desc(prop_include))
  
  # Get proportion of must include
  prop_must_include <- props %>% 
    dplyr::filter(cat == cat_must) %>% 
    dplyr::group_by(.[[scope]]) %>%
    dplyr::summarize(prop_must = sum(freq) / n_votes) %>% 
    dplyr::arrange(desc(prop_must))
  
  # Add up weighted scores
  scores <- tables %>% 
    dplyr::bind_rows() %>% 
    dplyr::group_by(.[[scope]]) %>%
    dplyr::summarize(score = sum(score, na.rm = TRUE)) %>% 
    dplyr::arrange(desc(score))
  
  # Join everything together
  scores_table <- scores %>%
    dplyr::full_join(prop_must_include) %>%
    dplyr::full_join(prop_prob_or_must_include) %>%
    dplyr::arrange(desc(score)) %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.na(.x), 0, .x)), 
      dplyr::across(c(3:4), ~ format(round(.x, 2), nsmall = 2))
    ) %>%
    setNames(
      c(
        stringr::str_to_sentence(scope),
        'Score',
        'Proportion Must Include',
        'Proportion Must OR Probably Include'
      )
    )
  
  return(scores_table)
}