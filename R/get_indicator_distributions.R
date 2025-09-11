#' Create density plots for indicator score distributions
#'
#' This function generates a grid of density plots showing the distributions
#' of indicator scores for a specified transformation method. Highly skewed
#' indicators (|skew| > 2) are highlighted in red, while normal distributions
#' are shown in blue.
#'
#' @param scores_list A named list containing score data, where each element
#'   corresponds to a transformation method and contains an `indicator_scores`
#'   data frame.
#' @param transformation Character string specifying the transformation method
#'   name to extract from scores_list (e.g., "minmax_arithmetic", "zscore_geometric").
#' @param rows Numeric specifying the number of rows in the plot grid.
#'   Default is 10.
#' @param columns Numeric specifying the number of columns in the plot grid.
#'   Default is 4.
#'
#' @return A ggplot object containing the arranged density plots. The plot can
#'   be displayed or saved using standard ggplot2 methods.
#'
#' @details The function performs the following steps:
#'   \itemize{
#'     \item Extracts indicator scores for the specified transformation
#'     \item Removes aggregate entries (US_mean, US_median, NE_mean, NE_median)
#'     \item Calculates skewness for each indicator using psych::describe()
#'     \item Identifies highly skewed indicators (absolute skew > 2)
#'     \item Creates density plots with color coding:
#'       \itemize{
#'         \item Red fill/dark red border: Highly skewed indicators
#'         \item Light blue fill/royal blue border: Normal indicators
#'       }
#'     \item Arranges plots in a grid using ggpubr::ggarrange()
#'   }
#'
#' @importFrom dplyr filter select pull
#' @importFrom psych describe
#' @importFrom tibble rownames_to_column
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot aes geom_density theme_classic theme unit sym
#' @importFrom ggpubr ggarrange
#'
#' @examples
#' \dontrun{
#' # Assuming you have a scores list with indicator scores
#' scores_list <- list(
#'   minmax_arithmetic = list(
#'     indicator_scores = data.frame(
#'       state = c("CA", "TX", "NY", "US_median"),
#'       indicator1 = c(0.2, 0.8, 0.5, 0.5),
#'       indicator2 = c(0.9, 0.1, 0.3, 0.4),
#'       indicator3 = c(0.4, 0.6, 0.7, 0.6)
#'     )
#'   )
#' )
#'
#' # Create distribution plots for minmax arithmetic transformation
#' plot_grid <- get_indicator_distributions(
#'   scores_list,
#'   "minmax_arithmetic",
#'   rows = 2,
#'   columns = 2
#' )
#' print(plot_grid)
#' }
#'
#' @export
get_indicator_distributions <- function(scores_list,
                                        transformation,
                                        rows = 10,
                                        columns = 4) {
  # Pull the indicators at desired transformation
  # Also remove averages, new england
  df <- scores_list[[transformation]]$indicator_scores %>% 
    dplyr::filter(!state %in% c('US_mean', 'US_median', 'NE_mean', 'NE_median'))
  
  # Get skews of variables
  skewed <- psych::describe(df) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column('variable_name') %>% 
    dplyr::select(variable_name, skew) %>% 
    dplyr::filter(abs(skew) > 2) %>% 
    dplyr::pull(variable_name)
  
  plots <- purrr::map(names(df)[names(df) != 'state'], \(var){
    # color based on skewness
    if (var %in% skewed) {
      fill <- 'red'
      color <- 'darkred'
    } else {
      fill <- 'lightblue'
      color <- 'royalblue'
    }
    
    # Make plot for variable
    df %>% 
      ggplot2::ggplot(ggplot2::aes(x = !!ggplot2::sym(var))) + 
      ggplot2::geom_density(
        fill = fill,
        color = color,
        alpha = 0.5
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(rep(0.5, 4)), 'cm'))
  }) 
  
  # Arrange them in 4 columns
  ggpubr::ggarrange(
    plotlist = plots,
    ncol = columns,
    nrow = rows
  )
}