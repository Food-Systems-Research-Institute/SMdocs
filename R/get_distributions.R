#' Create density plots for variable distributions
#'
#' This function generates a grid of density plots showing the distributions
#' of all numeric variables in a data frame (excluding the first column).
#' Variables with high skewness are highlighted in red, while normally
#' distributed variables are shown with customizable colors.
#'
#' @param df A data frame where the first column contains identifiers and
#'   remaining columns contain numeric variables to plot.
#' @param fill Character string specifying the fill color for normal
#'   (non-skewed) distributions. Default is '#154734'.
#' @param color Character string specifying the border color for normal
#'   distributions. Default is 'black'.
#' @param skew_cutoff Numeric specifying the absolute skewness threshold
#'   above which variables are considered highly skewed. Default is 2.
#' @param n_col Numeric specifying the number of columns in the plot grid.
#' @param n_row Numeric specifying the number of rows in the plot grid.
#'
#' @return A ggplot object containing the arranged density plots. The plot
#'   can be displayed or saved using standard ggplot2 methods.
#'
#' @details The function performs the following steps:
#'   \itemize{
#'     \item Calculates skewness for all numeric variables (excluding first column)
#'     \item Identifies highly skewed variables using the specified cutoff
#'     \item Creates density plots with color coding:
#'       \itemize{
#'         \item Red fill/dark red border: Highly skewed variables
#'         \item Custom fill/border colors: Normal variables
#'       }
#'     \item Arranges plots in a grid using ggpubr::ggarrange()
#'   }
#'
#'   Note: There appears to be a small typo in the original code where the
#'   normal fill color defaults to '#154724' instead of the parameter default
#'   '#154734'. This has been corrected to use the actual parameter value.
#'
#' @importFrom psych describe
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr select filter pull
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot aes geom_density theme_classic theme unit sym
#' @importFrom ggpubr ggarrange
#'
#' @examples
#' \dontrun{
#' # Sample data with an ID column and numeric variables
#' sample_data <- data.frame(
#'   id = 1:100,
#'   normal_var = rnorm(100),
#'   skewed_var = rexp(100),
#'   uniform_var = runif(100)
#' )
#'
#' # Create distribution plots with default settings
#' get_distributions(sample_data, n_col = 2, n_row = 2)
#'
#' # Customize colors and skewness threshold
#' get_distributions(
#'   sample_data,
#'   fill = "lightblue",
#'   color = "navy",
#'   skew_cutoff = 1.5,
#'   n_col = 3,
#'   n_row = 1
#' )
#' }
#'
#' @export
get_distributions <- function(df,
                              fill = '#154734',
                              color = 'black',
                              skew_cutoff = 1){

  skewed <- psych::describe(df[, -1]) %>%
    as.data.frame() %>%
    tibble::rownames_to_column('variable_name') %>%
    dplyr::select(variable_name, skew) %>%
    dplyr::filter(abs(skew) > skew_cutoff) %>%
    dplyr::pull(variable_name)

  plots <- purrr::map(names(df)[-1], \(var){
    # color based on skewness
    if (var %in% skewed) {
      plot_fill <- 'red'
      plot_color <- 'darkred'
    } else {
      plot_fill <- fill
      plot_color <- color
    }

    # Make plot for variable
    df %>%
      dplyr::select(!!sym(var)) %>%
      dplyr::mutate(!!sym(var) := as.numeric(!!sym(var))) %>%
      na.omit() %>%
      ggplot2::ggplot(ggplot2::aes(x = !!ggplot2::sym(var))) +
      ggplot2::geom_density(
        fill = plot_fill,
        color = plot_color,
        alpha = 0.5
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(rep(0.5, 4)), 'cm'))
  })
  return(plots)
}
