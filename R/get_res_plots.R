#' Create residual diagnostic plots for regression models
#'
#' This function generates the standard suite of residual diagnostic plots
#' for regression models in a 2x2 grid layout. It's a convenience wrapper
#' around the base R plot() method for model objects.
#'
#' @param model A fitted regression model object (e.g., from lm(), glm(), or
#'   other modeling functions that support the generic plot() method).
#'
#' @return NULL (invisible). The function creates plots as a side effect using
#'   base R graphics.
#'
#' @details The function displays four standard diagnostic plots in a 2x2 grid:
#'   \itemize{
#'     \item Residuals vs Fitted: Shows residual patterns and heteroscedasticity
#'     \item Normal Q-Q: Assesses normality of residuals
#'     \item Scale-Location: Shows spread of residuals (homoscedasticity)
#'     \item Residuals vs Leverage: Identifies influential observations
#'   }
#'
#'   After plotting, the graphics parameter is reset to single plot mode.
#'   This function is particularly useful for quickly assessing model
#'   assumptions and identifying potential issues with regression models.
#'
#' @examples
#' \dontrun{
#' # Fit a linear regression model
#' model <- lm(mpg ~ wt + hp + qsec, data = mtcars)
#'
#' # Create diagnostic plots
#' get_res_plots(model)
#'
#' # Works with other model types too
#' glm_model <- glm(am ~ mpg + hp, data = mtcars, family = binomial)
#' get_res_plots(glm_model)
#' }
#'
#' @export
get_res_plots <- function(model) {
  par(mfrow = c(2, 2))
  plot(model)
  par(mfrow = c(1, 1))
}
