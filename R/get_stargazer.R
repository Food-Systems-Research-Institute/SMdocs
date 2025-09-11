#' Create a formatted stargazer regression table with three models
#'
#' This function is a wrapper for stargazer that produces three regression models
#' with default settings: (1) dimensions only, (2) with GDP per capita, and 
#' (3) with GDP and population weights. It includes options for robust standard
#' errors and custom formatting.
#'
#' @param model A regression model object (e.g., from lm() or glm()) that serves
#'   as the base model.
#' @param dep_var Character string specifying the dependent variable label for
#'   the table.
#' @param robust Logical indicating whether to use robust standard errors (HC3).
#'   When TRUE, also includes robust F-statistics. Default is FALSE.
#' @param digits Numeric specifying the number of digits for rounding.
#'   Default is 3.
#' @param single_row Logical indicating whether to display coefficients and
#'   standard errors in a single row. Default is TRUE.
#' @param column_labels Character vector of length 3 specifying column labels
#'   for the three models. Default is c('Dimensions Only', 'With GDP per Capita',
#'   'GDP and Pop. Weights').
#' @param type Character string specifying the output format for stargazer.
#'   Default is 'latex'.
#' @param out Character string specifying the output file path. Default is NULL
#'   (no file output).
#' @param ... Additional arguments passed to stargazer().
#'
#' @return A stargazer table object. The output format depends on the \code{type}
#'   parameter.
#'
#' @details The function creates three models:
#'   \itemize{
#'     \item Model 1: The original model (dimensions only)
#'     \item Model 2: Original model + gdp_per_cap variable
#'     \item Model 3: Model 2 with population weights (WLS)
#'   }
#'   
#'   When robust = TRUE, the function calculates robust standard errors using
#'   HC3 heteroskedasticity-consistent estimators and includes robust F-statistics
#'   using Wald tests.
#'   
#'   The table includes additional rows indicating whether robust standard errors
#'   and weighted least squares (WLS) were used.
#'
#' @importFrom purrr map map2_chr
#' @importFrom stargazer stargazer
#' @importFrom lmtest waldtest
#' @importFrom sandwich vcovHC
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' model <- lm(outcome ~ dimension1 + dimension2, data = my_data)
#' get_stargazer(model, dep_var = "Outcome Variable")
#'
#' # With robust standard errors
#' get_stargazer(
#'   model,
#'   dep_var = "Outcome Variable",
#'   robust = TRUE,
#'   type = "html"
#' )
#'
#' # Custom column labels
#' get_stargazer(
#'   model,
#'   dep_var = "My Outcome",
#'   column_labels = c("Base Model", "With Controls", "Weighted")
#' )
#' }
#'
#' @export
get_stargazer <- function(model,
                          dep_var,
                          robust = FALSE,
                          digits = 3,
                          single_row = TRUE,
                          column_labels = c(
                            'Dimensions Only',
                            'With GDP per Capita',
                            'GDP and Pop. Weights'
                          ),
                          type = 'latex',
                          out = NULL,
                          ...) {
  
  # Make two more models based on first, put them in list to work over later
  model2 <- update(model, ~ . + gdp_per_cap)
  model3 <- update(model2, weights = population)
  models <- list(model, model2, model3)
  
  # Start a list of lines to add to bottom of regression.
  cond_add_lines <- list(c('WLS', 'No', 'No', 'Yes'))
  
  # If robust, get robust SEs and F test. 
  if (robust == TRUE) {
    vcovs <- purrr::map(models, ~ sandwich::vcovHC(.x, type = 'HC3'))
    cond_ses <- purrr::map(vcovs, ~ {
      sqrt(diag(.x))
    })
    cond_omit <- 'f'
    cond_f <- purrr::map2_chr(models, vcovs, ~ {
      f_test <- lmtest::waldtest(.x, vcov = .y)
      raw_f_stat <- f_test[['F']][2]
      p_value <- f_test[['Pr(>F)']][2]
      if (p_value < 0.001) {
        f_stat <- paste0(round(raw_f_stat, digits), '***')
      } else if (p_value < 0.01) {
        f_stat <- paste0(round(raw_f_stat, digits), '**')
      } else if (p_value < 0.05) {
        f_stat <- paste0(round(raw_f_stat, digits), '*')
      } else {
        f_stat <- round(raw_f_stat, digits)
      }
      out <- paste0(
        f_stat,
        ' (df = ', abs(f_test[['Df']][2]), 
        '; ', 
        round(f_test[['Res.Df']][1], digits), ')'
      )
      return(out)
    })
    
    # Append robust line and f stat line to bottom of table
    cond_add_lines[[2]] <- c('Robust', rep('Yes', 3))
    cond_add_lines[[3]] <- c('F Statistic', cond_f)
    
  } else {
    
    # If not robust, just do nothing, except say robust no no no
    cond_ses <- NULL
    cond_omit <- NULL
    cond_f <- NULL
    cond_add_lines[[2]] <- c('Robust', rep('No', 3))
    cond_add_lines[[3]] <- NULL
  }
  
  # Make table
  stargazer::stargazer(
    model, 
    model2, 
    model3,
    type = type, 
    out = out,
    digits = digits,
    column.labels = column_labels,
    column.separate = c(1, 1, 1),
    dep.var.labels = dep_var,
    intercept.bottom = FALSE,
    single.row = single_row,
    report = 'vc*s',
    ci = TRUE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    notes = c("<sup>&sstarf;</sup>p<0.05; <sup>&sstarf;&sstarf;</sup>p<0.01; <sup>&sstarf;&sstarf;&sstarf;</sup>p<0.001"),
    notes.append = FALSE,
    se = cond_ses,
    omit.stat = cond_omit,
    add.lines = cond_add_lines,
    ...
  )
}