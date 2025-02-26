# Get Stargazer
# 2025-02-26


# Description -------------------------------------------------------------

# Wrapper for stargazer that produces three regressions with default settings.


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stargazer,
  lmtest,
  sandwich
)



# Function ----------------------------------------------------------------


get_stargazer <- function(model,
                          dep_var,
                          robust = FALSE,
                          digits = 3,
                          column_labels = c(
                            'Dimensions Only',
                            'With GDP per Capita',
                            'GDP and Pop. Weights'
                          ),
                          ...) {
  
  # Make two more models based on first, put them in list to work over later
  model2 <- update(model, ~ . + gdp_per_cap)
  model3 <- update(model2, weights = population)
  models <- list(model, model2, model3)
  
  # Start a list of lines to add to bottom of regression.
  cond_add_lines <- list(c('WLS', 'No', 'No', 'Yes'))
  
  # If robust, get robust SEs and F test. 
  if (robust == TRUE) {
    vcovs <- map(models, ~ vcovHC(.x, type = 'HC3'))
    cond_ses <- map(vcovs, ~ {
      sqrt(diag(.x))
    })
    cond_omit <- 'f'
    cond_f <- map2_chr(models, vcovs, ~ {
      f_test <- waldtest(.x, vcov = .y)
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
  stargazer(
    model, 
    model2, 
    model3,
    type = 'html', 
    digits = digits,
    column.labels = column_labels,
    column.separate = c(1, 1, 1),
    dep.var.labels = dep_var,
    intercept.bottom = FALSE,
    single.row = TRUE,
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