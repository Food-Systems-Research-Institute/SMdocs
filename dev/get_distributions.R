# Get Distributions
# 2025-08-05


# Description -------------------------------------------------------------


# Dependencies ------------------------------------------------------------


# Function ----------------------------------------------------------------

get_distributions <- function(df,
                              fill = '#154734',
                              color = 'black',
                              skew_cutoff = 2,
                              n_col,
                              n_row){
  
  skewed <- psych::describe(df[, -1]) %>% 
    as.data.frame() %>% 
    rownames_to_column('variable_name') %>% 
    dplyr::select(variable_name, skew) %>% 
    dplyr::filter(abs(skew) > skew_cutoff) %>% 
    pull(variable_name)
  
  plots <- map(names(df)[-1], \(var){
    # color based on skewness
    if (var %in% skewed) {
      fill <- 'red'
      color <- 'darkred'
    } else {
      fill <- '#154724'
      color <- 'black'
    }
    
    # Make plot for variable
    dat %>% 
      ggplot(aes(x = !!sym(var))) + 
      geom_density(
        fill = fill,
        color = color,
        alpha = 0.5
      ) +
      theme_classic() +
      theme(plot.margin = unit(c(rep(0.5, 4)), 'cm'))
  }) 
  
  # Arrange them in 4 columns
  ggarrange(
    plotlist = plots,
    ncol = n_col,
    nrow = n_row
  )
}
                              
