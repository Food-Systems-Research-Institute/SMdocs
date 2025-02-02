pacman::p_load(
  purrr,
  psych,
  dplyr,
  tibble
)

get_indicator_distributions <- function(scores_list,
                                        transformation) {
  # Pull the indicators at desired transformation
  # Also remove averages, new england
  df <- scores_list[[transformation]]$indicator_scores %>% 
    filter(!state %in% c('US_mean', 'US_median', 'NewEng'))
  
  # Get skews of variables
  skewed <- psych::describe(df) %>% 
    as.data.frame() %>% 
    rownames_to_column('variable_name') %>% 
    select(variable_name, skew) %>% 
    filter(abs(skew) > 2) %>% 
    pull(variable_name)
  
  plots <- map(names(df)[names(df) != 'state'], \(var){
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
    ncol = 4,
    nrow = 8
  )
}