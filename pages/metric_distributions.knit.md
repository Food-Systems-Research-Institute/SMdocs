---
title: "Metric Distributions"
format:
  html:
    fig-dpi: 200
editor_options: 
  chunk_output_type: inline
warnings: false
---







Explore metric distributions before normalization. Use this to inform how we might want to deal with outliers or normalize data at the metric level. For now, we are leaving the metrics as is, taking arithmetic means, and saving all normalization and weighting for the indicator level.







::: {.cell}

:::







Transforming our data from long format to wide and making sure everything came through alright.

# Distributions

Here we explore univariate distributions of each of our metrics. Highly skewed distributions might be good candidates for Box-Cox transformations or Winsorization. The figure below shows metrics with a skew \> 2 in red, while those with a skew \< 2 are in blue.







::: {.cell layout-align="center"}

```{.r .cell-code}
pacman::p_load(
  ggplot2,
  purrr,
  ggpubr,
  psych,
  tibble
)

# Load metrics_df (created in refined_framework page)
metrics_df <- readRDS('data/metrics_df.rds')

# Get skews of variables
skewed <- psych::describe(metrics_df[, -1]) %>% 
  as.data.frame() %>% 
  rownames_to_column('variable_name') %>% 
  dplyr::select(variable_name, skew) %>% 
  dplyr::filter(abs(skew) > 2) %>% 
  pull(variable_name)

plots <- map(names(metrics_df)[-1], \(var){
  # color based on skewness
  if (var %in% skewed) {
    fill <- 'red'
    color <- 'darkred'
  } else {
    fill <- 'lightblue'
    color <- 'royalblue'
  }
  
  # Make plot for variable
  metrics_df %>% 
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
  nrow = 33
)
```

::: {.cell-output-display}
![Distributions of metrics at the state level.](metric_distributions_files/figure-html/metric_distributions-1.png){fig-align='center' width=2000}
:::
:::







It seems most of our metrics fall along respectable somewhat-normal distributions. 32 metrics are skewed out of 129 total. They include several variables related to local farm economies (agrotourism sales as a percentage of total sales, direct to consumer sales as a percentage of total sales, and value added sales as a percentage of total sales), as well as a couple of the TreeMap 2016 variables (dead standing carbon and live trees) and GHG emissions from agriculture (CH4 and CO2, with an honorable mention for N2O). Just about the whole collection of ERS metrics are skewed, including importts and exports, indemnities, and real estate expenses. 

There might be a case for transformations here, or it might make more sense to do it at the indicator level. Another option is to weight our metrics to take into account population, number of farms, acres of farmland, GDP, or some other appropriate variable for each metric. @bene2019GlobalMapIndicators used Box Cox transformations for highly skewed indicators before normalizing all indicators with Min Max transformations. 

Putting a pin in this to consider trying it with raw metrics and transformed metrics and then to compare the two. 


