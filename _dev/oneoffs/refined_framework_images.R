# Refined Framework Images
# 2025-03-31


# Description -------------------------------------------------------------

# Making some more readable images of refined framework for metrics meeting


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  conflicted,
  purrr,
  stringr,
  ggraph,
  igraph,
  dplyr,
  readr,
  RColorBrewer,
  paletteer,
  snakecase,
  tibble, 
  tidyr,
  htmltools,
  reactable
)

source('dev/get_dimension_ggraph.R')
# frame <- readRDS('data/frame.rds')
plots <- list()

# Load refined framework
sm_data <- readRDS('data/sm_data.rds')
raw_frame <- sm_data[['refined_tree']]

# Clean up the framework df 
frame <- raw_frame %>% 
  select(dimension:variable_name, use) %>% 
  filter(use == 'x') %>% 
  select(-use) %>% 
  mutate(
    metric = ifelse(
      str_length(metric) > 75,
      paste0(str_sub(metric, end = 75), '...'),
      metric
    )
  )
get_str(frame)



# -------------------------------------------------------------------------


# Parameters
dimensions <- c(
  'environment',
  'economics',
  'health',
  'production',
  'social'
)

limits <- list(
  c(-5, 3.5),
  c(-3.5, 3),
  c(-3, 3),
  c(-2.5, 2.5),
  c(-1.7, 3)
)

fig_dims <- list(
  c(10, 10),
  c(10, 12),
  c(12, 11),
  c(9, 10),
  c(8, 10)
)

# Make plots
plots <- map2(dimensions, limits, ~ {
  get_dimension_ggraph(
    framework_df = frame,
    dimension_in = .x,
    y_limits = .y,
    include_metrics = TRUE,
    palette = 'ggthemes::stata_s2color'
  )
}) %>% 
  setNames(c(dimensions))
names(plots)


# Save PNGs
pwalk(list(plots, fig_dims, dimensions), \(plot, fig_dim, dimension) {
  png(
    filename = paste0('dev/oneoffs/', dimension, '.png'),
    height = fig_dim[1],
    width = fig_dim[2],
    units = 'in',
    res = 200
  ) 
  print(plot)
  dev.off()
})
