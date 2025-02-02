# Organizing metrics framework data as csv for ds1 hw
pacman::p_load(
  dplyr,
  stringr,
  purrr,
  tidyr,
  readr
)

# Explore our set of state level metrics from refined secondary framework
sm_data <- readRDS('data/sm_data.rds')
raw_tree <- sm_data[['refined_tree']]
get_str(raw_tree)

# Clean up the framework df 
frame <- raw_tree %>% 
  select(dimension:variable_name, resolution, use) %>% 
  dplyr::filter(use == 'x') %>% 
  select(-use) %>% 
  mutate(
    metric = ifelse(
      str_length(metric) > 50,
      paste0(str_sub(metric, end = 50), '...'),
      metric
    )
  )
get_str(frame)


## Join with metadata to double check the resolution of our metrics
meta <- sm_data$metadata
get_str(meta)

dat <- frame %>% 
  select(variable_name) %>% 
  left_join(meta, by = 'variable_name') %>% 
  unique()
get_str(dat)

# check resolution
dat$resolution
str_detect(dat$resolution, 'state')
# Looks good, everything is at state level

# Pull it from the actual metrics data
metrics <- sm_data$metrics %>% 
  filter(
    variable_name %in% frame$variable_name,
    fips %in% sm_data$state_key$state_code
  )
get_str(metrics)

# Filter to last year only, pivot longer, add state names
source('dev/data_pipeline_functions.R')
state_key <- sm_data$state_key
get_str(state_key)
out <- metrics %>% 
  get_latest_year() %>% 
  mutate(value = as.numeric(value)) %>% 
  unique() %>% 
  pivot_wider(
    names_from = 'variable_name',
    values_from = 'value'
  ) %>% 
  left_join(
    select(state_key, state_code, state), 
    by = join_by(fips == state_code)
  ) %>% 
  select(state, everything()) %>% 
  select(-matches('waterIrrSrcOffFarmExp'), -fips)
get_str(out)  
  
# Save as csv
write_csv(out, 'temp/metrics_ds.csv')
