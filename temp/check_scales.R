# Check Scales
# 2025-05-09


# Description -------------------------------------------------------------

# Figuring out which of our metrics are at county or state levels to inform
# the Frontiers submission


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  stringr
)

# Load metrics
sm_data <- readRDS('data/sm_data.rds')
metrics <- sm_data$metrics
meta <- sm_data$metadata

# Load frame
frame <- readRDS('data/frameworks/new_frame.rds')

# Functions
source('dev/data_pipeline_functions.R')



# Check -------------------------------------------------------------------


get_str(metrics)
get_str(meta)
get_str(frame)

# Variable names
vars <- frame %>% 
  filter(variable_name != 'NONE') %>% 
  pull(variable_name)
vars
# 124



## By Meta -----------------------------------------------------------------


# Reduce meta to those variables
var_meta <- meta %>% 
  filter(variable_name %in% vars) %>% 
  select(variable_name, resolution)
get_str(var_meta)

# Get props
get_table(var_meta$resolution) %>% 
  prop.table()
# 40% at state level only
# but these are probably not updated lately




# By Metrics --------------------------------------------------------------


# Check with the actual metrics
get_str(metrics)

# Filter to vars, remove state fips codes
var_metrics <- metrics %>% 
  filter(
    variable_name %in% vars
    # str_length(fips) == 5
  )
get_str(var_metrics)
length(unique(var_metrics$variable_name))
# 73 left

# Check how many are left
out <- var_metrics %>% 
  filter(str_length(fips) == 5) %>% 
  pull(variable_name) %>% 
  unique() %>% 
  length()
out / length(unique(var_metrics$variable_name))
# 58% are at the county level. Better


## What do we ONLY have at the state level?
county_metrics <- var_metrics %>% 
  filter(str_length(fips) == 5) %>% 
  pull(variable_name) %>%
  unique()
state_metrics <- var_metrics %>% 
  filter(str_length(fips) == 2) %>% 
  pull(variable_name) %>%
  unique()
(state_only <- setdiff(state_metrics, county_metrics))
length(state_only)
# 51

# Back to frame for context
state_only_frame <- frame %>% 
  filter(variable_name %in% state_only)
state_only_frame

# Save it to csv to explore
write.csv(state_only_frame, 'temp/state_only_metrics.csv')
