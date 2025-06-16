# Narea
# 2025-06-10


# Description -------------------------------------------------------------

# Prep for NAREA on soils


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  purrr
)

sm_data <- readRDS('data/sm_data.rds')
metrics <- sm_data$metrics
meta <- sm_data$metadata



# Practices ---------------------------------------------------------------


# Pull vars for cover crop, no till, etc
vars <- meta %>% 
  filter(str_detect(metric, regex('tillage|cover crop', ignore_case = TRUE))) %>% 
  select(metric, variable_name)
vars

# Pull those, only New England
dat <- metrics %>% 
  filter(
    variable_name %in% vars$variable_name,
    str_length(fips) == '5'
  ) %>% 
  mutate(across(c(year, value), ~ as.numeric(.x)))
get_str(dat)

# See how total acres change
notill_ac_vars <- str_subset(vars$variable_name, 'Acres$')
notill <- map(notill_ac_vars, ~ {
  dat %>% 
    filter(variable_name == .x) %>% 
    group_by(year) %>% 
    summarize(total_acres = sum(value, na.rm = TRUE))
}) %>% 
  setNames(c(notill_ac))
notill

# Get % change
map(notill, ~ {
  diff <- .x$total_acres[2] - .x$total_acres[1]
  (diff / .x$total_acres[1]) * 100
})
# 36% increase in conservation tillage
# 15% in no till specifically
# 4% drop in cover crop
