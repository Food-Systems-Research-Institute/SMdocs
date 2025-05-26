pacman::p_load(
  dplyr,
  stringr
)

sm_data <- readRDS('data/sm_data.rds')
get_str(sm_data$fips_key)
get_str(sm_data$state_key)

# Convert state_key names to better ones
state_key <- sm_data$state_key %>% 
  select(
    fips = state_code,
    name = state_name
  )
county_key <- sm_data$fips_key %>% 
  select(
    fips,
    name = county_name
  ) %>% 
  filter(str_length(fips) > 2)

complete_key <- bind_rows(state_key, county_key) %>% 
  mutate(resolution = ifelse(str_length(fips) == 2, 'state', 'county'))
get_str(complete_key)

# Save it in shiny folder for use in test app
saveRDS(complete_key, 'shiny/complete_key.rds')
