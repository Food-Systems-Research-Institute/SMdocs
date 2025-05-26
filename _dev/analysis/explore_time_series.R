metrics_df <- readRDS('data/metrics_df.rds')

dat <- readRDS('data/sm_data.rds')$metrics
get_str(dat)

# Get metrics that are in metrics df
dat <- dat %>% 
  filter(variable_name %in% names(metrics_df))
get_str(dat)

# For each variable name, see how many time points we have
vars <- unique(dat$variable_name)
walk(vars, ~ {
  years <- dat %>% 
    filter(variable_name == .x) %>% 
    pull(year) %>% 
    unique()
  range <- range(years)
  count <- length(years)
  cat(
    '\n\nVariable:', .x, 
    '\nRange: ', range, 
    '\nCount: ', count
  )
})
