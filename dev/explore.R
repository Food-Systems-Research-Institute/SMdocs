pacman::p_load(
  dplyr,
  purrr,
  stringr
)

dat <- readRDS('data/sm_data.rds')
get_str(dat)
names(dat)

metrics <- dat$metrics
meta <- dat$meta

# Check for ghg data
check <- meta %>% 
  filter(str_detect(variable_name, '^mmt'))
check
