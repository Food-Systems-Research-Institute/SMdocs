# Exploring graph options 
# 2024-10-08



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  ggplot2,
  plotly,
  purrr,
  RColorBrewer
)

source('dev/data_pipeline_functions.R')
dat <- readRDS('data/aggregated_metrics.rds')
meta <- readRDS('data/aggregated_meta.rds')
fips_key <- readRDS('data/fips_key.rds')



# ts vars -----------------------------------------------------------------


# Which variables have good time series data
# Let's take everything with 5 or more years
get_str(dat)
ts_vars <- dat %>% 
  group_by(variable_name) %>% 
  summarize(n_years = length(unique(year))) %>% 
  filter(n_years >= 5) %>% 
  pull(variable_name)



# Plotly ------------------------------------------------------------------


# Try it with just first one
plot <- dat %>% 
  filter(variable_name == ts_vars[1]) %>% 
  mutate(
    across(c(year, value), as.numeric),
    state = str_sub(fips, end = 2)
  ) %>% 
  left_join(fips_key, by = 'fips') %>% 
  ggplot(aes(
    x = year, 
    y = value, 
    group = fips, 
    color = state_name,
    text = paste0(
      'State: ', state_name, '\n',
      'County: ', county_name, '\n',
      'CFIR: ', round(value, 3)
    )
  )) +
  geom_line(
    lwd = 1.25,
    alpha = 0.6
  ) +
  theme_bw() +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  labs(
    x = 'Year',
    y = 'Child Food Insecurity Rate',
    color = 'State',
    title = 'Child Food Insecurity Rate by County, 2011-2021'
  ) +
  scale_color_manual(values = brewer.pal(6, 'Dark2'))
plot
# Pretty neat

# Test plotly
test <- ggplotly(plot, tooltip = 'text')
test


# Many --------------------------------------------------------------------

ts_vars
get_str(dat)


dat %>% 
  filter(str_length(fips) == 2) %>% 
  get_str()
