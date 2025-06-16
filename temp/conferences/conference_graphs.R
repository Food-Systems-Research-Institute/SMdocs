# Conference Graphs
# For NAREA and AFHVS


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  tibble, 
  dplyr,
  tidyr,
  ggplot2,
  ggpubr,
  purrr,
  stringr,
  readr,
  conflicted,
  ggraph,
  igraph,
  RColorBrewer,
  paletteer,
  snakecase,
  htmltools,
  reactable,
  downloadthis,
  fmsb,
  mapview,
  sf,
  leaflet,
  extrafont
)

loadfonts(device = 'win')

sm_data <- readRDS('data/sm_data.rds')
state_key <- readRDS('data/state_key.rds')
fips_key <- readRDS('data/fips_key.rds')
ne_states <- c('VT', 'NH', 'ME', 'MA', 'CT', 'RI') %>% sort

source('dev/get_dimension_ggraph.R')
source('dev/get_vt_spiders.R')
source('dev/get_reactable_scores.R')
# source('dev/filter_fips.R')
load_sm()

conflicts_prefer(
  dplyr::select(),
  dplyr::filter(),
  dplyr::arrange(),
  dplyr::summarize(),
  dplyr::as_data_frame(),
  .quiet = TRUE
)

plots <- list()



# Check Meta --------------------------------------------------------------


meta <- sm_data$metadata
check <- meta %>%
  filter(grepl('oty', meta$variable_name))
get_str(check)
check$metric %>% sort
check$resolution %>% unique %>% sort
check$updates %>% unique %>% sort
check$source %>% unique %>% sort



# Framework Charts --------------------------------------------------------


conf_tree <- sm_data[['conf_tree']]

# Clean up the framework df 
frame <- conf_tree %>% 
  select(dimension:variable_name, use) %>% 
  filter(
    use == 'x',
    str_detect(metric, 'single-parent', negate = TRUE)
  ) %>% 
  select(-use) %>% 
  mutate(
    metric = ifelse(
      str_length(metric) > 75,
      paste0(str_sub(metric, end = 75), '...'),
      metric
    )
  )
get_str(frame)

# Rename some things, add spaces to beginning of metrics for consistent gap
frame <- frame %>% 
  mutate(
    indicator = case_when(
       str_detect(indicator, 'failure') ~ 'food business failure rate',
      .default = indicator
    ),
    metric = paste0(' ', metric)
  )
get_str(frame)



## Economics ---------------------------------------------------------------


econ <- get_dimension_ggraph(
  framework_df = frame,
  dimension_in = 'Economics',
  include_metrics = TRUE,
  y_limits = c(-2, 3),
  palette = "scico::batlowW",
  # palette = "ggthemes::stata_s2color",
  leaf_font_size = 4.5,
  index_label_size = 0.2,
  index_font_size = 4.5,
  arrow = arrow(
    angle = 20,
    length = unit(0.1, 'inches'),
    ends = 'last',
    type = 'closed'
  )
)
econ
ggsave(
  'temp/conferences/narea_econ_graph.png',
  plot = econ,
  width = 14,
  height = 6,
  units = 'in',
  bg = 'white'
)

# Try with indicators and metrics only
econ_slim <- get_dimension_ggraph_slim(
  framework_df = frame,
  dimension_in = 'Economics',
  include_metrics = TRUE,
  y_limits = c(-2.5, 0.5),
  # palette = "ggsci::category10_d3",
  palette = "scico::batlowW",
  # palette = "ggthemes::Classic_10",
  # palette = "rcartocolor::Bold",
  # palette = "ggthemes::stata_s2color",
  leaf_font_size = 5,
  index_label_size = 0.2,
  index_font_size = 5,
  arrow = arrow(
    angle = 20,
    length = unit(0.1, 'inches'),
    ends = 'last',
    type = 'closed'
  )
)
econ_slim
# ggsave(
#   'temp/conferences/narea_econ_graph.png',
#   plot = econ_slim,
#   width = 12,
#   height = 6,
#   units = 'in',
#   bg = 'white'
# )



## Social ------------------------------------------------------------------


social <- get_dimension_ggraph(
  framework_df = frame,
  dimension_in = 'Social',
  include_metrics = TRUE,
  y_limits = c(-1.4, 3),
  palette = "scico::batlowW",
  # palette = "ggthemes::stata_s2color",
  leaf_font_size = 4.5,
  index_label_size = 0.2,
  index_font_size = 4.5,
  arrow = arrow(
    angle = 20,
    length = unit(0.1, 'inches'),
    ends = 'last',
    type = 'closed'
  )
)
social
ggsave(
  'temp/conferences/narea_social_graph.png',
  plot = social,
  width = 11,
  height = 8,
  units = 'in',
  bg = 'white'
)

# Try with indicators and metrics only
social_slim <- get_dimension_ggraph_slim(
  framework_df = frame,
  dimension_in = 'Social',
  include_metrics = TRUE,
  y_limits = c(-1.5, 0.5),
  # palette = 'black',
  # palette = "ggsci::default_igv",
  # palette = "ggthemes::Classic_10",
  palette = "scico::batlowW",
  # palette = "ggthemes::stata_s2color",
  leaf_font_size = 5,
  index_label_size = 0.2,
  index_font_size = 5,
  arrow = arrow(
    angle = 20,
    length = unit(0.1, 'inches'),
    ends = 'last',
    type = 'closed'
  )
)
social_slim
# ggsave(
#   'temp/conferences/narea_social_graph.png',
#   plot = social_slim,
#   width = 8,
#   height = 8,
#   units = 'in',
#   bg = 'white'
# )



# Spider Plot -------------------------------------------------------------


# Just doing one for Vermont I guess
scores <- readRDS('data/state_score_iterations.rds')

png(
  filename = 'temp/conferences/narea_spider.png',
  width = 6,
  height = 6,
  units = 'in',
  res = 300
)
get_single_spider(
  scores$raw_minmax_geometric$dimension_scores,
  'Dimension Scores for Vermont\nMin-Max Geometric'
) 
dev.off()


# Map ---------------------------------------------------------------------


# Get food insecurity, latest year, make map
sm_spatial <- readRDS('data/sm_spatial.rds') 
counties <- sm_spatial$ne_counties_2024

# Get food insecurity at county level
metrics <- sm_data$metrics
get_str(metrics)
metrics$variable_name %>% 
  unique %>% 
  str_subset('insecurity')
dat <- metrics %>% 
  filter(
    variable_name == 'food_insecurity_rate',
    str_length(fips) == 5
  ) %>% 
  # filter_fips(scope = 'counties') %>% 
  get_latest_year() %>% 
  mutate(value = as.numeric(value))
get_str(dat)    
  
# Join 
df <- counties %>% 
  left_join(dat)
get_str(df)

# Map
pal <- colorNumeric(
  palette = "YlGn",
  domain = df$value,
  reverse = FALSE
)
df %>% 
  leaflet() %>% 
  addProviderTiles(
    providers$Stadia.AlidadeSmooth, 
    group = 'Stadia AlidadeSmooth'
  ) %>%
  addPolygons(
    color = "black",
    weight = 1, 
    smoothFactor = 0.5,
    opacity = 1.0, 
    fillOpacity = 0.8,
    fillColor = ~pal(df$value)
  ) %>% 
  addLegend(
    "bottomright",
    pal = pal,
    values = ~df$value,
    title = 'Food Insecurity<br>Rate 2023',
    opacity = 1
  )


# TMAP --------------------------------------------------------------------

pacman::p_load(tmap, maptiles, tmaptools)
bbox <- st_bbox(counties)
tiles <- get_tiles(
  bbox,
  provider = "CartoDB.PositronNoLabels",
  zoom = 7,
  crop = TRUE
)

tmap_mode('plot')
map <- tm_shape(tiles) +
  tm_rgb() +
  tm_shape(df) +
  tm_polygons(
    "value", 
    palette = "brewer.greens",
    title = "Food Insecurity\nRate 2023",
    fill.legend = tm_legend(
      reverse = TRUE
    )
  ) +
  tm_layout(
    legend.position = c('right', 'bottom'),
    legend.title.fontface = 'bold',
    legend.width = 11,
    legend.height = 14,
    legend.title.size = 1.3,
    inner.margins = rep(0, 4),
    outer.margins = rep(0, 4),
    legend.text.size = 1.05
  )
map

tmap_save(
  tm = map,
  filename = 'temp/conferences/ne_map.png',
  asp = 0,
  dpi = 300
)




# Check Indicator count ---------------------------------------------------


pacman::p_load(
  readxl,
  dplyr
)

dat <- read_excel('temp/frontiers/frontiers_tree.xlsx', sheet = 1)
get_str(dat)

dat %>% 
  filter(share == 'y') %>% 
  # pull(indicator) %>% 
  unique() %>% 
  length()

tab <- dat %>% 
  filter(share == 'y') %>% 
  pull(quality) %>% 
  get_table()
tab

tab <- dat %>% filter(share == 'y') %>% pull(quality) %>% get_table()
tab <- get_table(pull(filter(dat, share == 'y'), quality))



# Time Series -------------------------------------------------------------


get_str(metrics)

# Get New England county data + Vermont
# somethign about conservartion
dat <- metrics %>% 
  filter(
    str_detect(fips, '^50'),
    str_detect(
      variable_name, 
      regex('^oty|naics|^yield', ignore_case = TRUE), 
      negate = TRUE
    )
  )
get_str(dat)

(vars <- unique(sort(dat$variable_name)))
str_subset(vars, 'Income')

dat <- dat %>% 
  filter(str_detect(
    variable_name, 
    'consTillNoTillAcres|CO2FromAg|FarmIncomePF|cost_per_meal|acresPF|socialAssociations'
  )) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(value))
get_str(dat)
dat %>% 
  group_by(variable_name) %>% 
  summarize(
    scale = paste(unique(fips), collapse = ', '),
    year = paste(unique(year), collapse = ', ')
  )

# Just do Chittenden county
# conservation till no till acres per farm - Vermont
# cost per meal? vermont
# CO2FromAg Vermont

theme_set(
  theme_bw(base_family = 'Georgia')
)
graphs <- list()

# Graph of cost per meal Chittenden County
graphs$cost_per_meal <- dat %>% 
  filter(variable_name == 'cost_per_meal', fips == '50007') %>% 
  ggplot(aes(x = year, y = value, group = 1)) + 
  geom_line(
    lwd = 1.5,
    color = '#154734'
  ) + 
  labs(
    x = 'Year',
    y = 'Cost per Meal ($)',
    title = 'Cost per Meal in Chittenden County'
  ) +
  scale_x_continuous(n.breaks = 10)
graphs$cost_per_meal

# C02 from Ag Vermont
graphs$co2 <- dat %>% 
  filter(variable_name == 'CO2FromAg', fips == '50') %>% 
  ggplot(aes(x = year, y = value, group = 1)) + 
  geom_line(
    lwd = 1.5,
    color = '#154734'
  ) + 
  labs(
    x = 'Year',
    y = 'CO2 from Ag (Tg)',
    title = 'CO2 from Agriculture in Vermont'
  ) +
  scale_x_continuous(n.breaks = 10) +
  ylim(0, 0.015)
graphs$co2

# Save them
ggsave(
  filename = 'temp/conferences/co2_from_ag.png',
  plot = graphs$co2,
  dpi = 300,
  height = 4,
  width = 6,
  scale = 0.75
)
ggsave(
  filename = 'temp/conferences/cost_per_meal.png',
  plot = graphs$cost_per_meal,
  dpi = 300,
  height = 4,
  width = 6,
  scale = 0.75
)
