# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  stringr,
  readxl,
  openxlsx2,
  tidyr,
  readr,
  remotes,
  writexl,
  httr2
)

# Get fresh data
remotes::install_github('Food-Systems-Research-Institute/SMdata', force = TRUE)
library(SMdata)


# Pull Metrics Revised and Lit Justification Excel files -----------------


path <- "/Users/isabellaloconte/OneDrive - University of Vermont/Susannah Walsh Daloz's files - Sustainability Metrics Manuscript/Metrics/secondary_metrics_revised.xlsx"

path2 <- "/Users/isabellaloconte/OneDrive - University of Vermont/Susannah Walsh Daloz's files - Sustainability Metrics Manuscript/Metrics/literature_justifying_indicators.xlsx"

new_xl <- '2_clean/secondary_metrics.xlsx'


# Read Excel files (5 sheets of Metrics file) ----------------------------


df1 <- read_excel(path)
df2 <- read_excel(path2)

sheet_names <- excel_sheets(path)

sheets_list <- lapply(sheet_names[1:5], function(sheet) {
  df <- read_excel(path, sheet = sheet)
  df$quality <- as.character(df$quality)
  df
})

combined_df <- bind_rows(sheets_list)


# Join Excel files --------------------------------------------------------


metrics_and_justification <- combined_df %>%
  left_join(df2 %>%
              select(indicator,
                     dimension = dimension,
                     "Shorthand Citations" = "Shorthand Citations"),
            by = "indicator") %>%
  fill(index, indicator, `Shorthand Citations`, dimension, .direction = "down") %>%
  select(quality, dimension, index, everything())


View(metrics_and_justification)


# Making mega table (filtering out things) --------------------------------


mega_table <- metrics_and_justification %>%
  select(-index, -`indicator type (Schreeful et al 2024)`, -quality, -keep, -analyze, -url, -notes, -status, -updates, -resolution, -desirable)

View(mega_table)


# Joining with current metric sheet BY CODE NOT SHEETS ---------------------


# Create folder for cleaned data
dir.create("2_clean", recursive = TRUE, showWarnings = FALSE)

file.copy(path, new_xl, overwrite = TRUE)

# Pull the working excel from OneDrive to yoink variable names and figure out
# what we need to do
sheets <- excel_sheets(new_xl)[1:5]
tab <- map(sheets, ~ {
  read_excel('2_clean/secondary_metrics.xlsx', sheet = .x) %>% 
    mutate(
      dimension = str_to_lower(.x), .before = 'index',
      quality = as.character(quality)
    ) %>% 
    fill(c(index, indicator), .direction = 'down')
}) %>% 
  bind_rows()
get_str(tab)

# Also pull weighting variables from utilities sheet
# add this to 5_objects so it gets lumped with sm_data and exported as rda
util_sheet <- read_excel('2_clean/secondary_metrics.xlsx', sheet = 'utilities')
utils <- util_sheet %>% 
  filter(status != 'stall') %>% 
  select(metric, variable_name)

# Create a folder again
dir.create("5_objects", showWarnings = FALSE)

saveRDS(utils, '5_objects/weighting_vars.rds')

# Pull them from our metrics
existing_metrics <- metrics %>% 
  filter(variable_name %in% tab$variable_name) %>% 
  filter_fips('neast')
get_str(existing_metrics)

# Do existing first
existing_metrics_sum <- existing_metrics %>% 
  group_by(variable_name) %>% 
  summarize(
    n_states = length(unique(fips[nchar(fips) == 2])),
    n_counties = length(unique(fips[nchar(fips) == 5])),
    n_years = length(unique(sort(year))),
    first_year = min(unique(year)),
    latest_year = max(unique(year)),
    year_range = max(unique(as.numeric(year))) - min(unique(as.numeric(year)))
  )
get_str(existing_metrics_sum)

# Joining mega table with stats
new_table_with_sum <- mega_table %>%
  left_join(existing_metrics_sum %>%
              select(variable_name,
                     n_states = n_states,
                     n_counties = n_counties,
                     n_years = n_years,
                     first_year = first_year,
                     latest_year = latest_year,
                     year_range = year_range),
            by = "variable_name") %>%
  select(-variable_name)
                     
View(new_table_with_sum)


# Save table with state, county, year stats to OneDrive -------------------


new_path <- paste0(
  "/Users/isabellaloconte/OneDrive - University of Vermont/Susannah Walsh Daloz's files - Sustainability Metrics Manuscript/Metrics/",
  Sys.Date(),
  '_table_for_paper.xlsx'
)

openxlsx2::write_xlsx(
  new_table_with_sum, 
  new_path,
  widths = c(15, 'auto'),
  na.strings = 'NA'
)