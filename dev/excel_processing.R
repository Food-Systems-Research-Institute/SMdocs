# Excel processing
# 2025-09-11

#' This script takes our 'revised_secondary_metrics.xlsx' from OneDrive and
#' combines it with the literature justifications to get one very large, some
#' might even say giant, table. It may be saved back to OneDrive for reference,
#' but it is not a final product. We will use this table to:
#'   1. Create the body table that will go in the body of the paper. This
#'    will include just the essential information for the reader: dimension,
#'    indicator, metric, definition, weighting, source, and shorthand citations.
#'    Subject to revision.
#'  2. Create the supplementary table with stuff for nerds: level of analysis,
#'    indicator type, state coverage, county coverage, first year, last year, year
#'    range, url, resolution, updates, probably others. I suppose it's just
#'    everything else that is of any use.

#' TODO: Add units to body table.
#' TODO: Add cell merging to body table
#' TODO: Make appendix table



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
  httr2,
  knitr,
  kableExtra
)

pacman::p_load_current_gh(
  'ChrisDonovan307/projecter',
  'Food-Systems-Research-Institute/SMdata'
)



# Pull Metrics Revised and Lit Justification Excel files -----------------
## Set Paths ---------------------------------------------------------------


# If else statement to get paths to work based on who is running the script
if (Sys.getenv('username') == 'isabellaloconte') {
  root_metric_path <- "/Users/isabellaloconte/OneDrive - University of Vermont/Susannah Walsh Daloz's files - Sustainability Metrics Manuscript/Metrics/"
  og_xl_path <- paste0(root_metric_path, "secondary_metrics_revised.xlsx")
  lit_path <- paste0(root_metric_path, "literature_justifying_indicators.xlsx")
} else if (Sys.getenv('username') == 'cdonov12') {
  root_metric_path <- "C:/Users/cdonov12/OneDrive - University of Vermont/Food Systems Research Center/Sustainability Metrics/Sustainability Metrics Manuscript/Metrics/"
  og_xl_path <- paste0(root_metric_path, "secondary_metrics_revised.xlsx")
  lit_path <- paste0(root_metric_path, "literature_justifying_indicators.xlsx")
}

# Relative path to where we will save a copy of the secondary metrics xl locally
new_xl_path <- 'data/data_paper/secondary_metrics.xlsx'



## Read Revised Metrics files (5 sheets of Metrics file) -------------------


# og_xl <- read_excel(og_xl_path)
sheet_names <- excel_sheets(og_xl_path)

combined_df <- lapply(sheet_names[1:5], function(sheet) {
  df <- read_excel(og_xl_path, sheet = sheet)
  df$quality <- as.character(df$quality)
  df$dimension <- sheet
  return(df)
}) %>%
  bind_rows()
str(combined_df)



## Existing Metrics --------------------------------------------------------


# Pull out existing SMdata metrics
existing_metrics <- SMdata::metrics %>%
  filter(variable_name %in% combined_df$variable_name) %>%
  SMdata::filter_fips('neast')
get_str(existing_metrics)



## Summary Stats -----------------------------------------------------------


# Get a summary table of existing metrics
sum_stats <- existing_metrics %>%
  group_by(variable_name) %>%
  summarize(
    n_states = length(unique(fips[nchar(fips) == 2])),
    n_counties = length(unique(fips[nchar(fips) == 5])),
    n_years = length(unique(sort(year))),
    first_year = min(unique(year)),
    latest_year = max(unique(year)),
    year_range = max(unique(as.numeric(year))) - min(unique(as.numeric(year)))
  )
get_str(sum_stats)

# Join to combined_df
new_table_with_sum <- combined_df %>%
  left_join(sum_stats, by = 'variable_name')
get_str(new_table_with_sum)

# Also join to SMdata metadata to get units for each variable
# TODO: fix this and add units
# out <- metadata %>%
#   select(variable_name, units) %>%
#   inner_join(new_table_with_sum, by = 'variable_name')



## Lit Justifications ------------------------------------------------------


lit_xl <- read_excel(lit_path)
get_str(lit_xl)

# Reformat shorthand citations to be separated by commas
head(lit_xl$`Shorthand Citations`)
lit_xl <- lit_xl %>%
  mutate(
    shorthand_citations = `Shorthand Citations` %>%
      str_replace_all('\\r\\n', ', '),
    .keep = 'unused'
  )
head(lit_xl$shorthand_citations)

# Check indicator counts in lit_xl and combined_df
lit_ind_count <- length(unique(lit_xl$indicator))
df_ind_count <- length(unique(new_table_with_sum$indicator))
if (lit_ind_count != df_ind_count) {
  warning(
    paste(
      "There should be the same number of indicators in the lit xl and the summary table",
      'The lit XL has', lit_ind_count, 'and the summary table has', df_ind_count
    )
  )
}

# Combine summary table with justification
giant_table <- new_table_with_sum %>%
  left_join(
    select(lit_xl, indicator, shorthand_citations),
    by = "indicator"
  ) %>%
  fill(indicator, index, shorthand_citations, .direction = "down") %>%
  select(quality, dimension, everything())
get_str(giant_table)



# Save Giant Table to OneDrive --------------------------------------------


# Note that this is not a final product, just a way to reference our giant table
giant_table_path <- paste0(
  root_metric_path,
  Sys.Date(),
  '_giant_table.xlsx'
)

openxlsx2::write_xlsx(
  giant_table,
  giant_table_path,
  widths = c(15, 'auto'),
  na.strings = 'NA'
)



# Save DP Objects ---------------------------------------------------------


# Saving some data paper (dp) datasets used in the project.

## dp_tree ----
# For making framework diagrams
# Includes NONE_# as placeholder when we are missing a metric
get_str(giant_table)
dp_tree <- giant_table %>%
  select(dimension, index, indicator, metric)
count <- sum(dp_tree$metric == 'NONE')
dp_tree$metric[dp_tree$metric == 'NONE'] <- paste0('NONE_', 1:count)
get_str(dp_tree)
usethis::use_data(dp_tree, overwrite = TRUE)

## dp_meta ----
# all metadata for just dp_metrics based on excel sheet in OneDrive
get_str(giant_table)
dp_meta <- giant_table
usethis::use_data(dp_meta, overwrite = TRUE)

## dp_metrics ----
# Only the metrics where we have state and county level that are used in dp
get_str(existing_metrics)
dp_metrics <- existing_metrics %>%
  mutate(across(c(year, value), as.numeric))
usethis::use_data(dp_metrics, overwrite = TRUE)

## dp_weight_vars ----
# Variable names and metric names of weighting variables from utils sheet
dp_weight_vars <- read_excel(
  og_xl_path,
  sheet = 'utilities'
) %>%
  filter(status != 'stall') %>%
  select(metric, variable_name)
get_str(dp_weight_vars)

# Save to data folder
usethis::use_data(dp_weight_vars, overwrite = TRUE)



# Body Table --------------------------------------------------------------


# Pulling from giant table, here we pull out relevant columns for the table that
# goes into the body of the paper and make a latex table. We are also putting in a placeholder column for our trend graphs.

# TODO: Work out cell merging in latex table for dimension, citations
trend_files <- dir('outputs/trend_plots/')
body_table <- giant_table %>%
  select(
    dimension,
    indicator,
    metric,
    definition,
    weighting,
    source,
    variable_name # use this to link to trend graph, then drop
    # shorthand_citations
  ) %>%
  # Where we have a metric, include an image of trend
  mutate(
    trend = case_when(
      (metric != 'NONE' & !is.na(variable_name) &
         str_detect(paste(trend_files, collapse = "|"), variable_name)) ~ paste0(
        '\\includegraphics[width=\\hsize,valign=c]{figures/trend_figures/fig_trend_',
        variable_name,
        '.png}'
      ),
      .default = NA_character_
    ),
    .after = definition,
  ) %>%
  mutate(
    # Manually escape (so that we don't excape in kbl())
    across(
      everything(),
      ~ .x %>%
        str_replace_all('%', ' percent') %>%
        str_replace_all('\\$', ' dollars')
    ),
    # Put a space between Tradition and Heritage
    indicator = case_when(
      str_detect(indicator, 'Tradition') ~ 'Tradition and Heritage',
      .default = indicator
    )
  ) %>%
  select(-variable_name) %>%
  # Format headers
  setNames(c(names(.) %>% snakecase::to_title_case()))

get_str(body_table)
body_table$Trend

# Get baseline table
body_latex <- body_table %>%
  kbl(
    format = 'latex',
    caption = 'Metric Attributes and Trends',
    label = 'tab_metrics',
    escape = FALSE
  ) %>%
  kable_styling(
    font_size = 10
  )
body_latex

# Remove existing table formatting
body <- body_latex %>%
  str_split_i("\\\\begin\\{tabular\\}\\[t\\]\\{[^}]+\\}\\n", 2) %>%
  str_replace("^\\\\begin\\{tabular\\}\\[t\\]\\{[^}]+\\}\\s*", "") %>%
  str_remove_all('\\\\hline\n') %>%
  str_remove('\\\\end\\{tabular\\}\n\\\\end\\{table\\}')
body %>%
  cat()

# Add our own header and footer
header <- '\\begin{landscape}
\\scriptsize
\\begin{longtblr}[caption = Metric Attributes]{
  % colspec={Q[50] Q[200] Q[200] Q[200]}, % Column widths,
  rowhead = 1,
  row{1} = {font=\\bfseries},
  colsep = 2pt, % Spaces between column lines and text/figure
  cells={halign=c,valign=m}, % Center horizontal and vertical
  column{1-2}={wd=2cm},
  column{3}={wd=3cm},
  column{4}={wd=4cm},
  column{5-6}={wd=2cm},
  column{7-Z}={wd=3cm},
  vlines,
  % hline{1,2,Z}={solid} % for only header and footer
  hlines, % all horizontal lines
}
\\label{tab:tab_metrics_body}
'

footer <- '\\end{longtblr}
\\end{landscape}'

# Put them all together
body_out <- paste0(header, body, footer)
cat(body_out)

# Save this to latex file
writeLines(body_out, 'outputs/tab_body_metrics.tex')


