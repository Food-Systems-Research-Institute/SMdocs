# Giant Tables
# 2025-09-14

# Make two giant latex tables from giant_table made in excel_processing.R


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  knitr,
  kableExtra
)

pacman::p_load_current_gh('ChrisDonovan307/projecter')
devtools::load_all()



# Body Table --------------------------------------------------------------


# TODO: Pull this out into a different script. The body table depends on graphs
# that we put together in dp_trends. We need to run it after that. Whereas
# this script needs to be run before any of the dp_* pages.

# Pulling from giant table, here we pull out relevant columns for the table that
# goes into the body of the paper and make a latex table. We are also putting in a placeholder column for our trend graphs.

# TODO: Work out cell merging in latex table for dimension, citations
trend_files <- dir('outputs/trend_plots/')
body_table <- giant_table %>%
  select(
    dimension,
    indicator,
    metric,
    units,
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
  column{1-4}={wd=2cm},
  column{5}={wd=4cm},
  column{6-7}={wd=2cm},
  column{8-Z}={wd=3cm},
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

